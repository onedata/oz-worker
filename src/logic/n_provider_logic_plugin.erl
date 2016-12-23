%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_provider_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("errors.hrl").
-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-export([entity_type/0, create/4, get_entity/1, get_internal/4, get_external/2,
    update/3, delete/2]).
-export([exists/2, authorize/4, validate/2]).


entity_type() ->
    od_provider.


create(_, _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    URLs = maps:get(<<"urls">>, Data),
    RedirectionPoint = maps:get(<<"redirectionPoint">>, Data),
    CSR = maps:get(<<"csr">>, Data),
    Latitude = maps:get(<<"latitude">>, Data, undefined),
    Longitude = maps:get(<<"longitude">>, Data, undefined),

    ProviderId = datastore_utils:gen_uuid(),
    {ok, {ProviderCertPem, Serial}} = try
        {ok, {_, _}} = worker_proxy:call(
            ozpca_worker, {sign_provider_req, ProviderId, CSR}
        )
    catch
        _:_ ->
            throw(?ERROR_BAD_DATA(<<"csr">>))
    end,
    Provider = #od_provider{name = Name, urls = URLs,
        redirection_point = RedirectionPoint, serial = Serial,
        latitude = Latitude, longitude = Longitude},
    od_provider:create(#document{key = ProviderId, value = Provider}),
    {ok, {ProviderId, ProviderCertPem}};

create(_, _, entity_dev, Data) ->
    Name = maps:get(<<"name">>, Data),
    URLs = maps:get(<<"urls">>, Data),
    RedirectionPoint = maps:get(<<"redirectionPoint">>, Data),
    CSR = maps:get(<<"csr">>, Data),
    Latitude = maps:get(<<"latitude">>, Data, undefined),
    Longitude = maps:get(<<"longitude">>, Data, undefined),
    UUID = maps:get(<<"uuid">>, Data, undefined),

    ProviderId = UUID,
    {ok, {ProviderCertPem, Serial}} = try
        {ok, {_, _}} = worker_proxy:call(
            ozpca_worker, {sign_provider_req, ProviderId, CSR}
        )
    catch
        _:_ ->
            throw(?ERROR_BAD_DATA(<<"csr">>))
    end,
    Provider = #od_provider{name = Name, urls = URLs,
        redirection_point = RedirectionPoint, serial = Serial,
        latitude = Latitude, longitude = Longitude},
    od_provider:create(#document{key = ProviderId, value = Provider}),
    {ok, {ProviderId, ProviderCertPem}};

create(?PROVIDER(ProviderId), ProviderId, support, Data) ->
    SupportSize = maps:get(<<"size">>, Data),
    Macaroon = maps:get(<<"token">>, Data),
    {ok, {od_space, SpaceId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_space, SpaceId, od_provider, ProviderId, SupportSize
    ),
    {ok, SpaceId};

create(_, undefined, check_my_ports, Data) ->
    test_connection(Data).


get_entity(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            {ok, Provider};
        _ ->
            ?ERROR_NOT_FOUND
    end.


get_internal(_, _ProviderId, #od_provider{spaces = Spaces}, spaces) ->
    {ok, Spaces};
get_internal(_, _ProviderId, #od_provider{}, {space, SpaceId}) ->
    ?assert_success(n_space_logic_plugin:get_entity(SpaceId));
get_internal(_, _ProviderId, #od_provider{eff_users = EffUsers}, eff_users) ->
    {ok, maps:keys(EffUsers)};
get_internal(_, _ProviderId, #od_provider{}, {eff_user, UserId}) ->
    ?assert_success(n_user_logic_plugin:get_entity(UserId));
get_internal(_, _ProviderId, #od_provider{eff_groups = EffGroups}, eff_groups) ->
    {ok, maps:keys(EffGroups)};
get_internal(_, _ProviderId, #od_provider{}, {eff_group, GroupId}) ->
    ?assert_success(n_group_logic_plugin:get_entity(GroupId)).


get_external(_, check_my_ip) ->
    % Peer is resolved and returned during response transformation.
    {ok, peer};
get_external(_, list) ->
    {ok, ProviderDocs} = od_provider:list(),
    {ok, [ProviderId || #document{key = ProviderId} <- ProviderDocs]}.


update(ProviderId, entity, Data) when is_binary(ProviderId) ->
    {ok, _} = od_provider:update(ProviderId, fun(Provider) ->
        #od_provider{
            name = Name, urls = URLs, redirection_point = RedPoint,
            latitude = Latitude, longitude = Longitude
        } = Provider,
        {ok, Provider#od_provider{
            name = maps:get(<<"name">>, Data, Name),
            urls = maps:get(<<"urls">>, Data, URLs),
            redirection_point = maps:get(<<"redirectionPoint">>, Data, RedPoint),
            latitude = maps:get(<<"latitude">>, Data, Latitude),
            longitude = maps:get(<<"longitude">>, Data, Longitude)
        }}
    end),
    ok;

update(ProviderId, {space, SpaceId}, Data) when is_binary(ProviderId) ->
    NewSupportSize = maps:get(<<"size">>, Data),
    entity_graph:update_relation(
        od_space, SpaceId, od_provider, ProviderId, NewSupportSize
    ).


delete(ProviderId, entity) when is_binary(ProviderId) ->
    entity_graph:delete_with_relations(od_provider, ProviderId);

delete(ProviderId, {space, SpaceId}) when is_binary(ProviderId) ->
    entity_graph:remove_relation(
        od_space, SpaceId, od_provider, ProviderId
    ).


exists(undefined, _) ->
    true;
exists(ProviderId, {space, SpaceId}) when is_binary(ProviderId) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{spaces = Spaces}) ->
        lists:member(SpaceId, Spaces)
    end};
exists(ProviderId, {eff_user, UserId}) when is_binary(ProviderId) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{eff_users = EffUsers}) ->
        maps:is_key(UserId, EffUsers)
    end};
exists(ProviderId, {eff_group, GroupId}) when is_binary(ProviderId) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{eff_groups = EffGroups}) ->
        maps:is_key(GroupId, EffGroups)
    end};
exists(ProviderId, _) when is_binary(ProviderId) ->
    {internal, fun(#od_provider{}) ->
        % If the provider with ProviderId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, check_my_ports, _) ->
    true;
authorize(create, undefined, entity, _) ->
    true;
authorize(create, undefined, entity_dev, _) ->
    true;
authorize(create, ProvId, support, ?PROVIDER(ProvId)) ->
    true;

authorize(get, undefined, check_my_ip, _) ->
    true;
authorize(get, undefined, list, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST);
authorize(get, _ProvId, entity, ?PROVIDER) ->
    % Any provider can get info about other providers
    true;
authorize(get, _ProvId, entity, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST);
authorize(get, ProvId, spaces, ?PROVIDER(ProvId)) ->
    true;
authorize(get, _ProvId, spaces, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_SPACES);
authorize(get, ProvId, {space, _}, ?PROVIDER(ProvId)) ->
    true;
authorize(get, _ProvId, {space, _}, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_SPACES);
authorize(get, _ProvId, eff_users, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_USERS);
authorize(get, _ProvId, {eff_user, _}, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_USERS);
authorize(get, _ProvId, eff_groups, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_GROUPS);
authorize(get, _ProvId, {eff_group, _}, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_GROUPS);

authorize(update, ProvId, entity, ?PROVIDER(ProvId)) ->
    true;
authorize(update, ProvId, {space, _}, ?PROVIDER(ProvId)) ->
    true;

authorize(delete, ProvId, entity, ?PROVIDER(ProvId)) ->
    true;
authorize(delete, _ProvId, entity, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_DELETE);
authorize(delete, ProvId, {space, _}, ?PROVIDER(ProvId)) ->
    true.


validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"csr">> => {binary, non_empty}
    },
    optional => #{
        <<"latitude">> => {float, {between, -90, 90}},
        <<"longitude">> => {float, {between, -180, 180}}
    }
};
validate(create, entity_dev) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"csr">> => {binary, non_empty},
        <<"uuid">> => {binary, non_empty}
    },
    optional => #{
        <<"latitude">> => {float, {between, -90, 90}},
        <<"longitude">> => {float, {between, -180, 180}}
    }
};
validate(create, support) -> #{
    required => #{
        <<"token">> => {token, ?SPACE_SUPPORT_TOKEN},
        <<"size">> => {integer, {not_lower_than, get_min_support_size()}}
    }
};
validate(create, check_my_ports) -> #{
};
validate(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"latitude">> => {float, {between, -90, 90}},
        <<"longitude">> => {float, {between, -180, 180}}
    }
}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tests connection to given urls.
%% @end
%% @equiv test_connection/2
%%--------------------------------------------------------------------
-spec test_connection(#{ServiceName :: binary() => Url :: binary()}) ->
    {ok, #{ServiceName :: binary() => Status :: ok | error}}.
test_connection(Map) ->
    test_connection(maps:to_list(Map), []).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tests connection to given urls.
%% @end
%%--------------------------------------------------------------------
-spec test_connection([{ServiceName :: binary(), Url :: binary()}], Result) ->
    {ok, Result}
    when Result :: #{ServiceName :: binary() => Status :: ok | error}.
test_connection([], Acc) ->
    {ok, Acc};
test_connection([{<<"undefined">>, <<Url/binary>>} | Rest], Acc) ->
    ConnStatus = case http_client:get(Url, [], <<>>, [insecure]) of
        {ok, 200, _, _} ->
            ok;
        _ ->
            error
    end,
    test_connection(Rest, Acc#{Url => ConnStatus});
test_connection([{<<ServiceName/binary>>, <<Url/binary>>} | Rest], Acc) ->
    ConnStatus = case http_client:get(Url, [], <<>>, [insecure]) of
        {ok, 200, _, ServiceName} ->
            ok;
        Error ->
            ?debug("Checking connection to ~p failed with error: ~n~p",
                [Url, Error]),
            error
    end,
    test_connection(Rest, Acc#{Url => ConnStatus});
test_connection([{Key, _} | _], _) ->
    throw(?ERROR_BAD_DATA(Key)).


get_min_support_size() ->
    {ok, MinSupportSize} = application:get_env(
        oz_worker, minimum_space_support_size
    ),
    MinSupportSize.