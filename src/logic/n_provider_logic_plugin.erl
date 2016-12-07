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

-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create/4, get_entity/1, get_internal/4, get_external/2,
    update/3, delete/1]).
-export([exists/2, authorize/5, validate/2]).


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
            throw({error, ?EL_BAD_DATA(<<"csr">>)})
    end,
    Provider = #od_provider{name = Name, urls = URLs,
        redirection_point = RedirectionPoint, serial = Serial,
        latitude = Latitude, longitude = Longitude},
    od_provider:create(#document{key = ProviderId, value = Provider}),
    {ok, {ProviderId, ProviderCertPem}};

create(_, ProviderId, spaces, Data) ->
    SupportSize = maps:get(<<"size">>, Data),
    Macaroon = maps:get(<<"token">>, Data),
    {ok, {space, SpaceId}} = token_logic:consume(Macaroon),
    case entity_graph:add_relation(
        od_space, SpaceId,
        od_provider, ProviderId,
        SupportSize
    ) of
        ok ->
            {ok, SpaceId};
        Error ->
            Error
    end;
create(_, undefined, check_my_ports, Data) ->
    test_connection(Data).


get_entity(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            {ok, Provider};
        _ ->
            ?EL_NOT_FOUND
    end.


get_internal(_, _ProviderId, #od_provider{spaces = Spaces}, spaces) ->
    {ok, Spaces};
get_internal(_, _ProviderId, #od_provider{}, {space, SpaceId}) ->
    n_space_logic_plugin:get_entity(SpaceId);
get_internal(_, _ProviderId, #od_provider{eff_users = EffUsers}, eff_users) ->
    {ok, maps:keys(EffUsers)};
get_internal(_, _ProviderId, #od_provider{}, {eff_user, UserId}) ->
    n_user_logic_plugin:get_entity(UserId);
get_internal(_, _ProviderId, #od_provider{eff_groups = EffGroups}, eff_groups) ->
    {ok, maps:keys(EffGroups)};
get_internal(_, _ProviderId, #od_provider{}, {eff_group, GroupId}) ->
    n_group_logic_plugin:get_entity(GroupId).


get_external(_, check_my_ip) ->
    % Peer is resolved and returned during response transformation.
    {ok, peer}.


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
    ok.


delete(ProviderId) when is_binary(ProviderId) ->
    ok = od_provider:delete(ProviderId).


exists(undefined, entity) ->
    true;
exists(_, check_my_ports) ->
    true;
exists(_, check_my_ip) ->
    true;
exists(ProviderId, {space, SpaceId}) when is_binary(ProviderId) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{spaces = Spaces}) ->
        lists:member(SpaceId, Spaces)
    end};
exists(ProviderId, {eff_user, UserId}) when is_binary(ProviderId) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{eff_users = EffUsers}) ->
        lists:member(UserId, EffUsers)
    end};
exists(ProviderId, {eff_group, GroupId}) when is_binary(ProviderId) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{eff_groups = EffGroups}) ->
        lists:member(GroupId, EffGroups)
    end};
exists(ProviderId, _) when is_binary(ProviderId) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{}) ->
        % If the provider with ProviderId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, check_my_ports, _, _) ->
    true;
authorize(create, undefined, entity, ?NOBODY, _) ->
    true;
authorize(create, ProvId, spaces, ?PROVIDER(ProvId), _) ->
    true;

authorize(get, undefined, check_my_ip, _, _) ->
    true;
authorize(get, _ProvId, entity, ?PROVIDER, _) ->
    true;
authorize(get, _ProvId, entity, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_providers);
authorize(get, ProvId, spaces, ?PROVIDER(ProvId), _) ->
    true;
authorize(get, _ProvId, spaces, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_spaces_of_provider);
authorize(get, ProvId, {space, _}, ?PROVIDER(ProvId), _) ->
    true;
authorize(get, _ProvId, {space, _}, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_spaces_of_provider);
authorize(get, _ProvId, eff_users, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_users_of_provider);
authorize(get, _ProvId, {eff_user, _}, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_users_of_provider);
authorize(get, _ProvId, eff_groups, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_groups_of_provider);
authorize(get, _ProvId, {eff_group, _}, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_groups_of_provider).


validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, any},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"csr">> => {binary, non_empty}
    },
    optional => #{
        <<"latitude">> => {float, fun(F) -> F >= -90 andalso F =< 90 end},
        <<"longitude">> => {float, fun(F) -> F >= -180 andalso F =< 180 end}
    }
};
validate(create, spaces) -> #{
    required => #{
        <<"token">> => {token, space_support_token},
        <<"size">> => {positive_integer, fun(I) ->
            {ok, Limit} = application:get_env(oz_worker, minimum_space_support_size),
            I >= Limit
        end}
    }
};
validate(create, check_my_ports) -> #{
};
validate(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, any},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"latitude">> => {float, fun(F) -> F >= -90 andalso F =< 90 end},
        <<"longitude">> => {float, fun(F) -> F >= -180 andalso F =< 180 end}
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
test_connection(_, _) ->
    throw({error, ?EL_BAD_DATA}).
