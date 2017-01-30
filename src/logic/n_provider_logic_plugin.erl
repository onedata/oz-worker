%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_provider model.
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

-type resource() :: entity | entity_dev | list | support |
eff_users | {eff_user, od_user:id()} |
eff_groups | {eff_group, od_group:id()} |
spaces | {space, od_space:id()} |
check_my_ports | {check_my_ip, cowboy_req:req()}.

-export_type([resource/0]).

-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/1, authorize/4, validate/2]).
-export([entity_to_string/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec get_entity(EntityId :: n_entity_logic:entity_id()) ->
    {ok, n_entity_logic:entity()} | {error, Reason :: term()}.
get_entity(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            {ok, Provider};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(),
    EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    n_entity_logic:data()) -> n_entity_logic:result().
create(_, _, entity, Data) ->
    Name = case maps:get(<<"name">>, Data, undefined) of
        undefined ->
            maps:get(<<"clientName">>, Data);
        N ->
            N
    end,
    URLs = maps:get(<<"urls">>, Data),
    RedirectionPoint = maps:get(<<"redirectionPoint">>, Data),
    CSR = maps:get(<<"csr">>, Data),
    Latitude = maps:get(<<"latitude">>, Data, 0.0),
    Longitude = maps:get(<<"longitude">>, Data, 0.0),

    ProviderId = datastore_utils:gen_uuid(),
    case worker_proxy:call(ozpca_worker, {sign_provider_req, ProviderId, CSR}) of
        {error, bad_csr} ->
            ?ERROR_BAD_DATA(<<"csr">>);
        {ok, {ProviderCertPem, Serial}} ->
            Provider = #od_provider{name = Name, urls = URLs,
                redirection_point = RedirectionPoint, serial = Serial,
                latitude = Latitude, longitude = Longitude},
            od_provider:create(#document{key = ProviderId, value = Provider}),
            {ok, {ProviderId, ProviderCertPem}}
    end;

create(_, _, entity_dev, Data) ->
    Name = case maps:get(<<"name">>, Data, undefined) of
        undefined ->
            maps:get(<<"clientName">>, Data);
        N ->
            N
    end,
    URLs = maps:get(<<"urls">>, Data),
    RedirectionPoint = maps:get(<<"redirectionPoint">>, Data),
    CSR = maps:get(<<"csr">>, Data),
    Latitude = maps:get(<<"latitude">>, Data, undefined),
    Longitude = maps:get(<<"longitude">>, Data, undefined),
    UUID = maps:get(<<"uuid">>, Data, undefined),

    ProviderId = UUID,
    case worker_proxy:call(ozpca_worker, {sign_provider_req, ProviderId, CSR}) of
        {error, bad_csr} ->
            ?ERROR_BAD_DATA(<<"csr">>);
        {ok, {ProviderCertPem, Serial}} ->
            Provider = #od_provider{name = Name, urls = URLs,
                redirection_point = RedirectionPoint, serial = Serial,
                latitude = Latitude, longitude = Longitude},
            od_provider:create(#document{key = ProviderId, value = Provider}),
            {ok, {ProviderId, ProviderCertPem}}
    end;

create(_, ProviderId, support, Data) ->
    SupportSize = maps:get(<<"size">>, Data),
    Macaroon = maps:get(<<"token">>, Data),
    {ok, {od_space, SpaceId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_space, SpaceId, od_provider, ProviderId, SupportSize
    ),
    {ok, SpaceId};

create(_, undefined, check_my_ports, Data) ->
    try
        test_connection(Data)
    catch _:_ ->
        ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: n_entity_logic:client(), EntityId :: n_entity_logic:entity_id(),
    Entity :: n_entity_logic:entity(), Resource :: resource()) ->
    n_entity_logic:result().
get(_, undefined, undefined, list) ->
    {ok, ProviderDocs} = od_provider:list(),
    {ok, [ProviderId || #document{key = ProviderId} <- ProviderDocs]};
get(_, _ProviderId, #od_provider{} = Provider, data) ->
    #od_provider{
        name = Name, urls = Urls, redirection_point = RedirectionPoint,
        latitude = Latitude, longitude = Longitude
    } = Provider,
    {ok, #{
        <<"name">> => Name, <<"urls">> => Urls,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"latitude">> => Latitude, <<"longitude">> => Longitude,
        % TODO VFS-2918
        <<"clientName">> => Name
    }};
get(_, _ProviderId, #od_provider{spaces = Spaces}, spaces) ->
    {ok, maps:keys(Spaces)};
get(_, _ProviderId, #od_provider{}, {space, SpaceId}) ->
    {ok, Space} = ?throw_on_failure(n_space_logic_plugin:get_entity(SpaceId)),
    n_space_logic_plugin:get(?ROOT, SpaceId, Space, data);
get(_, _ProviderId, #od_provider{eff_users = EffUsers}, eff_users) ->
    {ok, maps:keys(EffUsers)};
get(_, _ProviderId, #od_provider{}, {eff_user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _ProviderId, #od_provider{eff_groups = EffGroups}, eff_groups) ->
    {ok, maps:keys(EffGroups)};
get(_, _ProviderId, #od_provider{}, {eff_group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, undefined, undefined, {check_my_ip, Req}) ->
    {{Ip, _Port}, _} = cowboy_req:peer(Req),
    {ok, list_to_binary(inet_parse:ntoa(Ip))}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec update(EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    n_entity_logic:data()) -> n_entity_logic:result().
update(ProviderId, entity, Data) ->
    {ok, _} = od_provider:update(ProviderId, fun(Provider) ->
        #od_provider{
            name = Name, urls = URLs, redirection_point = RedPoint,
            latitude = Latitude, longitude = Longitude
        } = Provider,
        {ok, Provider#od_provider{
            % TODO VFS-2918
            name = maps:get(<<"name">>, Data, maps:get(<<"clientName">>, Data, Name)),
            urls = maps:get(<<"urls">>, Data, URLs),
            redirection_point = maps:get(<<"redirectionPoint">>, Data, RedPoint),
            latitude = maps:get(<<"latitude">>, Data, Latitude),
            longitude = maps:get(<<"longitude">>, Data, Longitude)
        }}
    end),
    ok;

update(ProviderId, {space, SpaceId}, Data) ->
    NewSupportSize = maps:get(<<"size">>, Data),
    entity_graph:update_relation(
        od_space, SpaceId, od_provider, ProviderId, NewSupportSize
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec delete(EntityId :: n_entity_logic:entity_id(), Resource :: resource()) ->
    n_entity_logic:result().
delete(ProviderId, entity) ->
    entity_graph:delete_with_relations(od_provider, ProviderId);

delete(ProviderId, {space, SpaceId}) ->
    entity_graph:remove_relation(
        od_space, SpaceId, od_provider, ProviderId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec exists(Resource :: resource()) ->
    n_entity_logic:existence_verificator()|
    [n_entity_logic:existence_verificator()].
exists({space, SpaceId}) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{spaces = Spaces}) ->
        maps:is_key(SpaceId, Spaces)
    end};

exists({eff_user, UserId}) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{eff_users = EffUsers}) ->
        maps:is_key(UserId, EffUsers)
    end};

exists({eff_group, GroupId}) ->
    % No matter the resource, return true if it belongs to a provider
    {internal, fun(#od_provider{eff_groups = EffGroups}) ->
        maps:is_key(GroupId, EffGroups)
    end};

exists(_) ->
    {internal, fun(#od_provider{}) ->
        % If the provider with ProviderId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec authorize(Operation :: n_entity_logic:operation(),
    EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    Client :: n_entity_logic:client()) ->
    n_entity_logic:authorization_verificator() |
    [authorization_verificator:existence_verificator()].
authorize(create, undefined, check_my_ports, _) ->
    true;

authorize(create, undefined, entity, _) ->
    true;

authorize(create, undefined, entity_dev, _) ->
    true;

authorize(create, ProvId, support, ?PROVIDER(ProvId)) ->
    true;


authorize(get, undefined, {check_my_ip, _}, _) ->
    true;

authorize(get, _ProvId, entity, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST);

authorize(get, ProvId, entity, ?PROVIDER(ProvId)) ->
    true;

authorize(get, _ProvId, data, ?PROVIDER) ->
    % Any provider can get data about other providers
    true;

authorize(get, _ProvId, data, ?USER(UserId)) -> [
    auth_by_membership(UserId),
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST)
];

authorize(get, undefined, list, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST);

authorize(get, _ProvId, eff_users, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_USERS);

authorize(get, _ProvId, {eff_user, _}, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_USERS);

authorize(get, _ProvId, eff_groups, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_GROUPS);

authorize(get, _ProvId, {eff_group, _}, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_GROUPS);

authorize(get, ProvId, spaces, ?PROVIDER(ProvId)) ->
    true;

authorize(get, _ProvId, spaces, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_SPACES);

authorize(get, ProvId, {space, _}, ?PROVIDER(ProvId)) ->
    true;

authorize(get, _ProvId, {space, _}, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_LIST_SPACES);


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


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given Operation and Resource identifier.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(Operation :: n_entity_logic:operation(),
    Resource :: resource()) ->
    n_entity_logic:validity_verificator().
validate(create, entity) -> #{
    required => #{
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"csr">> => {binary, non_empty}
    },
    optional => #{
        <<"latitude">> => {float, {between, -90, 90}},
        <<"longitude">> => {float, {between, -180, 180}}
    },
    % TODO VFS-2918
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"clientName">> => {binary, non_empty}
    }
};
validate(create, entity_dev) -> #{
    required => #{
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"csr">> => {binary, non_empty},
        <<"uuid">> => {binary, non_empty}
    },
    optional => #{
        <<"latitude">> => {float, {between, -90, 90}},
        <<"longitude">> => {float, {between, -180, 180}}
    },
    % TODO VFS-2918
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"clientName">> => {binary, non_empty}
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
        <<"clientName">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty},
        <<"latitude">> => {float, {between, -90, 90}},
        <<"longitude">> => {float, {between, -180, 180}}
    }
};
validate(update, {space, _SpaceId}) -> #{
    required => #{
        <<"size">> => {integer, {not_lower_than, get_min_support_size()}}
    }
}.


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the entity with given id.
%% @end
%%--------------------------------------------------------------------
-spec entity_to_string(EntityId :: n_entity_logic:entity_id()) -> binary().
entity_to_string(SpaceId) ->
    od_provider:to_string(SpaceId).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user is an
%% effective user of the provider represented by entity.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_membership(UserId :: od_user:id()) ->
    n_entity_logic:authorization_verificator().
auth_by_membership(UserId) ->
    {internal, fun(#od_provider{eff_users = EffUsers}) ->
        maps:is_key(UserId, EffUsers)
    end}.


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
    test_connection(maps:to_list(Map), #{}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tests connection to given urls.
%% @end
%%--------------------------------------------------------------------
-spec test_connection([{ServiceName :: binary(), Url :: binary()}], Result) ->
    {ok, Result}
    when Result :: #{Url :: binary() => Status :: ok | error}.
test_connection([], Acc) ->
    {ok, Acc};
test_connection([{<<"undefined">>, <<Url/binary>>} | Rest], Acc) ->
    ConnStatus = case http_client:get(Url, #{}, <<>>, [insecure]) of
        {ok, 200, _, _} ->
            ok;
        _ ->
            error
    end,
    test_connection(Rest, Acc#{Url => ConnStatus});
test_connection([{<<ServiceName/binary>>, <<Url/binary>>} | Rest], Acc) ->
    ConnStatus = case http_client:get(Url, #{}, <<>>, [insecure]) of
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads minimum space support size from app.config env variable.
%% @end
%%--------------------------------------------------------------------
-spec get_min_support_size() -> integer().
get_min_support_size() ->
    {ok, MinSupportSize} = application:get_env(
        oz_worker, minimum_space_support_size
    ),
    MinSupportSize.