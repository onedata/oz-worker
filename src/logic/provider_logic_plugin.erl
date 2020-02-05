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
-module(provider_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

-define(MINIMUM_SUPPORT_SIZE, oz_worker:get_env(minimum_space_support_size, 1000000)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
%%  * false
%%      if fetch is not applicable for this operation
%%  * {error, _}
%%      if there was an error, such as ?ERROR_NOT_FOUND
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = ProviderId}) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Provider, Revision}};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(create, instance, private) -> true;
operation_supported(create, instance_dev, private) -> true;
operation_supported(create, support, private) -> true;
operation_supported(create, check_my_ports, private) -> true;
operation_supported(create, map_idp_user, private) -> true;
operation_supported(create, map_idp_group, private) -> true;
operation_supported(create, verify_provider_identity, private) -> true;
operation_supported(create, {dns_txt_record, _}, private) -> true;

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, instance, shared) -> true;

operation_supported(get, eff_users, private) -> true;
operation_supported(get, {eff_user_membership, _}, private) -> true;
operation_supported(get, eff_groups, private) -> true;
operation_supported(get, {eff_group_membership, _}, private) -> true;
operation_supported(get, eff_harvesters, private) -> true;
operation_supported(get, spaces, private) -> true;
operation_supported(get, eff_spaces, private) -> true;
operation_supported(get, {user_spaces, _}, private) -> true;
operation_supported(get, {group_spaces, _}, private) -> true;
operation_supported(get, domain_config, private) -> true;
operation_supported(get, {check_my_ip, _}, private) -> true;
operation_supported(get, current_time, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {space, _}, private) -> true;
operation_supported(update, domain_config, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {space, _}, private) -> true;
operation_supported(delete, {dns_txt_record, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, _) -> true;
is_subscribable({user_spaces, _}, private) -> true;
is_subscribable({eff_user_membership, _}, private) -> true;
is_subscribable({eff_group_membership, _}, private) -> true;
is_subscribable(eff_harvesters, _) -> true;
is_subscribable(_, _) -> false.

%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{auth = Auth, gri = #gri{id = undefined, aspect = instance} = GRI}) ->
    Data = Req#el_req.data,
    create_provider(Auth, Data, datastore_key:new(), GRI);

create(Req = #el_req{auth = Auth, gri = #gri{id = undefined, aspect = instance_dev} = GRI}) ->
    Data = Req#el_req.data,
    create_provider(Auth, Data, maps:get(<<"uuid">>, Data, undefined), GRI);

%% @TODO VFS-5856 deprecated, included for backward compatibility
%% Used by providers, that do not keep storages in onezone
create(#el_req{gri = #gri{id = ProviderId, aspect = support}, data = Data}) ->
    %% Virtual storage record with id equal to providers is created.
    %% This record will be used to keep support information of all provider spaces.
    %% It will be deleted during provider upgrade.
    case provider_logic:has_storage(ProviderId, ProviderId) of
        true -> ok;
        false -> storage_logic:create(?PROVIDER(ProviderId), ProviderId, ?STORAGE_DEFAULT_NAME)
    end,
    {ok, SpaceId} = storage_logic:support_space(?PROVIDER(ProviderId), ProviderId, Data),
    {true, {
        #od_space{} = Space,
        Rev
    }} = space_logic_plugin:fetch_entity(#gri{id = SpaceId}),

    NewGRI = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
    {ok, SpaceData} = space_logic_plugin:get(#el_req{gri = NewGRI}, Space),
    {ok, resource, {NewGRI, {SpaceData, Rev}}};

create(Req = #el_req{gri = #gri{aspect = check_my_ports}}) ->
    try
        {ok, value, test_connection(Req#el_req.data)}
    catch _:_ ->
        ?ERROR_INTERNAL_SERVER_ERROR
    end;

create(#el_req{gri = #gri{id = ProviderId, aspect = {dns_txt_record, RecordName}}, data = Data}) ->
    case fetch_entity(#gri{id = ProviderId}) of
        {true, {#od_provider{subdomain_delegation = true}, _}} ->
            #{<<"content">> := Content} = Data,
            TTL = maps:get(<<"ttl">>, Data, undefined),
            ok = dns_state:set_txt_record(ProviderId, RecordName, Content, TTL);
        {true, {#od_provider{subdomain_delegation = false}, _}} ->
            ?ERROR_SUBDOMAIN_DELEGATION_DISABLED;
        Error ->
            Error
    end;

create(#el_req{gri = #gri{aspect = map_idp_user}, data = Data}) ->
    IdP = maps:get(<<"idp">>, Data),
    UserId = maps:get(<<"userId">>, Data),
    {ok, value, linked_accounts:gen_user_id(IdP, UserId)};

create(#el_req{gri = #gri{aspect = map_idp_group}, data = Data}) ->
    IdP = maps:get(<<"idp">>, Data),
    GroupId = maps:get(<<"groupId">>, Data),
    try entitlement_mapping:map_entitlement(IdP, GroupId) of
        {ok, {OnedataGroupId, _}} -> {ok, value, OnedataGroupId};
        {error, malformed} -> ?ERROR_BAD_DATA(<<"groupId">>)
    catch
        Type:Reason ->
            ?debug_stacktrace("Cannot map group '~s' from IdP '~p' due to ~p:~p", [
                GroupId, IdP, Type, Reason
            ]),
            ?ERROR_MALFORMED_DATA
    end;

%% @TODO VFS-5846 old provider verification API kept for backward compatibility
create(#el_req{auth = Auth, gri = #gri{aspect = verify_provider_identity}, data = Data}) ->
    ProviderId = maps:get(<<"providerId">>, Data),
    %% @TODO VFS-5554 Deprecated, included for backward compatibility
    Token = case maps:find(<<"macaroon">>, Data) of
        {ok, M} -> M;
        error -> maps:get(<<"token">>, Data)
    end,
    AuthCtx = token_auth:build_auth_ctx(undefined, Auth#auth.peer_ip, aai:auth_to_audience(Auth)),
    case token_auth:verify_identity_token(Token, AuthCtx) of
        {ok, {?SUB(?ONEPROVIDER, ProviderId), _}} -> ok;
        {ok, _} -> ?ERROR_TOKEN_INVALID;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, ProviderDocs} = od_provider:list(),
    {ok, [ProviderId || #document{key = ProviderId} <- ProviderDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Provider) ->
    {ok, Provider};
get(#el_req{gri = #gri{id = Id, aspect = instance, scope = protected}}, Provider) ->
    #od_provider{
        name = Name, domain = Domain,
        latitude = Latitude, longitude = Longitude,
        creation_time = CreationTime
    } = Provider,
    {ok, #{
        <<"name">> => Name, <<"domain">> => Domain,
        <<"latitude">> => Latitude, <<"longitude">> => Longitude,
        <<"online">> => provider_connections:is_online(Id),
        <<"creationTime">> => CreationTime
    }};
get(#el_req{gri = #gri{aspect = instance, scope = shared}}, Provider) ->
    #od_provider{name = Name} = Provider,
    {ok, #{<<"name">> => Name}};

get(#el_req{gri = #gri{aspect = domain_config, id = ProviderId}}, Provider) ->
    #od_provider{
        domain = Domain, subdomain_delegation = SubdomainDelegation
    } = Provider,
    Response = #{
        <<"domain">> => Domain,
        <<"subdomainDelegation">> => SubdomainDelegation,
        <<"subdomain">> => null,
        <<"ipList">> => []
    },
    case SubdomainDelegation of
        true ->
            {ok, Subdomain, IPs} = dns_state:get_delegation_config(ProviderId),
            {ok, Response#{
                <<"subdomain">> => Subdomain,
                <<"ipList">> => IPs
            }};
        false ->
            {ok, Response}
    end;

get(#el_req{gri = #gri{aspect = eff_users}}, Provider) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Provider)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Provider) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, Provider)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Provider) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Provider)};
get(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Provider) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, GroupId, Provider)};
get(#el_req{gri = #gri{aspect = eff_harvesters}}, Provider) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_harvester, Provider)};

%% @TODO VFS-5856 deprecated, included for backward compatibility
%% Used by providers, that do not keep storages in onezone
get(#el_req{gri = #gri{aspect = spaces}}, Provider) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_space, Provider)};
get(#el_req{gri = #gri{aspect = eff_spaces}}, Provider) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_space, Provider)};

get(#el_req{gri = #gri{aspect = {user_spaces, UserId}}}, Provider) ->
    {ok, AllSpaces} = get(#el_req{gri = #gri{aspect = spaces}}, Provider),
    {ok, UserSpaces} = user_logic:get_eff_spaces(?ROOT, UserId),
    {ok, ordsets:intersection(
        ordsets:from_list(AllSpaces),
        ordsets:from_list(UserSpaces)
    )};

get(#el_req{gri = #gri{aspect = {group_spaces, GroupId}}}, Provider) ->
    {ok, AllSpaces} = get(#el_req{gri = #gri{aspect = spaces}}, Provider),
    {ok, GroupSpaces} = group_logic:get_eff_spaces(?ROOT, GroupId),
    {ok, ordsets:intersection(
        ordsets:from_list(AllSpaces),
        ordsets:from_list(GroupSpaces)
    )};

get(#el_req{gri = #gri{aspect = {check_my_ip, ClientIP}}}, _) ->
    {ok, ClientIP};

get(#el_req{gri = #gri{aspect = current_time}}, _) ->
    {ok, time_utils:cluster_time_millis()}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = ProviderId, aspect = instance}, data = Data}) ->
    {ok, _} = od_provider:update(ProviderId, fun(Provider) ->
        #od_provider{
            name = Name, latitude = Latitude, longitude = Longitude,
            admin_email = AdminEmail
        } = Provider,
        {ok, Provider#od_provider{
            name = maps:get(<<"name">>, Data, Name),
            admin_email = maps:get(<<"adminEmail">>, Data, AdminEmail),
            latitude = maps:get(<<"latitude">>, Data, Latitude),
            longitude = maps:get(<<"longitude">>, Data, Longitude)
        }}
    end),
    ok;


%% @TODO VFS-5856 deprecated, included for backward compatibility
%% Used by providers, that do not keep storages in onezone
update(#el_req{gri = #gri{id = ProviderId, aspect = {space, SpaceId}}, data = Data}) ->
    storage_logic:update_support_size(?PROVIDER(ProviderId), ProviderId, SpaceId, Data);

update(#el_req{gri = #gri{id = ProviderId, aspect = domain_config}, data = Data}) ->
    % prevent race condition with simultaneous updates
    critical_section:run({domain_config_update, ProviderId}, fun() ->
        case maps:get(<<"subdomainDelegation">>, Data) of
            true -> update_provider_subomain(ProviderId, Data);
            false -> update_provider_domain(ProviderId, Data)
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = ProviderId, aspect = instance}}) ->
    ok = dns_state:remove_delegation_config(ProviderId),
    {true, {#od_provider{
        name = Name,
        eff_spaces = Spaces,
        eff_harvesters = Harvesters
    }, _}} = fetch_entity(#gri{aspect = instance, id = ProviderId}),

    % Invalidate client tokens
    temporary_token_secret:clear(?SUB(?ONEPROVIDER, ProviderId)),
    token_logic:delete_all_provider_named_tokens(?PROVIDER(ProviderId), ProviderId),

    lists:foreach(fun(HarvesterId) ->
        harvester_indices:update_stats(HarvesterId, all,
            fun(ExistingStats) ->
                harvester_indices:coalesce_index_stats(ExistingStats, maps:keys(Spaces), ProviderId, true)
            end)
    end, maps:keys(Harvesters)),

    ClusterId = ProviderId,
    entity_graph:remove_all_relations(od_cluster, ClusterId),
    entity_graph:delete_with_relations(od_provider, ProviderId),
    cluster_logic:delete_oneprovider_cluster(ClusterId),

    ?info("Provider '~ts' has been deregistered (~s)", [Name, ProviderId]),

    % Force disconnect the provider (if connected), but with a delay to allow
    % some time for the provider to receive the response to deletion request.
    timer:apply_after(10000, provider_connections, close_all, [ProviderId]),
    ok;

%% @TODO VFS-5856 deprecated, included for backward compatibility
%% Used by providers, that do not keep storages in onezone
delete(#el_req{gri = #gri{id = ProviderId, aspect = {space, SpaceId}}}) ->
    storage_logic:revoke_support(?PROVIDER(ProviderId), ProviderId, SpaceId);

delete(#el_req{gri = #gri{id = ProviderId, aspect = {dns_txt_record, RecordName}}}) ->
    ok = dns_state:remove_txt_record(ProviderId, RecordName).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, Provider) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            provider_logic:has_eff_user(Provider, UserId);
        ?THROUGH_GROUP(GroupId) ->
            provider_logic:has_eff_group(Provider, GroupId);
        ?THROUGH_SPACE(SpaceId) ->
            provider_logic:supports_space(Provider, SpaceId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Provider) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Provider);

exists(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Provider) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Provider);

exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, Provider) ->
    entity_graph:has_relation(effective, bottom_up, od_space, SpaceId, Provider);

exists(#el_req{gri = #gri{aspect = {user_spaces, UserId}}}, Provider) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Provider);

exists(#el_req{gri = #gri{aspect = {group_spaces, GroupId}}}, Provider) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Provider);

% All other aspects exist if provider record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_provider{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(#el_req{operation = create, gri = #gri{aspect = check_my_ports}}, _) ->
    true;

authorize(#el_req{operation = create, gri = #gri{aspect = map_idp_user}}, _) ->
    true;

authorize(#el_req{operation = create, gri = #gri{aspect = map_idp_group}}, _) ->
    true;

authorize(#el_req{operation = create, gri = #gri{aspect = verify_provider_identity}}, _) ->
    true;

authorize(#el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}, _) ->
    true;

authorize(#el_req{operation = create, gri = #gri{id = undefined, aspect = instance_dev}}, _) ->
    true =:= oz_worker:get_env(dev_mode, true);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = support}}, _) ->
    auth_by_self(Req);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {dns_txt_record, _}}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_UPDATE);

authorize(#el_req{operation = get, gri = #gri{aspect = {check_my_ip, _}}}, _) ->
    true;

authorize(#el_req{operation = get, gri = #gri{aspect = current_time}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, gri = GRI = #gri{id = ProvId, aspect = instance, scope = protected}}, Provider) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this provider is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(UserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this provider is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_VIEW);

        {?PROVIDER(ClientProviderId), ?THROUGH_SPACE(SpaceId)} ->
            % Space's support by subject provider is checked in 'exists'
            provider_logic:supports_space(ClientProviderId, SpaceId);

        {?USER(UserId), ?THROUGH_SPACE(SpaceId)} ->
            % Space's support by this provider is checked in 'exists'
            user_logic:has_eff_space(UserId, SpaceId);

        {?PROVIDER(_), _} ->
            % Providers are allowed to view each other's protected data
            true;

        {?USER(UserId), _} ->
            ClusterId = ProvId,
            provider_logic:has_eff_user(Provider, UserId) orelse
                cluster_logic:has_eff_user(ClusterId, UserId);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Provider)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{id = ProviderId, aspect = instance, scope = shared}}, Provider) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_HARVESTER(HarvesterId)} ->
            provider_logic:has_eff_harvester(ProviderId, HarvesterId)
                andalso harvester_logic:has_eff_user(HarvesterId, UserId);
        _ ->
            % Access to protected data also allows access to shared data
            authorize(Req#el_req{gri = GRI#gri{scope = protected}}, Provider)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_users}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_groups}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_group_membership, GroupId}}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW) orelse group_logic:has_eff_user(GroupId, UserId);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_group_membership, _}}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_harvesters}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = spaces}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_spaces}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(#el_req{auth = ?USER(UserId), operation = get, gri = #gri{aspect = {user_spaces, UserId}}}, _) ->
    true;

authorize(#el_req{auth = ?USER(UserId), operation = get, gri = #gri{aspect = {group_spaces, GroupId}}}, _) ->
    group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = domain_config}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {space, _}}}, _) ->
    auth_by_self(Req);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = domain_config}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_UPDATE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {dns_txt_record, _}}}, _) ->
    auth_by_self_or_cluster_privilege(Req, ?CLUSTER_UPDATE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {space, _}}}, _) ->
    auth_by_self(Req);

authorize(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_PROVIDERS_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance}}) ->
    [?OZ_PROVIDERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = domain_config}}) ->
    [?OZ_PROVIDERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_PROVIDERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}) ->
    [?OZ_PROVIDERS_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_PROVIDERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_membership, _}}}) ->
    [?OZ_PROVIDERS_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_harvesters}}) ->
    [?OZ_PROVIDERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = spaces}}) ->
    [?OZ_PROVIDERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_spaces}}) ->
    [?OZ_PROVIDERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_PROVIDERS_UPDATE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_PROVIDERS_DELETE];

required_admin_privileges(_) ->
    forbidden.


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = create, gri = #gri{aspect = instance}, data = Data}) ->
    SubdomainDelegationSupported = oz_worker:get_env(subdomain_delegation_supported, true),
    SubdomainDelegationParam = maps:get(<<"subdomainDelegation">>, Data, undefined),
    DomainRelatedFields = case {SubdomainDelegationParam, SubdomainDelegationSupported} of
        {true, false} ->
            throw(?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED);
        {true, true} ->
            #{
                <<"subdomain">> => {binary, subdomain},
                <<"ipList">> => {list_of_ipv4_addresses, any}
            };
        {false, _} ->
            #{<<"domain">> => {binary, domain}};
        {_, _} ->
            % valid subdomainDelegation field was not sent, which will cause
            % BAD_DATA error. No need to generate domain related fields.
            #{}
    end,
    #{
        required => DomainRelatedFields#{
            <<"token">> => {invite_token, ?REGISTER_ONEPROVIDER},
            <<"name">> => {binary, name},
            <<"subdomainDelegation">> => {boolean, any},
            <<"adminEmail">> => {binary, email}
        },
        optional => #{
            <<"latitude">> => {float, {between, -90, 90}},
            <<"longitude">> => {float, {between, -180, 180}}
        }
    };

validate(Req = #el_req{operation = create, gri = GRI = #gri{aspect = instance_dev}}) ->
    ValidationRules = #{required := Required} = validate(Req#el_req{gri = GRI#gri{aspect = instance}}),
    ValidationRules#{
        required => Required#{
            <<"uuid">> => {binary, non_empty}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = support}}) -> #{
    required => #{
        <<"token">> => {invite_token, ?SUPPORT_SPACE},
        <<"size">> => {integer, {not_lower_than, ?MINIMUM_SUPPORT_SIZE}}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {dns_txt_record, _}}}) ->
    #{
        required => #{
            {aspect, <<"recordName">>} => {binary, non_empty},
            <<"content">> => {binary, non_empty}
        },
        optional => #{
            <<"ttl">> => {integer, {not_lower_than, 0}}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = check_my_ports}}) -> #{
};

validate(#el_req{operation = create, gri = #gri{aspect = map_idp_user}}) -> #{
    required => #{
        <<"idp">> => {atom, {exists, fun auth_config:idp_exists/1}},
        <<"userId">> => {binary, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = map_idp_group}}) -> #{
    required => #{
        <<"idp">> => {atom, {exists, fun entitlement_mapping:enabled/1}},
        <<"groupId">> => {binary, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = verify_provider_identity}, data = Data}) ->
    %% @TODO VFS-5554 Deprecated, included for backward compatibility
    TokenKey = case maps:is_key(<<"macaroon">>, Data) of
        true -> <<"macaroon">>;
        false -> <<"token">>
    end,
    #{
        required => #{
            <<"providerId">> => {any, {exists, fun provider_logic:exists/1}},
            TokenKey => {token, any}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"adminEmail">> => {binary, email},
        <<"latitude">> => {float, {between, -90, 90}},
        <<"longitude">> => {float, {between, -180, 180}}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {space, _}}}) -> #{
    required => #{
        <<"size">> => {integer, {not_lower_than, ?MINIMUM_SUPPORT_SIZE}}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = domain_config}, data = Data}) ->
    case maps:get(<<"subdomainDelegation">>, Data, undefined) of
        true ->
            true == oz_worker:get_env(subdomain_delegation_supported, true) orelse
                throw(?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED),
            #{required => #{
                <<"subdomainDelegation">> => {boolean, any},
                <<"subdomain">> => {binary, subdomain},
                <<"ipList">> => {list_of_ipv4_addresses, any}
            }};
        false ->
            #{required => #{
                <<"subdomainDelegation">> => {boolean, any},
                <<"domain">> => {binary, domain}
            }};
        _ ->
            #{required => #{
                <<"subdomainDelegation">> => {boolean, any}
            }}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec auth_by_self(entity_logic:req()) -> boolean().
auth_by_self(#el_req{auth = ?PROVIDER(ProvId), gri = #gri{id = ProvId}}) ->
    true;
auth_by_self(_) ->
    false.


-spec auth_by_self_or_cluster_privilege(entity_logic:req(), privileges:cluster_privilege()) ->
    boolean().
auth_by_self_or_cluster_privilege(#el_req{auth = ?USER(UserId), gri = #gri{id = ProvId}}, Privilege) ->
    ClusterId = ProvId,
    cluster_logic:has_eff_privilege(ClusterId, UserId, Privilege);
auth_by_self_or_cluster_privilege(Req, _) ->
    auth_by_self(Req).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tests connection to given urls.
%% @end
%% @equiv test_connection/2
%%--------------------------------------------------------------------
-spec test_connection(#{ServiceName :: binary() => Url :: binary()}) ->
    #{ServiceName :: binary() => Status :: ok | error}.
test_connection(Map) ->
    test_connection(maps:to_list(Map), #{}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tests connection to given urls.
%% @end
%%--------------------------------------------------------------------
-spec test_connection([{ServiceName :: binary(), Url :: binary()}], Result) ->
    Result when Result :: #{Url :: binary() => Status :: ok | error}.
test_connection([], Acc) ->
    Acc;
test_connection([{<<"undefined">>, <<Url/binary>>} | Rest], Acc) ->
    Opts = [{ssl_options, [{secure, false}]}],
    ConnStatus = case http_client:get(Url, #{}, <<>>, Opts) of
        {ok, 200, _, _} ->
            ok;
        _ ->
            error
    end,
    test_connection(Rest, Acc#{Url => ConnStatus});
test_connection([{<<ServiceName/binary>>, <<Url/binary>>} | Rest], Acc) ->
    Opts = [{ssl_options, [{secure, false}]}],
    ConnStatus = case http_client:get(Url, #{}, <<>>, Opts) of
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
%% Sets the domain of a provider, ensuring the domain is not used
%% by another provider.
%% @end
%%--------------------------------------------------------------------
-spec update_provider_domain(ProviderId :: od_provider:id(),
    Data :: entity_logic:data()) -> entity_logic:update_result().
update_provider_domain(ProviderId, Data) ->
    Domain = maps:get(<<"domain">>, Data),
    Result = od_provider:update(ProviderId, fun(Provider) ->
        {ok, Provider#od_provider{
            subdomain_delegation = false,
            domain = Domain,
            subdomain = undefined
        }}
    end),
    case Result of
        {ok, _} ->
            dns_state:remove_delegation_config(ProviderId),
            ok;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates provider to use given subdomain.
%% @end
%%--------------------------------------------------------------------
-spec update_provider_subomain(ProviderId :: od_provider:id(),
    Data :: entity_logic:data()) -> entity_logic:update_result().
update_provider_subomain(ProviderId, Data) ->
    Subdomain = maps:get(<<"subdomain">>, Data),
    Domain = dns_config:build_fqdn_from_subdomain(Subdomain),
    IPs = maps:get(<<"ipList">>, Data),
    Result = case dns_state:set_delegation_config(ProviderId, Subdomain, IPs) of
        ok ->
            od_provider:update(ProviderId, fun(Provider) ->
                {ok, Provider#od_provider{
                    subdomain_delegation = true,
                    domain = Domain,
                    subdomain = Subdomain
                }}
            end);
        {error, subdomain_exists} ->
            ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>)
    end,
    case Result of
        {ok, _} -> ok;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a new provider document in database.
%% @end
%%--------------------------------------------------------------------
-spec create_provider(aai:auth(), Data :: map(), ProviderId :: od_provider:id(),
    GRI :: entity_logic:gri()) -> entity_logic:create_result().
create_provider(Auth, Data, ProviderId, GRI) ->
    Name = maps:get(<<"name">>, Data),
    Token = maps:get(<<"token">>, Data),
    Latitude = maps:get(<<"latitude">>, Data, 0.0),
    Longitude = maps:get(<<"longitude">>, Data, 0.0),
    SubdomainDelegation = maps:get(<<"subdomainDelegation">>, Data),
    AdminEmail = maps:get(<<"adminEmail">>, Data),

    invite_tokens:consume(Auth, Token, ?REGISTER_ONEPROVIDER, fun(CreatorUserId, _) ->
        {ok, RootToken} = token_logic:create_provider_named_token(?ROOT, ProviderId, #{
            <<"name">> => ?PROVIDER_ROOT_TOKEN_NAME,
            <<"type">> => ?ACCESS_TOKEN
        }),

        {Domain, Subdomain} = case SubdomainDelegation of
            false ->
                {maps:get(<<"domain">>, Data), undefined};
            true ->
                ReqSubdomain = maps:get(<<"subdomain">>, Data),
                IPs = maps:get(<<"ipList">>, Data),
                case dns_state:set_delegation_config(ProviderId, ReqSubdomain, IPs) of
                    ok ->
                        {dns_config:build_fqdn_from_subdomain(ReqSubdomain), ReqSubdomain};
                    {error, subdomain_exists} ->
                        throw(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>))
                end
        end,

        ProviderRecord = #od_provider{
            name = Name, root_token = RootToken#token.id,
            subdomain_delegation = SubdomainDelegation,
            domain = Domain, subdomain = Subdomain,
            latitude = Latitude, longitude = Longitude,
            admin_email = AdminEmail
        },

        try
            {ok, _} = od_provider:create(#document{key = ProviderId, value = ProviderRecord}),
            cluster_logic:create_oneprovider_cluster(CreatorUserId, ProviderId),
            {true, {Provider, Rev}} = fetch_entity(#gri{aspect = instance, id = ProviderId}),
            ?info("Provider '~ts' has registered (~s)", [Name, ProviderId]),
            {ok, resource, {GRI#gri{id = ProviderId}, {{Provider, RootToken}, Rev}}}
        catch Type:Reason ->
            ?error_stacktrace("Cannot create a new provider due to ~p:~p", [Type, Reason]),
            dns_state:remove_delegation_config(ProviderId),
            ?ERROR_INTERNAL_SERVER_ERROR
        end
    end).
