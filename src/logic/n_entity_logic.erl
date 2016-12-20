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
-module(n_entity_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([create/5, get/4, update/5, delete/4]).
-export([ahaha/0]).

% TODO HINT JAK JEST BAD VALUE TYPU ZA MALY SUPPORT SIZE

-record(request, {
    client = #client{} :: #client{},
    el_plugin = undefined :: undefined | atom(),
    entity_id = undefined :: undefined | binary(),
    entity = undefined :: undefined | term(),
    operation = create :: atom(), % TODO
    resource = undefined :: undefined | term(),
    data = #{} :: maps:map()
}).

-type client() :: #client{}.
-export_type([client/0]).


% TODO do typu
type_rule() -> [
    atom,
    list_of_atoms,
    binary,
    list_of_binaries,
    integer,
    positive_integer,
    float,
    json,
    token
].

value_rule() -> [
    any,
    non_empty,
    [possible_values],
    fun() -> true end,
    {exists, fun(Id) -> true end},
    {not_exists, fun(Id) -> true end},
    token_type % compatible only with token
].

-define(PROXY_URL, <<"172.17.0.9:8080/api/v1">>).
-define(METADATA, <<"<?xml version=\"1.0\"?>",
    "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
    "<dc:title>Test dataset<\/dc:title>",
    "<dc:creator>John Johnson<\/dc:creator>",
    "<dc:creator>Jane Doe<\/dc:creator>",
    "<dc:subject>Test of datacite<\/dc:subject>",
    "<dc:description>Lorem ipsum lorem ipusm<\/dc:description>",
    "<dc:publisher>Onedata<\/dc:publisher>",
    "<dc:publisher>EGI<\/dc:publisher>",
    "<dc:date>2016<\/dc:date>",
    "<dc:format>application\/pdf<\/dc:format>",
    "<dc:identifier>onedata:LKJHASKFJHASLKDJHKJHuah132easd<\/dc:identifier>",
    "<dc:language>eng<\/dc:language>",
    "<dc:rights>CC-0<\/dc:rights>",
    "<\/metadata>">>).

-define(SERVICE_PROPERTIES, #{
    <<"allowTemplateOverride">> => false,
    <<"doiEndpoint">> => <<"/doi">>,
    <<"host">> => <<"https://mds.test.datacite.org">>,
    <<"mediaEndpoint">> => <<"/media">>,
    <<"metadataEndpoint">> => <<"/metadata">>,
    <<"password">> => <<"eg1Test40DP">>,
    <<"prefix">> => <<"10.5072">>,
    <<"type">> => <<"DOI">>,
    <<"username">> => <<"DATACITE.EGI">>
}).


-define(TEMP_DIR, "/tmp").

-define(CREATE_PROVIDER_DATA, begin
    {_, __CSRFile, _} = generate_cert_files(),
    {ok, __CSR} = file:read_file(__CSRFile),
    #{
        <<"name">> => <<"P1">>,
        <<"urls">> => [<<"127.0.0.1">>],
        <<"redirectionPoint">> => <<"https://127.0.0.1">>,
        <<"csr">> => __CSR,
        <<"latitude">> => 50.0,
        <<"longitude">> => -24.8
    }
end).


generate_cert_files() ->
    Prefix = "provider" ++ integer_to_list(erlang:system_time(micro_seconds)),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.


ahaha() ->
    {ok, U1} = rpc:call(node(), n_user_logic, create, [#od_user{name = <<"U1">>}]),
    ok = n_user_logic:modify_oz_privileges(?ROOT, U1, set, [set_privileges]),
    {ok, U2} = rpc:call(node(), n_user_logic, create, [#od_user{name = <<"U2">>}]),
    {ok, U3} = rpc:call(node(), n_user_logic, create, [#od_user{name = <<"U3">>}]),
    {ok, G1} = rpc:call(node(), n_group_logic, create, [?USER(U1), <<"G1">>]),
    entity_graph:ensure_up_to_date(),
    ok = n_group_logic:modify_oz_privileges(?USER(U1), G1, set, privileges:oz_admin()),
    entity_graph:ensure_up_to_date(),
    {ok, G1} = rpc:call(node(), n_group_logic, add_user, [?USER(U1), G1, U2]),
    {ok, G1} = rpc:call(node(), n_group_logic, add_user, [?USER(U1), G1, U3]),

    {ok, U4} = rpc:call(node(), n_user_logic, create, [#od_user{name = <<"U4">>}]),
    {ok, G2} = rpc:call(node(), n_group_logic, create, [?USER(U4), <<"G2">>]),
    ok = n_group_logic:modify_oz_privileges(?USER(U1), G2, grant, privileges:oz_admin()),
    ok = n_group_logic:modify_oz_privileges(?USER(U1), G2, revoke, privileges:oz_viewer()),
    entity_graph:ensure_up_to_date(),
    {ok, G2} = rpc:call(node(), n_group_logic, add_group, [?USER(U4), G2, G1]),

    {ok, U5} = rpc:call(node(), n_user_logic, create, [#od_user{name = <<"U5">>}]),
    {ok, S1} = rpc:call(node(), n_space_logic, create, [?USER(U5), <<"S1">>]),
    entity_graph:ensure_up_to_date(),
    {ok, S1} = rpc:call(node(), n_space_logic, add_group, [?USER(U1), S1, G2]),
    ProviderData = ?CREATE_PROVIDER_DATA,
    {ok, {P1, _}} = rpc:call(node(), n_provider_logic, create, [?NOBODY, ProviderData]),
    {ok, Token} = rpc:call(node(), n_space_logic, create_invite_provider_token, [?USER(U5), S1]),
    {ok, S1} = rpc:call(node(), n_provider_logic, support_space, [?PROVIDER(P1), P1, Token, 1000000000]),

    {ok, {P2, _}} = rpc:call(node(), n_provider_logic, create, [?NOBODY, ProviderData#{<<"name">> => <<"P2">>}]),
    {ok, Token2} = rpc:call(node(), n_space_logic, create_invite_provider_token, [?USER(U5), S1]),
    {ok, S1} = rpc:call(node(), n_provider_logic, support_space, [?PROVIDER(P2), P2, Token2, 1000000000]),

    {ok, G3} = rpc:call(node(), n_group_logic, create, [?USER(U1), <<"G3">>]),
    entity_graph:ensure_up_to_date(),
    {ok, G3} = rpc:call(node(), n_group_logic, add_user, [?USER(U1), G3, U3]),
    {ok, S1} = rpc:call(node(), n_space_logic, add_group, [?USER(U5), S1, G3]),

    {ok, U6} = rpc:call(node(), n_user_logic, create, [#od_user{name = <<"U6">>}]),
    {ok, HS1} = rpc:call(node(), n_handle_service_logic, create, [
        ?USER(U6), <<"HS1">>, ?PROXY_URL, ?SERVICE_PROPERTIES, <<"tajp">>
    ]),
    entity_graph:ensure_up_to_date(),
    {ok, HS1} = rpc:call(node(), n_handle_service_logic, add_group, [?USER(U6), HS1, G2]),
    {ok, HS1} = rpc:call(node(), n_handle_service_logic, add_group, [?USER(U6), HS1, G3]),
    {ok, HS1} = rpc:call(node(), n_handle_service_logic, add_user, [?USER(U6), HS1, U5]),
    entity_graph:ensure_up_to_date(),

        catch share_logic:remove(<<"Sh1ID">>),
    {ok, Sh1} = rpc:call(node(), n_share_logic, create, [
        ?USER(U5), <<"Sh1ID">>, <<"Sh1">>, <<"fileId">>, S1]),

    % TODO TRICK
    {ok, _} = od_share:update(Sh1, #{public_url => <<"https://onedata.org/share/Sh1ID">>}),

    {ok, H1} = rpc:call(node(), n_handle_logic, create, [
        ?USER(U1), HS1, <<"Share">>, Sh1, ?METADATA
    ]),
    entity_graph:ensure_up_to_date(),
    {ok, H1} = rpc:call(node(), n_handle_logic, add_group, [?USER(U1), H1, G1]),
    entity_graph:ensure_up_to_date(),
    timer:sleep(1500),

    print(od_user, U1),
    print(od_user, U2),
    print(od_user, U3),
    print(od_user, U4),
    print(od_user, U5),
    print(od_user, U6),
    print(od_group, G1),
    print(od_group, G2),
    print(od_group, G3),
    print(od_space, S1),
    print(od_share, Sh1),
    print(od_provider, P1),
    print(od_provider, P2),
    print(od_handle_service, HS1),
    print(od_handle, H1),
    ok.




print(ModelType, Id) when is_atom(ModelType) ->
    {ok, #document{value = Entity}} = ModelType:get(Id),
    print(Entity, Id);
print(#od_user{} = User, Id) ->
    #od_user{
        name = Name,
        groups = Groups, eff_groups = EffGroups,
        spaces = Spaces, eff_spaces = EffSpaces,
        handle_services = HServices, eff_handle_services = EffHServices,
        handles = Handles, eff_handles = EffHandles,
        eff_providers = EffProviders,
        oz_privileges = OzPrivileges, eff_oz_privileges = EffOzPrivileges
    } = User,
    print(od_user, Id, [
        {name, Name},
        {groups, prepare_relation_to_print(Groups, false, false, [])},
        {eff_groups, prepare_relation_to_print(EffGroups, true, false, [])},
        {spaces, prepare_relation_to_print(Spaces, false, false, [])},
        {eff_spaces, prepare_relation_to_print(EffSpaces, true, false, [])},
        {eff_providers, prepare_relation_to_print(EffProviders, true, false, [])},
        {handle_services, prepare_relation_to_print(HServices, false, false, [])},
        {eff_handle_services, prepare_relation_to_print(EffHServices, true, false, [])},
        {handles, prepare_relation_to_print(Handles, false, false, [])},
        {eff_handles, prepare_relation_to_print(EffHandles, true, false, [])},
        {oz_privileges, privs_to_str(OzPrivileges, privileges:oz_privileges())},
        {eff_oz_privileges, privs_to_str(EffOzPrivileges, privileges:oz_privileges())}
    ]);
print(#od_group{} = Group, Id) ->
    AllPrivs = privileges:group_privileges(),
    #od_group{
        name = Name,
        children = Children,
        eff_children = EffChildren,
        parents = Parents, eff_parents = EffParents,
        users = Users, eff_users = EffUsers,
        spaces = Spaces, eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        handle_services = HServices, eff_handle_services = EffHServices,
        handles = Handles, eff_handles = EffHandles,
        oz_privileges = OzPrivileges, eff_oz_privileges = EffOzPrivileges
    } = Group,
    print(od_group, Id, [
        {name, Name},
        {children, prepare_relation_to_print(Children, false, true, AllPrivs)},
        {eff_children, prepare_relation_to_print(EffChildren, true, true, AllPrivs)},
        {parents, prepare_relation_to_print(Parents, false, false, AllPrivs)},
        {eff_parents, prepare_relation_to_print(EffParents, true, false, AllPrivs)},
        {users, prepare_relation_to_print(Users, false, true, AllPrivs)},
        {eff_users, prepare_relation_to_print(EffUsers, true, true, AllPrivs)},
        {spaces, prepare_relation_to_print(Spaces, false, false, AllPrivs)},
        {eff_spaces, prepare_relation_to_print(EffSpaces, true, false, AllPrivs)},
        {eff_providers, prepare_relation_to_print(EffProviders, true, false, [])},
        {handle_services, prepare_relation_to_print(HServices, false, false, [])},
        {eff_handle_services, prepare_relation_to_print(EffHServices, true, false, [])},
        {handles, prepare_relation_to_print(Handles, false, false, [])},
        {eff_handles, prepare_relation_to_print(EffHandles, true, false, [])},
        {oz_privileges, privs_to_str(OzPrivileges, privileges:oz_privileges())},
        {eff_oz_privileges, privs_to_str(EffOzPrivileges, privileges:oz_privileges())}
    ]);
print(#od_space{} = Space, Id) ->
    AllPrivs = privileges:space_privileges(),
    #od_space{
        name = Name,
        users = Users, eff_users = EffUsers,
        groups = Groups, eff_groups = EffGroups,
        shares = Shares,
        providers = Providers
    } = Space,
    print(od_space, Id, [
        {name, Name},
        {users, prepare_relation_to_print(Users, false, true, AllPrivs)},
        {eff_users, prepare_relation_to_print(EffUsers, true, true, AllPrivs)},
        {groups, prepare_relation_to_print(Groups, false, true, AllPrivs)},
        {eff_groups, prepare_relation_to_print(EffGroups, true, true, AllPrivs)},
        {shares, prepare_relation_to_print(Shares, false, false, [])},
        {providers, maps:fold(
            fun(Id, SupportSize, AccMap) ->
                AccMap#{id_to_str(Id) => SupportSize}
            end, #{}, Providers)}
    ]);
print(#od_share{} = Share, Id) ->
    #od_share{
        name = Name,
        space = Space,
        handle = Handle,
        public_url = PublicUrl
    } = Share,
    print(od_share, Id, [
        {name, Name},
        {public_url, PublicUrl},
        {space, str_utils:format_bin("~s#~s", [model_to_str(od_space), id_to_str(Space)])},
        {handle, str_utils:format_bin("~s#~s", [model_to_str(od_handle), id_to_str(Handle)])}
    ]);
print(#od_provider{} = Provider, Id) ->
    #od_provider{
        name = Name,
        eff_users = EffUsers,
        eff_groups = EffGroups,
        spaces = Spaces
    } = Provider,
    print(od_provider, Id, [
        {name, Name},
        {eff_users, prepare_relation_to_print(EffUsers, true, false, [])},
        {eff_groups, prepare_relation_to_print(EffGroups, true, false, [])},
        {spaces, prepare_relation_to_print(Spaces, false, false, [])}
    ]);
print(#od_handle_service{} = HandleService, Id) ->
    AllPrivs = privileges:handle_service_privileges(),
    #od_handle_service{
        name = Name,
        users = Users, eff_users = EffUsers,
        groups = Groups, eff_groups = EffGroups
    } = HandleService,
    print(od_handle_service, Id, [
        {name, Name},
        {users, prepare_relation_to_print(Users, false, true, AllPrivs)},
        {eff_users, prepare_relation_to_print(EffUsers, true, true, AllPrivs)},
        {groups, prepare_relation_to_print(Groups, false, true, AllPrivs)},
        {eff_groups, prepare_relation_to_print(EffGroups, true, true, AllPrivs)}
    ]);
print(#od_handle{} = Handle, Id) ->
    AllPrivs = privileges:handle_privileges(),
    #od_handle{
        handle_service = HandleServiceId,
        resource_id = ShareId,
        public_handle = PublicHandle,
        users = Users, eff_users = EffUsers,
        groups = Groups, eff_groups = EffGroups
    } = Handle,
    print(od_handle, Id, [
        {name, id_to_str(HandleServiceId)},
        {public_handle, PublicHandle},
        {share, str_utils:format_bin("~s#~s", [model_to_str(od_share), id_to_str(ShareId)])},
        {users, prepare_relation_to_print(Users, false, true, AllPrivs)},
        {eff_users, prepare_relation_to_print(EffUsers, true, true, AllPrivs)},
        {groups, prepare_relation_to_print(Groups, false, true, AllPrivs)},
        {eff_groups, prepare_relation_to_print(EffGroups, true, true, AllPrivs)}
    ]).

print(ModelType, Id, Attrs) ->
    io:format("~s#~s~n", [model_to_str(ModelType), id_to_str(Id)]),
    lists:foreach(
        fun({K, V}) ->
            case V of
                B when is_binary(B) ->
                    io:format("   ~p: ~s~n", [K, V]);
                _ ->
                    io:format("   ~p: ~p~n", [K, V])
            end
        end, Attrs),
    io:format("~n").


prepare_relation_to_print(Map, false = _IsEff, true = _HasPrivs, AllPrivs) ->
    maps:fold(
        fun(Id, Privs, AccMap) ->
            AccMap#{id_to_str(Id) => privs_to_str(Privs, AllPrivs)}
        end, #{}, Map);
prepare_relation_to_print(Map, true = _IsEff, true = _HasPrivs, AllPrivs) ->
    maps:fold(
        fun(Id, {Privs, Intermediaries}, AccMap) ->
            AccMap#{id_to_str(Id) => privs_to_str(Privs, AllPrivs) ++ "  " ++ intermediaries_to_str(Intermediaries)}
        end, #{}, Map);
prepare_relation_to_print(List, false = _IsEff, false = _HasPrivs, _) ->
    lists:map(
        fun(Id) ->
            id_to_str(Id)
        end, List);
prepare_relation_to_print(Map, true = _IsEff, false = _HasPrivs, _) ->
    maps:fold(
        fun(Id, Intermediaries, AccMap) ->
            AccMap#{id_to_str(Id) => intermediaries_to_str(Intermediaries)}
        end, #{}, Map).


privs_to_str(Privs, AllPrivs) ->
    lists:map(
        fun(Priv) ->
            case lists:member(Priv, Privs) of
                true -> $x;
                false -> $-
            end
        end, AllPrivs).


intermediaries_to_str(Intermediaries) ->
    Str = intermediaries_to_str(Intermediaries, "["),
    string:sub_string(Str, 1, length(Str) - 3) ++ "]".
intermediaries_to_str([], Acc) ->
    Acc;
intermediaries_to_str([{Model, Id} | Tail], Acc) ->
    intermediaries_to_str(Tail,
        Acc ++ str_utils:format("~s#~s,  ", [model_to_str(Model), id_to_str(Id)])).

model_to_str(od_user) -> "usr";
model_to_str(od_group) -> "grp";
model_to_str(od_space) -> "spc";
model_to_str(od_share) -> "shr";
model_to_str(od_provider) -> "prv";
model_to_str(od_handle_service) -> "hsr";
model_to_str(od_handle) -> "hnl".


id_to_str(Id) ->
    str_utils:to_list(binary:part(Id, {0, min(7, byte_size(Id))})).



create(Client, ELPlugin, EntityId, Resource, Data) ->
    try
        Request = #request{
            client = Client,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = create,
            data = Data,
            resource = Resource
        },
        call_create(
            check_validity(
                check_authorization(
                    check_existence(Request))))
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in entity_logic:create - ~p:~p", [
                Error, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


get(Client, ELPlugin, EntityId, Resource) ->
    try
        Request = #request{
            client = Client,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = get,
            resource = Resource
        },
        call_get_resource(
            check_authorization(
                check_existence(Request)))
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in entity_logic:get - ~p:~p", [
                Error, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


update(Client, ELPlugin, EntityId, Resource, Data) ->
    try
        Request = #request{
            client = Client,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = update,
            resource = Resource,
            data = Data
        },
        call_update(
            check_validity(
                check_authorization(
                    check_existence(Request))))
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in entity_logic:update - ~p:~p", [
                Error, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


delete(Client, ELPlugin, EntityId, Resource) ->
    try
        Request = #request{
            client = Client,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = delete,
            resource = Resource
        },
        call_delete(
            check_authorization(
                check_existence(Request)))
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in entity_logic:update - ~p:~p", [
                Error, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


call_get_entity(Request) ->
    #request{el_plugin = ELPlugin, entity_id = EntityId} = Request,
    io:format("> call_get_entity: ~p~n", [EntityId]),
    case ELPlugin:get_entity(EntityId) of
        {ok, Entity} ->
            Entity;
        ?ERROR_NOT_FOUND ->
            throw(?ERROR_NOT_FOUND)
    end.


call_get_resource(Request) ->
    #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        entity = Entity,
        resource = Resource
    } = Request,
    % Entity might be already prefetched, reuse it if possible.
    Result = case {EntityId, Entity, Resource} of
        {undefined, _, _} ->
            % EntityId is not defined -> external resource
            io:format("> get_external: ~p~n", [{Client, Resource}]),
            ELPlugin:get_external(Client, Resource);
        {_EntityId, undefined, entity} ->
            % EntityId is defined and asking for entity -> entity resource.
            % The Entity was not fetched yet, fetch and return it.
            {ok, call_get_entity(Request)};
        {_EntityId, Entity, entity} ->
            % EntityId is defined and asking for entity -> entity resource.
            % The Entity is already fetched, return it.
            {ok, Entity};
        {_EntityId, undefined, _} ->
            % EntityId is defined and some resource -> internal resource.
            % The Entity is already fetched, reuse it.
            FetchedEntity = call_get_entity(Request),
            io:format("> get_internal: ~p~n", [{Client, EntityId, freshly_fetched_entity, Resource}]),
            ELPlugin:get_internal(Client, EntityId, FetchedEntity, Resource);
        {_EntityId, Entity, _} ->
            % EntityId is defined and some resource -> internal resource.
            % The Entity was not fetched yet, fetch and use it.
            io:format("> get_internal: ~p~n", [{Client, EntityId, prefetched_entity, Resource}]),
            ELPlugin:get_internal(Client, EntityId, Entity, Resource)
    end,
    case Result of
        {ok, _} ->
            Result;
        ?ERROR_NOT_FOUND ->
            throw(?ERROR_NOT_FOUND)
    end.


call_create(Request) ->
    #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:create(Client, EntityId, Resource, Data).


call_update(Request) ->
    #request{
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:update(EntityId, Resource, Data).


call_delete(Request) ->
    #request{
        el_plugin = ELPlugin,
        resource = Resource
    } = Request,
    ELPlugin:delete(Resource).


call_exists(Request) ->
    #request{
        entity_id = EntityId,
        % TODO potrzeba operation??
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    % Call the plugin to obtain auth verification procedures
    case ELPlugin:exists(EntityId, Resource) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    end.


call_authorize(#request{client = ?ROOT}) ->
    % Root client type is allowed to do everything
    [true];
call_authorize(Request) ->
    #request{
        client = Client,
        entity_id = EntityId,
        operation = Operation,
        resource = Resource,
        el_plugin = ELPlugin,
        data = Data
    } = Request,
    % Call the plugin to obtain auth verification procedures
    try ELPlugin:authorize(Operation, EntityId, Resource, Client, Data) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    catch
        throw:Error ->
            throw(Error);
        _:_ ->
            case Client of
                ?NOBODY ->
                    % The client was not authenticated -> unauthorized
                    throw(?ERROR_UNAUTHORIZED);
                _ ->
                    % The client was authenticated but cannot access the
                    % resource -> forbidden
                    throw(?ERROR_FORBIDDEN)
            end
    end.


call_validate(Request) ->
    #request{
        operation = Operation,
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    ELPlugin:validate(Operation, Resource).


check_existence(Request) ->
    Verificators = call_exists(Request),
    check_existence(Verificators, Request).
check_existence([], _) ->
    throw(?ERROR_NOT_FOUND);
check_existence([true | _], Request) ->
    Request;
check_existence([false | _], _) ->
    throw(?ERROR_NOT_FOUND);
check_existence([{external, Fun} | Tail], Request) ->
    case Fun() of
        true ->
            Request;
        false ->
            check_existence(Tail, Request)
    end;
check_existence([{internal, _} | _] = List, #request{entity = undefined} = Req) ->
    Entity = call_get_entity(Req),
    check_existence(List, Req#request{entity = Entity});
check_existence([{internal, Fun} | Tail], #request{entity = Entity} = Req) ->
    case Fun(Entity) of
        true ->
            Req;
        false ->
            check_existence(Tail, Req)
    end.


check_authorization(#request{client = Client} = Request) ->
    Verificators = call_authorize(Request),
    case check_authorization(Verificators, Request) of
        false ->
            case Client of
                ?NOBODY ->
                    % The client was not authenticated -> unauthorized
                    throw(?ERROR_UNAUTHORIZED);
                _ ->
                    % The client was authenticated but cannot access the
                    % resource -> forbidden
                    throw(?ERROR_FORBIDDEN)
            end;
        NewRequest ->
            NewRequest
    end.
check_authorization([], _) ->
    false;
check_authorization([true | _], Request) ->
    Request;
check_authorization([false | _], _) ->
    false;
check_authorization([{external, Fun} | Tail], Request) ->
    case Fun() of
        true ->
            Request;
        false ->
            check_authorization(Tail, Request)
    end;
check_authorization([{internal, _} | _] = List, #request{entity = undefined} = Req) ->
    Entity = call_get_entity(Req),
    check_authorization(List, Req#request{entity = Entity});
check_authorization([{internal, Fun} | Tail], #request{entity = Entity} = Req) ->
    case Fun(Entity) of
        true ->
            Req;
        false ->
            check_authorization(Tail, Req)
    end.


% TODO jesli juz jest atom to ne trzeba konwersji
check_validity(#request{data = Data} = Request) ->
    ValidatorsMap = call_validate(Request),
    % Get all types of validators validators
    Required = maps:get(required, ValidatorsMap, #{}),
    Optional = maps:get(optional, ValidatorsMap, #{}),
    AtLeastOne = maps:get(at_least_one, ValidatorsMap, #{}),
    % Start with required parameters. Transform the data if needed, fail when
    % any key is missing or cannot be validated.
    Data2 = lists:foldl(
        fun(Key, DataAcc) ->
            case transform_and_check_value(Key, DataAcc, Required) of
                false ->
                    throw(?ERROR_MISSING_REQUIRED_DATA(Key));
                {true, NewData} ->
                    NewData
            end
        end, Data, maps:keys(Required)),
    % Now, optional parameters. Transform the data if needed, fail when
    % any of the keys exists in the data but cannot be validated.
    Data3 = lists:foldl(
        fun(Key, DataAcc) ->
            case transform_and_check_value(Key, DataAcc, Optional) of
                false ->
                    DataAcc;
                {true, NewData} ->
                    NewData
            end
        end, Data2, maps:keys(Optional)),
    % Finally, "at least one" parameters. Transform the data if needed, fail
    % when less than one key exists in the data or any of the keys cannot
    % be validated.
    {Data4, HasAtLeastOne} = lists:foldl(
        fun(Key, {DataAcc, HasAtLeastOneAcc}) ->
            case transform_and_check_value(Key, DataAcc, AtLeastOne) of
                false ->
                    {DataAcc, HasAtLeastOneAcc orelse false};
                {true, NewData} ->
                    {NewData, true}
            end
        end, {Data3, false}, maps:keys(AtLeastOne)),
    case {length(maps:keys(AtLeastOne)), HasAtLeastOne} of
        {_, true} ->
            ok;
        {0, false} ->
            ok;
        {_, false} ->
            throw(?ERROR_MISSING_AT_LEAST_ONE_DATA(maps:keys(AtLeastOne)))
    end,
    Request#request{data = Data4}.


transform_and_check_value(Key, Data, Validator) ->
    case maps:get(Key, Data, undefined) of
        undefined ->
            false;
        Value ->
            {TypeRule, ValueRule} = maps:get(Key, Validator),
            try
                NewValue = check_type(TypeRule, Value),
                case check_value(TypeRule, ValueRule, NewValue) of
                    true ->
                        {true, Data#{Key => NewValue}};
                    false ->
                        throw(?ERROR_BAD_DATA(Key))
                end
            catch
                throw:bad_data ->
                    throw(?ERROR_BAD_DATA(Key));
                throw:empty ->
                    throw(?ERROR_EMPTY_DATA(Key));
                throw:id_not_found ->
                    throw(?ERROR_ID_NOT_FOUND(Key));
                throw:id_occupied ->
                    throw(?ERROR_ID_OCCUPIED(Key));
                throw:bad_token ->
                    throw(?ERROR_BAD_TOKEN(Key));
                throw:bad_token_type ->
                    throw(?ERROR_BAD_TOKEN_TYPE(Key));
                throw:Throw ->
                    throw(Throw);
                A:B ->
                    ?dump({A, B, erlang:get_stacktrace()}),
                    throw(?ERROR_BAD_DATA(Key))
            end
    end.


check_type(atom, Atom) when is_atom(Atom) ->
    Atom;
check_type(atom, Binary) when is_binary(Binary) ->
    binary_to_existing_atom(Binary, utf8);
check_type(atom, _) ->
    throw(bad_data);
check_type(list_of_atoms, []) ->
    [];
check_type(list_of_atoms, [Atom | _] = Atoms) when is_atom(Atom) ->
    Atoms;
check_type(list_of_atoms, [Binary | _] = Binaries) when is_binary(Binary) ->
    [binary_to_existing_atom(Bin, utf8) || Bin <- Binaries];
check_type(list_of_atoms, _) ->
    throw(bad_data);
check_type(binary, Binary) when is_binary(Binary) ->
    Binary;
check_type(binary, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
check_type(binary, _) ->
    throw(bad_data);
check_type(list_of_binaries, []) ->
    [];
check_type(list_of_binaries, [Binary | _] = Binaries) when is_binary(Binary) ->
    Binaries;
check_type(list_of_binaries, [Atom | _] = Atoms) when is_atom(Atom) ->
    [atom_to_binary(A, utf8) || A <- Atoms];
check_type(list_of_binaries, _) ->
    throw(bad_data);
check_type(integer, Int) when is_integer(Int) ->
    Int;
check_type(integer, _) ->
    throw(bad_data);
check_type(positive_integer, Int) when is_integer(Int) andalso Int > 0 ->
    Int;
check_type(positive_integer, _) ->
    throw(bad_data);
check_type(float, Int) when is_integer(Int) ->
    float(Int);
check_type(float, Float) when is_float(Float) ->
    Float;
check_type(float, _) ->
    throw(bad_data);
check_type(json, JSON) when is_map(JSON) ->
    JSON;
check_type(json, _) ->
    throw(bad_data);
check_type(token, Token) when is_binary(Token) ->
    case token_logic:deserialize(Token) of
        {ok, Macaroon} ->
            Macaroon;
        {error, macaroon_invalid} ->
            throw(bad_token)
    end;
check_type(token, Macaroon) ->
    % Accept everything, it will be validated in check_value
    Macaroon;
check_type(Rule, _) ->
    ?error("Unknown type rule: ~p", [Rule]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).


check_value(_, any, _) ->
    true;
check_value(atom, non_empty, '') ->
    throw(empty);
check_value(list_of_atoms, non_empty, []) ->
    throw(empty);
check_value(binary, non_empty, <<"">>) ->
    throw(empty);
check_value(list_of_binaries, non_empty, []) ->
    throw(empty);
check_value(json, non_empty, Map) when map_size(Map) == 0 ->
    throw(empty);
check_value(_, non_empty, _) ->
    true;
check_value(_, AllowedVals, Vals) when is_list(AllowedVals) andalso is_list(Vals) ->
    [] =:= ordsets:subtract(
        ordsets:from_list(Vals),
        ordsets:from_list(AllowedVals)
    );
check_value(_, AllowedVals, Val) when is_list(AllowedVals) ->
    lists:member(Val, AllowedVals);
check_value(_, VerifyFun, Vals) when is_function(VerifyFun, 1) andalso is_list(Vals) ->
    lists:all(VerifyFun, Vals); %TODO fun -> boolean() | empty
check_value(_, VerifyFun, Val) when is_function(VerifyFun, 1) ->
    VerifyFun(Val); %TODO fun -> boolean() | empty
check_value(_, {exists, VerifyFun}, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            true;
        false ->
            throw(id_not_found)
    end;
check_value(_, {not_exists, VerifyFun}, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            true;
        false ->
            throw(id_occupied)
    end;
check_value(token, TokenType, Macaroon) ->
    case token_logic:validate(Macaroon, TokenType) of
        ok ->
            true;
        inexistent ->
            throw(bad_token);
        bad_macaroon ->
            throw(bad_token);
        bad_type ->
            throw(bad_token_type)
    end;
check_value(_, Rule, _) ->
    ?error("Unknown value rule: ~p", [Rule]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).
