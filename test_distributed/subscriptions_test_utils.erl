%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Utility functions used in subscriptions tests.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_test_utils).
-author("Michał Zmuda").

-include("registered_names.hrl").
-include("subscriptions_test_utils.hrl").
-include("subscriptions/subscriptions.hrl").
-include_lib("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([save/3, update_document/4, delete_document/3, get_rev/3,
    create_provider/3, call_worker/2, generate_group_ids/1, generate_user_ids/1,
    generate_space_ids/1, create_users/3, create_spaces/4, create_groups/4, id/1,
    empty_cache/1, create_provider/4, delete_all/2, list/2, get_last_sequence_number/1]).
-export([expectation/2, public_only_user_expectation/2, group_expectation/10,
    privileges_as_binaries/1, expectation_with_rev/2, public_only_provider_expectation/3]).
-export([verify_messages_present/2, verify_messages_absent/2, init_messages/3,
    flush_messages/2, flush/0, verify_messages/3]).


call_worker(Node, Req) ->
    rpc:call(Node, worker_proxy, call, [?SUBSCRIPTIONS_WORKER_NAME, Req]).

%%%===================================================================
%%% Datastore setup
%%%===================================================================

save(Node, Id, Value) ->
    ?assertMatch({ok, Id}, rpc:call(Node, element(1, Value), save,
        [#document{key = Id, value = Value}])).

delete_all(Node, Documents) ->
    lists:foreach(fun(#document{key = Key, value = Value}) ->
        subscriptions_test_utils:delete_document(Node, element(1, Value), Key)
    end, Documents).

list(Node, Model) ->
    {ok, Documents} = rpc:call(Node, Model, list, []),
    Documents.

update_document(Node, Model, Id, Diff) ->
    ?assertMatch({ok, _}, rpc:call(Node, Model, update, [Id, Diff])).

delete_document(Node, Model, Id) ->
    ?assertMatch(ok, rpc:call(Node, Model, delete, [Id])).

get_rev(Node, Model, Id) ->
    Result = rpc:call(Node, Model, get, [Id]),
    ?assertMatch({ok, _}, Result),
    {ok, #document{rev = Rev}} = Result,
    Rev.

create_provider(Node, Name, Spaces) ->
    create_provider(Node, Name, Spaces, [<<"127.0.0.1">>]).
create_provider(Node, Name, Spaces, URLs) ->
    {_, CSRFile, _} = generate_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    Params = [Name, URLs, <<"https://127.0.0.1:443">>, CSR],
    {ok, Id, _} = rpc:call(Node, provider_logic, create, Params),
    {ok, Id} = rpc:call(Node, od_provider, update, [Id, #{spaces => Spaces}]),
    Id.

generate_space_ids(Number) ->
    generate_ids("s", Number).

generate_group_ids(Number) ->
    generate_ids("g", Number).

generate_user_ids(Number) ->
    generate_ids("u", Number).

generate_ids(Prefix, Number) ->
    [?Id(list_to_atom(Prefix ++ integer_to_list(N))) || N <- lists:seq(1, Number)].

create_spaces(SIds, UIds, GIds, Node) ->
    Groups = [{GId, []} || GId <- GIds],
    Users = [{UId, []} || UId <- UIds],
    lists:map(fun({SId, N}) ->
        Space = #od_space{
            name = list_to_binary("s" ++ integer_to_list(N) ++ integer_to_list(erlang:system_time(micro_seconds))),
            groups = Groups,
            users = Users
        },
        subscriptions_test_utils:save(Node, SId, Space),
        {SId, Space}
    end, lists:zip(SIds, lists:seq(1, length(SIds)))).

create_users(UIds, GIds, Node) ->
    lists:map(fun({UId, N}) ->
        User = #od_user{
            name = list_to_binary("u" ++ integer_to_list(N)),
            groups = GIds
        },
        subscriptions_test_utils:save(Node, UId, User),
        {UId, User}
    end, lists:zip(UIds, lists:seq(1, length(UIds)))).

create_groups(GIds, UIds, SIds, Node) ->
    Users = [{UId, []} || UId <- UIds],
    lists:map(fun({GId, N}) ->
        Group = #od_group{
            name = list_to_binary("g" ++ integer_to_list(N)),
            users = Users,
            spaces = SIds
        },
        subscriptions_test_utils:save(Node, GId, Group),
        {GId, Group}
    end, lists:zip(GIds, lists:seq(1, length(GIds)))).


generate_cert_files() ->
    {MegaSec, Sec, MiliSec} = erlang:timestamp(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.


get_last_sequence_number(Node) ->
    {ok, Start, _} =
        rpc:call(Node, couchdb_datastore_driver, db_run, [couchbeam_changes, follow_once, [], 30]),
    Start.


%%%===================================================================
%%% Message expectations
%%%===================================================================

expectation(Id, #od_space{name = Name, providers_supports = Supports,
    groups = Groups, users = Users, shares = Shares}) ->
    space_expectation(Id, Name, Users, Groups, Supports, Shares);
expectation(Id, #od_share{name = Name, space = Space,
    root_file = RootFileId, public_url = PublicUrl, handle = Handle}) ->
    RootFileBin = undefined_to_binary(RootFileId),
    PublicUrlBin = undefined_to_binary(PublicUrl),
    HandleBin = undefined_to_binary(Handle),
    share_expectation(Id, Name, Space, RootFileBin, PublicUrlBin, HandleBin);
expectation(Id, #od_user{name = Name, space_aliases = SpaceAliases,
    default_space = DefaultSpace, eff_groups = Groups,
    handle_services = HandleServices, handles = Handles}) ->
    user_expectation(Id, Name, maps:to_list(SpaceAliases), Groups,
        undefined_to_binary(DefaultSpace), HandleServices, Handles);
expectation(Id, #od_group{name = Name, type = Type, users = Users, spaces = Spaces,
    eff_users = EUsers, children = Children, parents = Parents,
    handle_services = HandleServices, handles = Handles}) ->
    group_expectation(Id, Name, Type, Users, EUsers, Spaces, Children, Parents,
        HandleServices, Handles);

expectation(Id, #od_provider{client_name = Name, urls = URLs, spaces = SpaceIds}) ->
    [{<<"id">>, Id}, {<<"od_provider">>, [
        {<<"client_name">>, Name},
        {<<"urls">>, URLs},
        {<<"spaces">>, SpaceIds},
        {<<"public_only">>, false}
    ]}];

expectation(Id, #od_handle_service{name = Name, proxy_endpoint = ProxyEndpoint,
    service_properties = ServiceProperties, groups = Groups, users = Users}) ->
    NameBin = undefined_to_binary(Name),
    ProxyEndpointBin = undefined_to_binary(ProxyEndpoint),
    handle_service_expectation(
        Id, NameBin, ProxyEndpointBin, ServiceProperties, Users, Groups
    );

expectation(Id, #od_handle{handle_service = HandleServiceId, public_handle = PublicHandle,
    resource_type = ResourceType, resource_id = ResourceId,
    metadata = Metadata, groups = Groups, users = Users, timestamp = Timestamp}) ->
    HandleServiceIdBin = undefined_to_binary(HandleServiceId),
    PublicHandleBin = undefined_to_binary(PublicHandle),
    ResourceTypeBin = undefined_to_binary(ResourceType),
    ResourceIdBin = undefined_to_binary(ResourceId),
    MetadataBin = undefined_to_binary(Metadata),
    handle_expectation(
        Id, HandleServiceIdBin, PublicHandleBin, ResourceTypeBin,
        ResourceIdBin, MetadataBin, Users, Groups, Timestamp
    ).

user_expectation(Id, Name, Spaces, Groups, DefaultSpace, HandleServices, Handles) ->
    [{<<"id">>, Id}, {<<"od_user">>, [
        {<<"name">>, Name},
        {<<"default_space">>, DefaultSpace},
        {<<"space_aliases">>, Spaces},
        {<<"groups">>, Groups},
        {<<"handle_services">>, HandleServices},
        {<<"handles">>, Handles},
        {<<"public_only">>, false}
    ]}].

public_only_user_expectation(Id, Name) ->
    [{<<"id">>, Id}, {<<"od_user">>, [
        {<<"name">>, Name},
        {<<"default_space">>, <<"undefined">>},
        {<<"space_aliases">>, []},
        {<<"groups">>, []},
        {<<"handle_services">>, []},
        {<<"handles">>, []},
        {<<"public_only">>, true}
    ]}].

group_expectation(Id, Name, Type, Users, EUsers, Spaces, Children, Parents, HandleServices, Handles) ->
    [{<<"id">>, Id}, {<<"od_group">>, [
        {<<"name">>, Name},
        {<<"type">>, atom_to_binary(Type, latin1)},
        {<<"children">>, privileges_as_binaries(Children)},
        {<<"parents">>, Parents},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"eff_users">>, privileges_as_binaries(EUsers)},
        {<<"spaces">>, Spaces},
        {<<"handle_services">>, HandleServices},
        {<<"handles">>, Handles}
    ]}].

space_expectation(Id, Name, Users, Groups, Supports, Shares) ->
    [{<<"id">>, Id}, {<<"od_space">>, [
        {<<"id">>, Id},
        {<<"name">>, Name},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"groups">>, privileges_as_binaries(Groups)},
        {<<"shares">>, Shares},
        {<<"providers_supports">>, Supports}
    ]}].

share_expectation(Id, Name, Space, RootFile, PublicUrl, Handle) ->
    [{<<"id">>, Id}, {<<"od_share">>, [
        {<<"id">>, Id},
        {<<"name">>, Name},
        {<<"public_url">>, PublicUrl},
        {<<"space">>, Space},
        {<<"root_file">>, RootFile},
        {<<"handle">>, Handle}
    ]}].

handle_service_expectation(Id, Name, ProxyEndpoint, ServiceProperties, Users, Groups) ->
    [{<<"id">>, Id}, {<<"od_handle_service">>, [
        {<<"id">>, Id},
        {<<"name">>, Name},
        {<<"proxy_endpoint">>, ProxyEndpoint},
        {<<"service_properties">>, ServiceProperties},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"groups">>, privileges_as_binaries(Groups)}
    ]}].

handle_expectation(Id, HandleServiceId, PublicHandle, ResourceType, ResourceId,
    Metadata, Users, Groups, Timestamp) ->
    [{<<"id">>, Id}, {<<"od_handle">>, [
        {<<"id">>, Id},
        {<<"handle_service">>, HandleServiceId},
        {<<"public_handle">>, PublicHandle},
        {<<"resource_type">>, ResourceType},
        {<<"resource_id">>, ResourceId},
        {<<"metadata">>, Metadata},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"groups">>, privileges_as_binaries(Groups)},
        {<<"timestamp">>, serialize_timestamp(Timestamp)}
    ]}].

public_only_provider_expectation(Id, Name, URLs) ->
    [{<<"id">>, Id}, {<<"od_provider">>, [
        {<<"client_name">>, Name},
        {<<"urls">>, URLs},
        {<<"spaces">>, []},
        {<<"public_only">>, true}
    ]}].

privileges_as_binaries(IdsWithPrivileges) ->
    lists:map(fun({Id, Privileges}) ->
        {Id, lists:map(fun(Privilege) ->
            atom_to_binary(Privilege, latin1)
        end, Privileges)}
    end, IdsWithPrivileges).

expectation_with_rev(Revs, Expectation) ->
    [{<<"revs">>, Revs} | Expectation].


%%%===================================================================
%%% Internal: Message presence/absence verification
%%%===================================================================

verify_messages_present(Context, Expected) ->
    verify_messages_present(Context, Expected, ?MESSAGES_RECEIVE_ATTEMPTS).

verify_messages_present(Context, Expected, AttemptsLimit) ->
    verify_messages(Context, AttemptsLimit, Expected, []).

verify_messages_absent(Context, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, [], Forbidden).

init_messages(Node, ProviderId, Users) ->
    call_worker(Node, {add_connection, ProviderId, self()}),

    Start = case rpc:call(Node, changes_cache, newest_seq, []) of
        {ok, Val} -> Val; _ -> 0
    end,


    #subs_ctx{node = Node, provider = ProviderId,
        users = Users, resume_at = Start, missing = []}.

flush_messages(Context, LastExpected) ->
    UpdatedContext = verify_messages(Context#subs_ctx{
        %% if changes were actually send before init, we could miss last expected
        resume_at = Context#subs_ctx.resume_at - 3
    }, [LastExpected], []),
    flush(),
    UpdatedContext.

flush() ->
    receive _ -> flush() after ?MESSAGES_WAIT_TIMEOUT -> ok end.

verify_messages(Context, Expected, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, Expected, Forbidden).

verify_messages(Context, _, [], []) ->
    Context;
verify_messages(Context, 0, Expected, _) ->
    ?assertMatch(Expected, []),
    Context;
verify_messages(Context, Retries, Expected, Forbidden) ->
    #subs_ctx{node = Node, provider = ProviderId,
        users = Users, resume_at = ResumeAt, missing = Missing} = Context,

    call_worker(Node, {update_users, ProviderId, Users}),
    call_worker(Node, {update_missing_seq, ProviderId, ResumeAt, Missing}),
    All = lists:append(get_messages()),

    ct:print("ALL: ~p~nEXPECTED: ~p~nFORBIdDEN: ~p~n", [All, Expected, Forbidden]),

    Seqs = extract_seqs(All),
    NextResumeAt = largest([ResumeAt | Seqs]),
    NewExpectedSeqs = new_expected_seqs(NextResumeAt, ResumeAt),
    NextMissing = (Missing ++ NewExpectedSeqs) -- Seqs,
    NextContext = Context#subs_ctx{
        resume_at = NextResumeAt,
        missing = NextMissing
    },

    ?assertMatch(Forbidden, remove_matched_expectations(Forbidden, All)),
    RemainingExpected = remove_matched_expectations(Expected, All),
    verify_messages(NextContext, Retries - 1, RemainingExpected, Forbidden).

get_messages() ->
    receive {push, Messages} ->
        [json_utils:decode(Messages)]
    after ?MESSAGES_WAIT_TIMEOUT -> [] end.


new_expected_seqs(NextResumeAt, ResumeAt) ->
    case NextResumeAt > (ResumeAt + 1) of
        true -> lists:seq(ResumeAt + 1, NextResumeAt);
        false -> []
    end.

largest(List) ->
    hd(lists:reverse(lists:usort(List))).

extract_seqs(Messages) ->
    lists:map(fun(Message) ->
        proplists:get_value(<<"seq">>, Message, -2)
    end, Messages).

remove_matched_expectations(Expected, Messages) ->
    lists:filter(fun(Exp) ->
        lists:all(fun(Msg) -> length(Exp -- Msg) =/= 0 end, Messages)
    end, Expected).


id(Id) when is_atom(Id) ->
    ?Id(Id);
id(Id) ->
    ?Id(list_to_atom(Id)).

empty_cache(Node) ->
    subscriptions_test_utils:update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => gb_trees:empty()
    }).

undefined_to_binary(Value) ->
    case Value of
        undefined -> <<"undefined">>;
        Bin when is_binary(Bin) -> Bin
    end.

serialize_timestamp({{A, B, C}, {D, E, F}}) ->
    [A, B, C, D, E, F].
