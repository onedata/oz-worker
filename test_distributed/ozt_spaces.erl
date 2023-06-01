%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating spaces of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_spaces).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([create/0, create/1, create/2, create_advertised/0, create_advertised/1]).
-export([get/1, exists/1]).
-export([update/2]).
-export([list_marketplace/0, list_marketplace/1]).
-export([submit_membership_request/2, submit_membership_request/3]).
-export([try_get_membership_info/2]).
-export([try_submit_membership_request/2, try_submit_membership_request/3]).
-export([resolve_membership_request/3, try_resolve_membership_request/3]).
-export([add_owner/2, remove_owner/2]).
-export([add_user/2, add_user/3]).
-export([remove_user/2]).
-export([add_group/2, add_group/3]).
-export([remove_group/2]).
-export([create_user_invite_token/2]).
-export([create_group_invite_token/2]).
-export([create_support_token/2]).
-export([create_share/2]).
-export([get_users/1, get_groups/1, get_eff_users/1, get_eff_groups/1]).
-export([has_eff_user/2]).
-export([get_user_privileges/2, get_group_privileges/2, get_eff_user_privileges/2, get_eff_group_privileges/2]).
-export([get_shares/1]).
-export([set_user_privileges/3]).
-export([random_support_parameters/0, expected_tweaked_support_parameters/1]).
-export([set_support_parameters/3, get_support_parameters/2]).
-export([delete/1]).
-export([minimum_support_size/0]).
-export([available_space_tags/0]).

-compile({no_auto_import, [get/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec create() -> od_space:id().
create() ->
    create(?UNIQUE_STRING).


-spec create(od_space:name() | entity_logic:data()) -> od_space:id().
create(NameOrData) ->
    create(?ROOT, NameOrData).

-spec create(aai:auth(), od_space:name() | entity_logic:data()) -> od_space:id().
create(Auth, NameOrData) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(space_logic, create, [Auth, NameOrData])),
    SpaceId.


-spec create_advertised() -> od_space:id().
create_advertised() ->
    create_advertised(#{}).

%% @doc Data overrides the default (randomized) values
-spec create_advertised(entity_logic:data()) -> od_space:id().
create_advertised(Data) ->
    create(maps:merge(#{
        <<"name">> => entity_logic_sanitizer:normalize_name(?RAND_UNICODE_STR(), undefined),
        <<"description">> => ?RAND_UNICODE_STR(),
        <<"organizationName">> => ?RAND_UNICODE_STR(),
        <<"tags">> => ?RAND_SUBLIST(available_space_tags()),
        <<"advertisedInMarketplace">> => true,
        <<"marketplaceContactEmail">> => ?RAND_EMAIL_ADDRESS()
    }, Data)).


-spec get(od_space:id()) -> od_space:record().
get(SpaceId) ->
    {ok, Space} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get, [?ROOT, SpaceId])),
    Space.


-spec exists(od_space:id()) -> od_space:record().
exists(SpaceId) ->
    ozt:rpc(space_logic, exists, [SpaceId]).


-spec update(od_space:id(), entity_logic:data()) -> ok.
update(SpaceId, Data) ->
    ?assertEqual(ok, ozt:rpc(space_logic, update, [?ROOT, SpaceId, Data])).


-spec list_marketplace() -> [od_space:id()].
list_marketplace() ->
    list_marketplace(all).

-spec list_marketplace(all | [od_space:tag()]) -> [od_space:id()].
list_marketplace(Tags) ->
    Entries = ozt:rpc(space_marketplace, list, [Tags, #{limit => 10000000000000000, offset => 0}]),
    element(2, lists:unzip(Entries)).


-spec submit_membership_request(od_space:id(), od_user:id()) -> space_membership_requests:request_id().
submit_membership_request(SpaceId, RequesterUserId) ->
    submit_membership_request(SpaceId, RequesterUserId, ?RAND_EMAIL_ADDRESS()).

-spec submit_membership_request(od_space:id(), od_user:id(), od_user:email()) -> space_membership_requests:request_id().
submit_membership_request(SpaceId, RequesterUserId, ContactEmail) ->
    {ok, RequestId} = ?assertMatch({ok, _}, try_submit_membership_request(SpaceId, RequesterUserId, ContactEmail)),
    RequestId.


-spec try_get_membership_info(od_space:id(), space_membership_requests:request_id()) ->
    {ok, entity_logic:data()} | errors:error().
try_get_membership_info(SpaceId, RequestId) ->
    ozt:rpc(space_logic, get_membership_requester_info, [?ROOT, SpaceId, RequestId]).


-spec try_submit_membership_request(od_space:id(), od_user:id()) ->
    {ok, space_membership_requests:request_id()} | errors:error().
try_submit_membership_request(SpaceId, RequesterUserId) ->
    try_submit_membership_request(SpaceId, RequesterUserId, ?RAND_EMAIL_ADDRESS()).

-spec try_submit_membership_request(od_space:id(), od_user:id(), od_user:email()) ->
    {ok, space_membership_requests:request_id()} | errors:error().
try_submit_membership_request(SpaceId, RequesterUserId, ContactEmail) ->
    ozt:rpc(space_logic, submit_membership_request, [?USER(RequesterUserId), SpaceId, #{
        <<"contactEmail">> => ContactEmail
    }]).


-spec resolve_membership_request(
    od_space:id(),
    space_membership_requests:request_id(),
    space_membership_requests:decision()
) ->
    ok.
resolve_membership_request(SpaceId, RequestId, {reject, Reason}) ->
    ?assertEqual(ok, try_resolve_membership_request(SpaceId, RequestId, {reject, Reason}));
resolve_membership_request(SpaceId, RequestId, Decision) ->
    ?assertEqual(ok, try_resolve_membership_request(SpaceId, RequestId, Decision)).


-spec try_resolve_membership_request(
    od_space:id(),
    space_membership_requests:request_id(),
    space_membership_requests:decision()
) -> ok | errors:error().
try_resolve_membership_request(SpaceId, RequestId, {reject, Reason}) ->
    ozt:rpc(space_logic, resolve_membership_request, [?ROOT, SpaceId, RequestId, #{
        <<"decision">> => reject, <<"rejectionReason">> => Reason }]);
try_resolve_membership_request(SpaceId, RequestId, Decision) ->
    ozt:rpc(space_logic, resolve_membership_request, [?ROOT, SpaceId, RequestId, #{
        <<"decision">> => Decision
    }]).


-spec add_owner(od_space:id(), od_user:id()) -> ok.
add_owner(SpaceId, UserId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, add_owner, [?ROOT, SpaceId, UserId])).


-spec remove_owner(od_space:id(), od_user:id()) -> ok.
remove_owner(SpaceId, UserId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, remove_owner, [?ROOT, SpaceId, UserId])).


-spec add_user(od_space:id(), od_user:id()) -> ok.
add_user(SpaceId, UserId) ->
    add_user(SpaceId, UserId, privileges:space_member()).

-spec add_user(od_space:id(), od_user:id(), [privileges:space_privilege()]) -> ok.
add_user(SpaceId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(space_logic, add_user, [?ROOT, SpaceId, UserId, Privileges])),
    ok.


-spec remove_user(od_space:id(), od_user:id()) -> ok.
remove_user(SpaceId, UserId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, remove_user, [?ROOT, SpaceId, UserId])).


-spec add_group(od_space:id(), od_group:id()) -> ok.
add_group(SpaceId, GroupId) ->
    add_group(SpaceId, GroupId, privileges:space_member()).

-spec add_group(od_space:id(), od_group:id(), [privileges:space_privilege()]) -> ok.
add_group(SpaceId, GroupId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(space_logic, add_group, [?ROOT, SpaceId, GroupId, Privileges])),
    ok.


-spec remove_group(od_space:id(), od_group:id()) -> ok.
remove_group(SpaceId, GroupId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, remove_group, [?ROOT, SpaceId, GroupId])).


-spec create_user_invite_token(od_space:id(), od_user:id()) -> tokens:token().
create_user_invite_token(SpaceId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceId)).


-spec create_group_invite_token(od_space:id(), od_user:id()) -> tokens:token().
create_group_invite_token(SpaceId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceId)).


-spec create_support_token(od_space:id(), od_user:id()) -> tokens:token().
create_support_token(SpaceId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId)).


-spec create_share(od_space:id(), od_share:name()) -> od_share:id().
create_share(SpaceId, Name) ->
    {ok, ShareId} = ?assertMatch({ok, _}, ozt:rpc(share_logic, create, [
        ?ROOT, str_utils:rand_hex(16), Name, ?ROOT_FILE_ID, SpaceId
    ])),
    ShareId.


-spec get_users(od_space:id()) -> [od_user:id()].
get_users(SpaceId) ->
    {ok, Users} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_users, [?ROOT, SpaceId])),
    Users.


-spec get_groups(od_space:id()) -> [od_group:id()].
get_groups(SpaceId) ->
    {ok, Groups} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_groups, [?ROOT, SpaceId])),
    Groups.


-spec get_eff_users(od_space:id()) -> [od_user:id()].
get_eff_users(SpaceId) ->
    {ok, Users} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_eff_users, [?ROOT, SpaceId])),
    Users.


-spec get_eff_groups(od_space:id()) -> [od_group:id()].
get_eff_groups(SpaceId) ->
    {ok, Groups} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_eff_groups, [?ROOT, SpaceId])),
    Groups.


-spec has_eff_user(od_space:id(), od_user:id()) -> boolean().
has_eff_user(SpaceId, UserId) ->
    ozt:rpc(space_logic, has_eff_user, [SpaceId, UserId]).


-spec get_user_privileges(od_space:id(), od_user:id()) -> [privileges:space_privilege()].
get_user_privileges(SpaceId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_user_privileges, [?ROOT, SpaceId, UserId])),
    Privs.


-spec get_group_privileges(od_space:id(), od_group:id()) -> [privileges:space_privilege()].
get_group_privileges(SpaceId, GroupId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_group_privileges, [?ROOT, SpaceId, GroupId])),
    Privs.


-spec get_eff_user_privileges(od_space:id(), od_user:id()) -> [privileges:space_privilege()].
get_eff_user_privileges(SpaceId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_eff_user_privileges, [?ROOT, SpaceId, UserId])),
    Privs.


-spec get_eff_group_privileges(od_space:id(), od_group:id()) -> [privileges:space_privilege()].
get_eff_group_privileges(SpaceId, GroupId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_eff_group_privileges, [?ROOT, SpaceId, GroupId])),
    Privs.


-spec get_shares(od_space:id()) -> [od_share:id()].
get_shares(SpaceId) ->
    {ok, Shares} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_shares, [?ROOT, SpaceId])),
    Shares.


-spec set_user_privileges(od_space:id(), od_user:id(), [privileges:space_privilege()]) -> ok.
set_user_privileges(SpaceId, UserId, Privileges) ->
    ?assertMatch(ok, ozt:rpc(space_logic, update_user_privileges, [?ROOT, SpaceId, UserId, #{
        <<"grant">> => Privileges,
        <<"revoke">> => lists_utils:subtract(privileges:space_admin(), Privileges)
    }])).


-spec random_support_parameters() -> support_parameters:record().
random_support_parameters() ->
    RandAccountingEnabled = ?RAND_BOOL(),
    #support_parameters{
        accounting_enabled = RandAccountingEnabled,
        dir_stats_service_enabled = RandAccountingEnabled orelse ?RAND_BOOL(),
        dir_stats_service_status = ?RAND_ELEMENT(support_parameters:all_dir_stats_service_statuses())
    }.


-spec expected_tweaked_support_parameters(support_parameters:record()) -> support_parameters:record().
expected_tweaked_support_parameters(#support_parameters{
    dir_stats_service_enabled = true, dir_stats_service_status = Status
} = SP) when Status =:= disabled; Status =:= stopping ->
    SP#support_parameters{dir_stats_service_status = initializing};
expected_tweaked_support_parameters(#support_parameters{
    dir_stats_service_enabled = false, dir_stats_service_status = Status
} = SP) when Status =:= enabled;  Status =:= initializing ->
    SP#support_parameters{dir_stats_service_status = stopping};
expected_tweaked_support_parameters(SP) ->
    SP.


-spec set_support_parameters(od_space:id(), od_provider:id(), support_parameters:record()) -> ok.
set_support_parameters(SpaceId, ProviderId, SupportParameters) ->
    ?assertMatch(ok, ozt:rpc(space_logic, update_support_parameters, [
        ?ROOT, SpaceId, ProviderId, jsonable_record:to_json(SupportParameters, support_parameters)
    ])).


-spec get_support_parameters(od_space:id(), od_provider:id()) -> support_parameters:record().
get_support_parameters(SpaceId, ProviderId) ->
    #od_space{support_parameters_registry = SupportParametersRegistry} = get(SpaceId),
    support_parameters_registry:get_entry(ProviderId, SupportParametersRegistry).


-spec delete(od_space:id()) -> ok.
delete(SpaceId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, delete, [?ROOT, SpaceId])).


-spec minimum_support_size() -> od_space:support_size().
minimum_support_size() ->
    ozt:get_env(minimum_space_support_size).


-spec available_space_tags() -> [od_space:tag()].
available_space_tags() ->
    lists:flatten(maps:values(ozt:get_env(available_space_tags))).
