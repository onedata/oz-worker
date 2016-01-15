%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Tests for the space_logic module.
%%%-------------------------------------------------------------------
-module(space_logic_tests).
-author("Konrad Zemek").

-ifdef(TEST).

-include("datastore/gr_datastore_models_def.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests functions
%%%===================================================================

has_effective_privilege_test() ->
    ok = meck:new(space),
    ok = meck:new(user_group),
    ok = meck:new(onedata_user),

    GroupId = <<"GroupId">>,
    SpaceId = <<"SpaceId">>,
    UserId = <<"UserId">>,
    User2Id = <<"User2Id">>,

    Space = #space{name = <<"Space">>, groups = [{GroupId, [space_view_data]}], users = [{UserId, [space_invite_user]}]},
    Group = #user_group{name = <<"Group">>, spaces = [SpaceId], users = [{UserId, privileges:group_admin()}, {User2Id, privileges:group_admin()}]},
    User = #onedata_user{name = <<"User">>, groups = [GroupId], spaces = [SpaceId]},
    User2 = #onedata_user{name = <<"User2">>, groups = [GroupId]},
    SpaceDoc = #document{key = SpaceId, value = Space},
    GroupDoc = #document{key = GroupId, value = Group},
    UserDoc = #document{key = UserId, value = User},
    User2Doc = #document{key = User2Id, value = User2},

    ok = meck:expect(space, exists, fun(_) -> true end),
    ok = meck:expect(user_group, exists, fun(_) -> true end),
    ok = meck:expect(onedata_user, exists, fun(_) -> true end),
    ok = meck:expect(space, get, fun(_) -> {ok, SpaceDoc} end),
    ok = meck:expect(user_group, get, fun(_) -> {ok, GroupDoc} end),
    ok = meck:expect(onedata_user, get,
        fun
            (UserId) -> {ok, UserDoc};
            (User2Id) -> {ok, User2Doc}
        end
    ),

    ?assert(space_logic:has_effective_privilege(SpaceId, UserId, space_view_data)),
    ?assert(space_logic:has_effective_privilege(SpaceId, UserId, space_invite_user)),
    ?assertNot(space_logic:has_effective_privilege(SpaceId, UserId, space_change_data)),

    ?assert(space_logic:has_effective_privilege(SpaceId, User2Id, space_view_data)),
    ?assertNot(space_logic:has_effective_privilege(SpaceId, User2Id, space_invite_user)),

    meck:unload().

-endif.
