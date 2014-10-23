%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Tests for the space_logic module.
%% ===================================================================
-module(space_logic_tests).
-author("Konrad Zemek").

-ifdef(TEST).

-include("dao/dao_types.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests functions
%% ===================================================================

has_effective_privilege_test() ->
    ok = meck:new(dao_lib),

    GroupId = <<"GroupId">>,
    SpaceId = <<"SpaceId">>,
    UserId  = <<"UserId">>,
    User2Id = <<"User2Id">>,

    Space = #space{name = <<"Space">>, groups = [{GroupId, [space_view_data]}], users = [{UserId, [space_invite_user]}]},
    Group = #user_group{name = <<"Group">>, spaces = [SpaceId], users = [{UserId, privileges:group_admin()}, {User2Id, privileges:group_admin()}]},
    User  = #user{name = <<"User">>, groups = [GroupId], spaces = [SpaceId]},
    User2 = #user{name = <<"User2">>, groups = [GroupId]},
    SpaceDoc = #db_document{uuid = SpaceId, record = Space},
    GroupDoc = #db_document{uuid = GroupId, record = Group},
    UserDoc  = #db_document{uuid = UserId,  record = User},
    User2Doc = #db_document{uuid = User2Id, record = User2},

    ok = meck:expect(dao_lib, apply, fun
        (dao_spaces, exist_space, _, _) -> {ok, true};
        (dao_groups, exist_group, _, _) -> {ok, true};
        (dao_users,  exist_user,  _, _) -> {ok, true};
        (dao_spaces, get_space,   _, _) -> {ok, SpaceDoc};
        (dao_groups, get_group,   _, _) -> {ok, GroupDoc};
        (dao_users,  get_user,    ["UserId"|_], _)  -> {ok, UserDoc};
        (dao_users,  get_user,    ["User2Id"|_], _) -> {ok, User2Doc}
    end),

    ?assert(space_logic:has_effective_privilege(SpaceId, UserId, space_view_data)),
    ?assert(space_logic:has_effective_privilege(SpaceId, UserId, space_invite_user)),
    ?assertNot(space_logic:has_effective_privilege(SpaceId, UserId, space_change_data)),

    ?assert(space_logic:has_effective_privilege(SpaceId, User2Id, space_view_data)),
    ?assertNot(space_logic:has_effective_privilege(SpaceId, User2Id, space_invite_user)),

    meck:unload().

-endif.
