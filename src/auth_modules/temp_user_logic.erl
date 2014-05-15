%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module implements auth_module_behaviour and handles singning in
%% via Github.
%% @end
%% ===================================================================
-module(temp_user_logic).

-include("logging.hrl").
-include("auth_common.hrl").

-define(USER_LOGIC_ETS, user_logic_ets).

%% API
-export([test/0, init/0, save_user/1, get_user/1, update_user/2]).


test() ->
    UserInfo = #user_info{emails = ["a", "b"], preferred_login = "a", preferred_name = "a", provider_infos = [
        #provider_user_info{provider_id = github, user_id = "a", email = "a", login = "a", name = "a"},
        #provider_user_info{provider_id = facebook, user_id = "b", email = "b", login = "b", name = "b"}
    ]},

    save_user(UserInfo),
    ?dump(get_user({github, "a"})),

    UserInfo2 = #user_info{emails = ["c", "d"], preferred_login = "c", preferred_name = "c", provider_infos = [
        #provider_user_info{provider_id = github, user_id = "c", email = "c", login = "c", name = "c"},
        #provider_user_info{provider_id = facebook, user_id = "d", email = "d", login = "d", name = "d"}
    ]},

    update_user({github, "a"}, UserInfo2),
    ?dump(get_user({github, "a"})),
    ?dump(get_user({github, "c"})),

    ok.


init() ->
    catch ets:delete(?USER_LOGIC_ETS),
    ets:new(?USER_LOGIC_ETS, [named_table, public, bag, {read_concurrency, true}]),
    ets:insert(?USER_LOGIC_ETS, {users, []}),
    ok.


save_user(User = #user_info{}) ->
    save_all_users([User | get_all_users()]).


get_user({Provider, ID}) ->
    LookupProvider =
        fun(#provider_user_info{provider_id = ProviderID, user_id = UserID}, Acc) ->
            Res = case {Provider, ID} of
                      {ProviderID, UserID} -> true;
                      _ -> false
                  end,
            case Acc of
                true -> true;
                _ -> Res
            end
        end,

    LookupUser =
        fun(User = #user_info{provider_infos = ProviderInfos}, Acc) ->
            case lists:foldl(LookupProvider, false, ProviderInfos) of
                true -> User;
                _ -> Acc
            end
        end,

    lists:foldl(LookupUser, undefined, get_all_users()).


update_user({Provider, ID}, NewUserInfo) ->
    LookupProvider =
        fun(#provider_user_info{provider_id = ProviderID, user_id = UserID}, Acc) ->
            Res = case {Provider, ID} of
                      {ProviderID, UserID} -> true;
                      _ -> false
                  end,
            case Acc of
                true -> true;
                _ -> Res
            end
        end,

    UpdateUser =
        fun(User = #user_info{provider_infos = ProviderInfos}) ->
            case lists:foldl(LookupProvider, false, ProviderInfos) of
                true -> NewUserInfo;
                _ -> User
            end
        end,

    save_all_users(lists:map(UpdateUser, get_all_users())).


get_all_users() ->
    [{users, Users}] = ets:lookup(?USER_LOGIC_ETS, users),
    Users.


save_all_users(Users) ->
    ets:delete_all_objects(?USER_LOGIC_ETS),
    ets:insert(?USER_LOGIC_ETS, {users, Users}).
