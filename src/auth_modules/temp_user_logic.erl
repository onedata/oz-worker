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
-export([create_association/2, get_association/1, get_user_to_json/1]).


test() ->
    UserInfo = #user_info{global_id = "ab", emails = ["a", "b"], preferred_name = "a", provider_infos = [
        #provider_user_info{provider_id = github, user_id = "a", emails = "a", login = "a", name = "a"},
        #provider_user_info{provider_id = facebook, user_id = "b", emails = "b", login = "b", name = "b"}
    ]},

    save_user(UserInfo),
    ?dump(get_user({github, "a"})),
    ?dump(get_user({global, "ab"})),

    UserInfo2 = #user_info{global_id = "cd", emails = ["c", "d"], preferred_name = "c", provider_infos = [
        #provider_user_info{provider_id = github, user_id = "c", emails = "c", login = "c", name = "c"},
        #provider_user_info{provider_id = facebook, user_id = "d", emails = "d", login = "d", name = "d"}
    ]},

    update_user({global, "ab"}, UserInfo2),
    ?dump(get_user({github, "a"})),
    ?dump(get_user({github, "c"})),
    ?dump(get_user({global, "ab"})),
    ?dump(get_user({global, "cd"})),

    ok.


init() ->
    catch ets:delete(?USER_LOGIC_ETS),
    ets:new(?USER_LOGIC_ETS, [named_table, public, bag, {read_concurrency, true}]),
    ets:insert(?USER_LOGIC_ETS, {users, []}),
    ets:insert(?USER_LOGIC_ETS, {associations, []}),
    ok.


save_user(User = #user_info{}) ->
    save_all_users([User | get_all_users()]).


get_user({email, Email}) ->
    LookupProvider =
        fun(#provider_user_info{emails = Emails}, Acc) ->
            case Acc of
                true -> true;
                _ -> lists:member(Email, Emails)
            end
        end,

    LookupUser =
        fun(User = #user_info{provider_infos = ProviderInfos, emails = Emails}, Acc) ->
            case lists:member(Email, Emails) of
                true -> User;
                _ ->
                    case lists:foldl(LookupProvider, false, ProviderInfos) of
                        true -> User;
                        _ -> Acc
                    end
            end
        end,

    lists:foldl(LookupUser, undefined, get_all_users());


get_user({global, ID}) ->
    LookupUser =
        fun(User = #user_info{global_id = GlobalID}, Acc) ->
            case ID of
                GlobalID -> User;
                _ -> Acc
            end
        end,

    lists:foldl(LookupUser, undefined, get_all_users());


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


update_user({global, ID}, NewUserInfo) ->
    UpdateUser =
        fun(User = #user_info{global_id = GlobalID}) ->
            case ID of
                GlobalID -> NewUserInfo;
                _ -> User
            end
        end,

    save_all_users(lists:map(UpdateUser, get_all_users()));


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
    ets:delete_object(?USER_LOGIC_ETS, {users, get_all_users()}),
    ets:insert(?USER_LOGIC_ETS, {users, Users}).


create_association(AuthorizationCode, UserGlobalID) ->
    save_all_associations([{AuthorizationCode, UserGlobalID} | get_all_associations()]).


get_association(AuthorizationCode) ->
    Associations = get_all_associations(),
    UserID = proplists:get_value(AuthorizationCode, Associations, undefined),
    save_all_associations(proplists:delete(AuthorizationCode, Associations)),
    UserID.


get_all_associations() ->
    [{associations, Associations}] = ets:lookup(?USER_LOGIC_ETS, associations),
    Associations.


save_all_associations(Associations) ->
    ets:delete_object(?USER_LOGIC_ETS, {associations, get_all_associations()}),
    ets:insert(?USER_LOGIC_ETS, {associations, Associations}).


get_user_to_json(UserID) ->
    try
        #user_info{
            global_id = GlobalID,
            preferred_name = PrefName,
            emails = Emails,
            provider_infos = ProvInfo} = get_user({global, UserID}),
        UserStruct = [
            {global_id, GlobalID},
            {preffered_name, PrefName},
            {emails, Emails}
        ],
        JSON = mochijson2:encode(UserStruct)
    catch
        _:_ -> <<"error">>
    end.