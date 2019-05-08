%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides an API for user authentication with Basic Auth
%%% (login & password).
%%% @end
%%%-------------------------------------------------------------------
-module(basic_auth).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

-type password() :: binary().
-type password_hash() :: binary().

-export_type([password/0, password_hash/0]).

%% API
-export([authenticate/2]).
-export([toggle_basic_auth/2]).
-export([change_password/3, set_password/2]).
-export([migrate_onepanel_user_to_onezone/4]).
-export([onepanel_uid_to_system_uid/1]).

% (Artificial) identity provider id used for creating user ids for users
% coming from onepanel.
-define(ONEZONE_IDP_ID, onezone).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Authenticates a user by basic credentials (login & password).
%% @end
%%--------------------------------------------------------------------
-spec authenticate(od_user:alias(), password()) -> {ok, od_user:id()} | {error, term()}.
authenticate(Alias, Password) ->
    case auth_config:is_onepanel_auth_enabled() of
        false ->
            ?ERROR_BASIC_AUTH_NOT_SUPPORTED;
        true ->
            case od_user:get_by_alias(Alias) of
                {ok, #document{value = #od_user{basic_auth_enabled = false}}} ->
                    ?ERROR_BASIC_AUTH_DISABLED;
                {ok, #document{value = #od_user{password_hash = undefined}}} ->
                    ?ERROR_BASIC_AUTH_DISABLED;
                {ok, #document{value = #od_user{password_hash = Hash}, key = UserId}} ->
                    case onedata_passwords:verify(Password, Hash) of
                        true ->
                            {ok, UserId};
                        false ->
                            ?ERROR_BAD_BASIC_CREDENTIALS
                    end;
                {error, not_found} ->
                    ?ERROR_BAD_BASIC_CREDENTIALS
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Enables/disables basic auth (ability to sign in by login & password)
%% in given user record.
%% @end
%%--------------------------------------------------------------------
-spec toggle_basic_auth(od_user:record(), boolean()) -> {ok, od_user:record()}.
toggle_basic_auth(UserRecord, Flag) ->
    {ok, UserRecord#od_user{basic_auth_enabled = Flag}}.


%%--------------------------------------------------------------------
%% @doc
%% Changes the password (the old one is required) in given user record.
%% @end
%%--------------------------------------------------------------------
-spec change_password(od_user:record(), OldPass :: password(), NewPass :: password()) ->
    {ok, od_user:record()} | {error, term()}.
change_password(#od_user{basic_auth_enabled = false}, _, _) ->
    ?ERROR_BASIC_AUTH_DISABLED;
change_password(#od_user{password_hash = undefined} = User, OldPass, NewPass) ->
    case OldPass of
        undefined ->
            {ok, User#od_user{password_hash = onedata_passwords:create_hash(NewPass)}};
        _ ->
            ?ERROR_BAD_BASIC_CREDENTIALS
    end;
change_password(#od_user{password_hash = Hash} = User, OldPass, NewPass) ->
    case onedata_passwords:verify(OldPass, Hash) of
        true ->
            {ok, User#od_user{password_hash = onedata_passwords:create_hash(NewPass)}};
        false ->
            ?ERROR_BAD_BASIC_CREDENTIALS
    end.


%%--------------------------------------------------------------------
%% @doc
%% Sets (overwrites) the password in given user record - dedicated for admins.
%% @end
%%--------------------------------------------------------------------
-spec set_password(od_user:record(), NewPass :: password()) ->
    {ok, od_user:record()} | {error, term()}.
set_password(#od_user{basic_auth_enabled = false}, _) ->
    ?ERROR_BASIC_AUTH_DISABLED;
set_password(User, NewPass) ->
    {ok, User#od_user{password_hash = onedata_passwords:create_hash(NewPass)}}.


%%--------------------------------------------------------------------
%% @doc
%% Creates a user account in Onezone based on OZ panel user data - dedicated for
%% migration between versions 18.02 and 19.02.
%% @end
%%--------------------------------------------------------------------
-spec migrate_onepanel_user_to_onezone(OnepanelUsername :: binary(),
    password_hash(), [od_group:id()], regular | admin) -> {ok, od_user:id()}.
migrate_onepanel_user_to_onezone(OnepanelUsername, PasswordHash, Groups, admin) ->
    {ok, UserId} = migrate_onepanel_user_to_onezone(OnepanelUsername, PasswordHash, Groups, regular),
    ok = make_cluster_admin(UserId),
    {ok, UserId};
migrate_onepanel_user_to_onezone(OnepanelUsername, PasswordHash, Groups, regular) ->
    UserId = onepanel_uid_to_system_uid(OnepanelUsername),
    UpdateFun = fun(User) ->
        {ok, User#od_user{
            alias = OnepanelUsername,
            basic_auth_enabled = true,
            password_hash = PasswordHash
        }}
    end,
    DefaultDoc = #document{key = UserId, value = #od_user{
        name = user_logic:normalize_name(OnepanelUsername),
        alias = OnepanelUsername,
        basic_auth_enabled = true,
        password_hash = PasswordHash
    }},
    {ok, _} = od_user:update(UserId, UpdateFun, DefaultDoc),

    lists:foreach(fun(GroupId) ->
        case group_logic:add_user(?ROOT, GroupId, UserId) of
            {ok, UserId} ->
                {ok, GroupName} = group_logic:get_name(?ROOT, GroupId),
                ?info("Added user '~s' to group '~ts'", [UserId, GroupName]);
            ?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _) ->
                ok
        end
    end, Groups),

    {ok, UserId}.


%%--------------------------------------------------------------------
%% @doc
%% Constructs user id based on user id from onepanel.
%% @end
%%--------------------------------------------------------------------
-spec onepanel_uid_to_system_uid(OnepanelUserId :: binary()) -> od_user:id().
onepanel_uid_to_system_uid(OnepanelUserId) ->
    linked_accounts:idp_uid_to_system_uid(?ONEZONE_IDP_ID, OnepanelUserId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds user with given Id as a member of the Onezone cluster with
%% admin privileges.
%% @end
%%--------------------------------------------------------------------
-spec make_cluster_admin(od_user:id()) -> ok | {error, term()}.
make_cluster_admin(UserId) ->
    case cluster_logic:add_user(?ROOT, ?ONEZONE_CLUSTER_ID, UserId, privileges:cluster_admin()) of
        {ok, _} ->
            ?info("Added user '~s' as admin of cluster '~s'", [UserId, ?ONEZONE_CLUSTER_ID]),
            ok;
        ?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _) ->
            ok;
        Error ->
            Error
    end.
