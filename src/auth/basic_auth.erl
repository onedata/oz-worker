%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides an API for user authentication with Basic Auth
%%% (username & password).
%%% @end
%%%-------------------------------------------------------------------
-module(basic_auth).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

-type password() :: binary().
-type password_hash() :: binary().

-export_type([password/0, password_hash/0]).

%% API
-export([check_basic_auth/1, check_basic_auth/2]).
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
%% Tries to a authorize a client by username and password.
%%      {true, Auth} - the client was authorized
%%      false - credentials were not found
%%      errors:error() - provided credentials are invalid
%% @end
%%--------------------------------------------------------------------
-spec check_basic_auth(cowboy_req:req()) ->
    {true, aai:auth()} | false | errors:error().
check_basic_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req, undefined) of
        <<"Basic ", UserPasswdB64/binary>> ->
            try
                UsernamePassword = base64:decode(UserPasswdB64),
                [Username, Password] = binary:split(UsernamePassword, <<":">>),
                check_basic_auth(Username, Password)
            catch _:_ ->
                ?ERROR_BAD_BASIC_CREDENTIALS
            end;
        _ ->
            false
    end.

-spec check_basic_auth(od_user:username(), password()) ->
    {true, aai:auth()} | errors:error().
check_basic_auth(Username, Password) ->
    case auth_config:is_basic_auth_enabled() of
        false ->
            ?ERROR_BASIC_AUTH_NOT_SUPPORTED;
        true ->
            case od_user:get_by_username(Username) of
                {ok, #document{value = #od_user{basic_auth_enabled = false}}} ->
                    ?ERROR_BASIC_AUTH_DISABLED;
                {ok, #document{value = #od_user{password_hash = undefined}}} ->
                    ?ERROR_BASIC_AUTH_DISABLED;
                {ok, #document{value = #od_user{password_hash = Hash}, key = UserId}} ->
                    case onedata_passwords:verify(Password, Hash) of
                        true -> {true, ?USER(UserId)};
                        false -> ?ERROR_BAD_BASIC_CREDENTIALS
                    end;
                {error, not_found} ->
                    ?ERROR_BAD_BASIC_CREDENTIALS
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Enables/disables basic auth (ability to sign in by username & password)
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
    {ok, od_user:record()} | errors:error().
change_password(#od_user{basic_auth_enabled = false} = _User, _OldPass, _NewPass) ->
    ?ERROR_BASIC_AUTH_DISABLED;
change_password(#od_user{password_hash = undefined} = User, undefined, NewPass) ->
    {ok, User#od_user{password_hash = onedata_passwords:create_hash(NewPass)}};
change_password(#od_user{password_hash = undefined} = _User, _OldPass, _NewPass) ->
    ?ERROR_BAD_BASIC_CREDENTIALS;
change_password(#od_user{password_hash = _Hash} = _User, undefined, _NewPass) ->
    ?ERROR_BAD_BASIC_CREDENTIALS;
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
    {ok, od_user:record()} | errors:error().
set_password(#od_user{basic_auth_enabled = false}, _) ->
    ?ERROR_BASIC_AUTH_DISABLED;
set_password(User, NewPass) ->
    {ok, User#od_user{password_hash = onedata_passwords:create_hash(NewPass)}}.


%%--------------------------------------------------------------------
%% @doc
%% Creates a user account in Onezone based on OZ panel user data - dedicated for
%% migration between versions 18.02 and 19.02.
%% Makes sure username is not occupied, or nullifies it for the conflicting user
%% (onepanel users have higher priority during upgrade).
%% @end
%%--------------------------------------------------------------------
-spec migrate_onepanel_user_to_onezone(OnepanelUserId :: binary(),
    OnepanelUsername :: binary(), password_hash(), Role :: regular | admin) ->
    {ok, od_user:id()}.
migrate_onepanel_user_to_onezone(OnepanelUserId, OnepanelUsername, PasswordHash, Role) ->
    UserId = onepanel_uid_to_system_uid(OnepanelUserId),

    case od_user:get_by_username(OnepanelUsername) of
        {ok, #document{key = UserId}} ->
            ok;
        {ok, #document{key = ConflictingUser}} ->
            % Nullify username (previously alias) of conflicting users
            {ok, _} = od_user:update(ConflictingUser, fun(User) ->
                {ok, User#od_user{username = undefined}}
            end);
        _ ->
            ok
    end,

    UpdateFun = fun(User) ->
        {ok, User#od_user{
            username = OnepanelUsername,
            basic_auth_enabled = true,
            password_hash = PasswordHash
        }}
    end,
    DefaultDoc = #document{key = UserId, value = #od_user{
        full_name = user_logic:normalize_full_name(OnepanelUsername),
        username = OnepanelUsername,
        basic_auth_enabled = true,
        password_hash = PasswordHash
    }},
    {ok, _} = od_user:update(UserId, UpdateFun, DefaultDoc),

    Groups = onepanel_role_to_groups(Role),
    add_user_to_groups(UserId, Groups),
    maybe_make_cluster_admin(UserId, Role),

    {ok, UserId}.


%%--------------------------------------------------------------------
%% @doc
%% Constructs user id based on user id from onepanel.
%% @end
%%--------------------------------------------------------------------
-spec onepanel_uid_to_system_uid(OnepanelUserId :: binary()) -> od_user:id().
onepanel_uid_to_system_uid(OnepanelUserId) ->
    linked_accounts:gen_user_id(?ONEZONE_IDP_ID, OnepanelUserId).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines groups based on Onepanel user role for migration purposes.
%% @end
%%--------------------------------------------------------------------
-spec onepanel_role_to_groups(Role :: admin | regular | binary()) ->
    [od_group:id()].
onepanel_role_to_groups(Role) when is_atom(Role) ->
    onepanel_role_to_groups(atom_to_binary(Role, utf8));

onepanel_role_to_groups(Role) when is_binary(Role) ->
    Mapping = oz_worker:get_env(onepanel_role_to_group_mapping, #{}),
    maps:get(Role, Mapping, []).


%% @private
-spec add_user_to_groups(UserId :: od_user:id(), GroupIds :: [od_group:id()]) ->
    ok.
add_user_to_groups(UserId, GroupIds) ->
    lists:foreach(fun(GroupId) ->
        case group_logic:add_user(?ROOT, GroupId, UserId) of
            {ok, UserId} ->
                {ok, GroupName} = group_logic:get_name(?ROOT, GroupId),
                ?info("Added user '~s' to group '~ts'", [UserId, GroupName]);
            ?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _) ->
                ok
        end
    end, GroupIds).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If given user is a migrated Onepanel admin adds the user as
%% Onezone cluster member with admin privileges.
%% @end
%%--------------------------------------------------------------------
-spec maybe_make_cluster_admin(od_user:id(), Role :: admin | regular) ->
    ok | errors:error().
maybe_make_cluster_admin(UserId, admin) ->
    case cluster_logic:add_user(
        ?ROOT, ?ONEZONE_CLUSTER_ID, UserId, privileges:cluster_admin()
    ) of
        {ok, _} ->
            ?info("Added user '~s' as admin of cluster '~s'", [UserId, ?ONEZONE_CLUSTER_ID]),
            ok;
        ?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _) ->
            ok;
        Error ->
            Error
    end;

maybe_make_cluster_admin(_UserId, _Role) ->
    ok.
