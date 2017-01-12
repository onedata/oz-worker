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
-module(n_user_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include("errors.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_user_logic_plugin).

-export([
    create/1, create/2,
    create_client_token/2,
    authorize/3
]).
-export([
    get/2,
    get_data/2,
    list/1,
    get_oz_privileges/2, get_eff_oz_privileges/2,
    list_client_tokens/2,
    get_default_space/2,
    get_space_alias/3,
    get_default_provider/2
]).
-export([
    update_name/3, update_alias/3, update/3,
    update_oz_privileges/4, update_oz_privileges/3,
    set_default_space/3,
    set_space_alias/4,
    set_default_provider/3]).
-export([
    delete/2,
    delete_oz_privileges/2,
    delete_client_token/3,
    unset_default_space/2,
    delete_space_alias/3,
    unset_default_provider/2
]).
-export([
    join_group/3,
    join_space/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,

    get_spaces/2, get_eff_spaces/2,
    get_space/3, get_eff_space/3,

    get_eff_providers/2, get_eff_provider/3,

    get_handle_services/2, get_eff_handle_services/2,
    get_handle_service/3, get_eff_handle_service/3,

    get_handles/2, get_eff_handles/2,
    get_handle/3, get_eff_handle/3,

    leave_group/3,
    leave_space/3,
    leave_handle_service/3,
    leave_handle/3
]).
-export([
    exists/1,
    has_eff_oz_privilege/2,
    has_eff_space/2,
    has_eff_provider/2
]).
-export([
    add_oauth_account/2,
    is_email_occupied/2,
    authenticate_by_basic_credentials/2,
    change_user_password/3
]).


create(UserInfo) ->
    create(UserInfo, undefined).

create(UserInfo, ProposedUserId) ->
    case od_user:create(#document{key = ProposedUserId, value = UserInfo}) of
        {error, already_exists} ->
            ?ERROR_BAD_VALUE_ID_OCCUPIED(<<"userId">>);
        {ok, UserId} ->
            setup_user(UserId, UserInfo),
            {ok, UserId}
    end.


create_client_token(Client, UserId) ->
    n_entity_logic:create(Client, ?PLUGIN, UserId, client_tokens, #{}).


authorize(Client, UserId, Identifier) when is_binary(Identifier) ->
    authorize(Client, UserId, #{<<"identifier">> => Identifier});
authorize(Client, UserId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, UserId, authorize, Data).


get(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, entity).


get_data(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, data).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


get_oz_privileges(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, oz_privileges).


get_eff_oz_privileges(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_oz_privileges).


get_default_space(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, default_space).


get_space_alias(Client, UserId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {space_alias, SpaceId}).


get_default_provider(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, default_provider).


list_client_tokens(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, client_tokens).


update_oz_privileges(Client, UserId, Operation, Privs) when is_list(Privs) ->
    update_oz_privileges(Client, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_oz_privileges(Client, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, UserId, oz_privileges, Data).


set_default_space(Client, UserId, SpaceId) when is_binary(SpaceId) ->
    set_default_space(Client, UserId, #{<<"spaceId">> => SpaceId});
set_default_space(Client, UserId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, UserId, default_space, Data).


set_space_alias(Client, UserId, SpaceId, Alias) when is_binary(Alias) ->
    set_space_alias(Client, UserId, SpaceId, #{<<"alias">> => Alias});
set_space_alias(Client, UserId, SpaceId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, UserId, {space_alias, SpaceId}, Data).


set_default_provider(Client, UserId, ProviderId) when is_binary(ProviderId) ->
    set_default_provider(Client, UserId, #{<<"providerId">> => ProviderId});
set_default_provider(Client, UserId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, UserId, default_provider, Data).


update_name(Client, UserId, NewName) ->
    update(Client, UserId, #{<<"name">> => NewName}).


update_alias(Client, UserId, NewAlias) ->
    update(Client, UserId, #{<<"alias">> => NewAlias}).


update(Client, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, UserId, entity, Data).


delete(Client, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, entity).


delete_oz_privileges(Client, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, oz_privileges).


delete_client_token(Client, UserId, TokenId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {client_token, TokenId}).


unset_default_space(Client, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, default_space).


delete_space_alias(Client, UserId, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {space_alias, SpaceId}).


unset_default_provider(Client, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, default_provider).


join_group(Client, UserId, Data) when is_map(Data) ->
    n_entity_logic:create(Client, ?PLUGIN, UserId, join_group, Data);
join_group(Client, UserId, Token) ->
    join_group(Client, UserId, #{<<"token">> => Token}).


join_space(Client, UserId, Data) when is_map(Data) ->
    n_entity_logic:create(Client, ?PLUGIN, UserId, join_space, Data);
join_space(Client, UserId, Token) ->
    join_space(Client, UserId, #{<<"token">> => Token}).


get_groups(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, groups).


get_eff_groups(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_groups).


get_group(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {group, GroupId}).


get_eff_group(Client, UserId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_group, GroupId}).


get_spaces(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, spaces).


get_eff_spaces(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_spaces).


get_space(Client, UserId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {space, SpaceId}).


get_eff_space(Client, UserId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_space, SpaceId}).


get_eff_providers(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_providers).


get_eff_provider(Client, UserId, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_provider, ProviderId}).


get_handle_services(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, handle_services).


get_eff_handle_services(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_handle_services).


get_handle_service(Client, UserId, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {handle_service, HServiceId}).


get_eff_handle_service(Client, UserId, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_handle_service, HServiceId}).


get_handles(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, handles).


get_eff_handles(Client, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, eff_handles).


get_handle(Client, UserId, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {handle, HandleId}).


get_eff_handle(Client, UserId, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, UserId, {eff_handle, HandleId}).


leave_group(Client, UserId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {groups, GroupId}).


leave_space(Client, UserId, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {spaces, SpaceId}).


leave_handle_service(Client, UserId, HServiceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {handle_services, HServiceId}).


leave_handle(Client, UserId, HandleId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {handles, HandleId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a user exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(UserId :: od_user:id()) -> boolean().
exists(UserId) ->
    od_user:exists(UserId).


has_eff_oz_privilege(UserId, Privilege) when is_binary(UserId) ->
    case od_user:get(UserId) of
        {ok, #document{value = User}} ->
            has_eff_oz_privilege(User, Privilege);
        _ ->
            false
    end;
has_eff_oz_privilege(#od_user{eff_oz_privileges = UserPrivileges}, Privilege) ->
    lists:member(Privilege, UserPrivileges).


has_eff_space(UserId, SpaceId) when is_binary(UserId) ->
    case od_user:get(UserId) of
        {ok, #document{value = User}} ->
            has_eff_space(User, SpaceId);
        _ ->
            false
    end;
has_eff_space(#od_user{eff_spaces = EffSpaces}, SpaceId) ->
    maps:is_key(SpaceId, EffSpaces).


has_eff_provider(UserId, ProviderId) when is_binary(UserId) ->
    case od_user:get(UserId) of
        {ok, #document{value = User}} ->
            has_eff_provider(User, ProviderId);
        _ ->
            false
    end;
has_eff_provider(#od_user{eff_providers = EffProviders}, ProviderId) ->
    maps:is_key(ProviderId, EffProviders).


%%--------------------------------------------------------------------
%% @doc
%% Adds an oauth account to user's accounts.
%% @end
%%--------------------------------------------------------------------
-spec add_oauth_account(UserId :: od_user:id(),
    OAuthAccount :: #oauth_account{}) -> ok.
add_oauth_account(UserId, OAuthAccount) ->
    {ok, #document{
        value = #od_user{
            name = Name,
            email_list = Emails,
            connected_accounts = ConnectedAccounts
        }}} = od_user:get(UserId),
    #oauth_account{
        name = OAuthName,
        email_list = OAuthEmails
    } = OAuthAccount,
    % If no name is specified, take the one provided with new info
    NewName = case Name of
        <<"">> -> OAuthName;
        _ -> Name
    end,
    {ok, _} = od_user:update(UserId, #{
        name => NewName,
        % Add emails from provider that are not yet added to account
        email_list => lists:usort(Emails ++ OAuthEmails),
        connected_accounts => ConnectedAccounts ++ [OAuthAccount]
    }),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Predicate telling if given email is occupied from the point of view of
%% given user. It is not recognized as occupied if the same user already has it.
%% @end
%%--------------------------------------------------------------------
-spec is_email_occupied(UserId :: od_user:id(), Email :: binary()) ->
    boolean().
is_email_occupied(UserId, Email) ->
    case od_user:get_by_criterion({email, Email}) of
        {ok, #document{key = UserId}} ->
            false;
        {ok, #document{}} ->
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Contacts onepanel to authenticate a user using basic authorization
%% headers. They are sent in base64 encoded form, for example:
%%   <<"Basic dXNlcjpwYXNzd29yZA==">>
%% for credentials user:password, i.e. "Basic base64(user:password)".
%% If the user does not exist in OZ, it is created.
%% Onepanel returns the type of user, i.e. admin|regular. Based on this,
%% the user is added to or removed from admins group (we have to assume that
%% the type can change in time, so when admin type is revoked we should
%% take the admin rights away from the user).
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_basic_credentials(Login :: binary(),
    Password :: binary()) ->
    {ok, UserDoc :: #document{}, FirstLogin :: boolean()} | {error, term()}.
authenticate_by_basic_credentials(Login, Password) ->
    Headers = [basic_auth_header(Login, Password)],
    URL = get_onepanel_rest_user_url(Login),
    RestCallResult = case http_client:get(URL, Headers, <<"">>, [insecure]) of
        {ok, 200, _, JSON} ->
            json_utils:decode(JSON);
        {ok, 401, _, _} ->
            {error, <<"Invalid login or password">>};
        {ok, _, _, ErrorJSON} when size(ErrorJSON) > 0 ->
            try
                ErrorProps = json_utils:decode(ErrorJSON),
                Message = proplists:get_value(<<"description">>, ErrorProps,
                    <<"Invalid login or password">>),
                {error, Message}
            catch _:_ ->
                {error, bad_request}
            end;
        {ok, _, _, _} ->
            {error, bad_request};
        {error, Error} ->
            {error, Error}
    end,
    case RestCallResult of
        {error, Reason} ->
            {error, Reason};
        Props ->
            UserId = proplists:get_value(<<"userId">>, Props),
            UserRole = proplists:get_value(<<"userRole">>, Props),
            {UserDocument, FirstLogin} = case od_user:get(UserId) of
                {error, {not_found, od_user}} ->
                    UserRecord = #od_user{
                        name = Login,
                        login = Login,
                        basic_auth_enabled = true
                    },
                    {ok, UserId} = create(UserRecord, UserId),
                    ?info("Created new account for user '~s' from onepanel "
                    "(role: '~s')", [Login, UserRole]),
                    {ok, UserDoc} = od_user:get(UserId),
                    {UserDoc, true};
                {ok, #document{value = #od_user{} = UserInfo} = UserDoc} ->
                    % Make sure user login is up to date (it might have changed
                    % in onepanel since last login). Also enable basic auth for
                    % him.
                    NewDoc = UserDoc#document{
                        value = UserInfo#od_user{
                            login = Login,
                            basic_auth_enabled = true
                        }},
                    {ok, UserId} = od_user:save(NewDoc),
                    {NewDoc, false}
            end,
            % Check if user's role entitles him to belong to any groups
            {ok, GroupMapping} = application:get_env(
                ?APP_NAME, onepanel_role_to_group_mapping),
            Groups = maps:get(UserRole, GroupMapping, []),
            lists:foreach(
                fun(GroupId) ->
                    case group_logic:has_user(GroupId, UserId) of
                        true ->
                            ok;
                        false ->
                            {ok, GroupData} = group_logic:get_data(GroupId),
                            GroupName = proplists:get_value(name, GroupData),
                            {ok, _} = group_logic:add_user(GroupId, UserId),
                            ?info("Added user '~s' to group '~s' based on "
                            "role '~s'", [Login, GroupName, UserRole])
                    end
                end, Groups),
            {ok, UserDocument, FirstLogin}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Contacts onepanel to change user's password using basic authorization
%% headers. They are sent in base64 encoded form, for example:
%%   <<"Basic dXNlcjpwYXNzd29yZA==">>
%% for credentials user:password, i.e. "Basic base64(user:password)".
%% New password is sent in request body.
%% @end
%%--------------------------------------------------------------------
-spec change_user_password(Login :: binary(), OldPassword :: binary(),
    Password :: binary()) -> ok | {error, term()}.
change_user_password(Login, OldPassword, NewPassword) ->
    Headers = [
        {<<"content-type">>, <<"application/json">>},
        basic_auth_header(Login, OldPassword)
    ],
    URL = get_onepanel_rest_user_url(Login),
    Body = json_utils:encode([{<<"password">>, NewPassword}]),
    case http_client:request(patch, URL, Headers, Body, [insecure]) of
        {ok, 204, _, _} ->
            ok;
        {ok, 401, _, _} ->
            {error, <<"Invalid password">>};
        {ok, _, _, ErrorJSON} when size(ErrorJSON) > 0 ->
            try
                ErrorProps = json_utils:decode(ErrorJSON),
                Message = proplists:get_value(<<"description">>, ErrorProps,
                    <<"Cannot change password">>),
                {error, Message}
            catch _:_ ->
                {error, bad_request}
            end;
        {ok, _, _, _} ->
            {error, bad_request};
        {error, Error} ->
            {error, Error}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns basic authorization headers based on login and password.
%% @end
%%--------------------------------------------------------------------
-spec basic_auth_header(Login :: binary(), Password :: binary()) ->
    {Key :: binary(), Value :: binary()}.
basic_auth_header(Login, Password) ->
    UserAndPassword = base64:encode(<<Login/binary, ":", Password/binary>>),
    {<<"Authorization">>, <<"Basic ", UserAndPassword/binary>>}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns onepanel REST endpoint for user management.
%% @end
%%--------------------------------------------------------------------
-spec get_onepanel_rest_user_url(Login :: binary()) -> URL :: binary().
get_onepanel_rest_user_url(Login) ->
    {ok, OnepanelRESTURL} =
        application:get_env(?APP_NAME, onepanel_rest_url),
    {ok, OnepanelGetUsersEndpoint} =
        application:get_env(?APP_NAME, onepanel_users_endpoint),
    <<(str_utils:to_binary(OnepanelRESTURL))/binary,
        (str_utils:to_binary(OnepanelGetUsersEndpoint))/binary, Login/binary>>.

setup_user(UserId, UserInfo) ->
    % Check if automatic first space is enabled, if so create a space
    % for the user.
    case application:get_env(?APP_NAME, enable_automatic_first_space) of
        {ok, true} ->
            SpaceName = case UserInfo#od_user.name of
                <<"">> ->
                    <<"Your First Space">>;
                Name ->
                    <<Name/binary, "'s space">>
            end,
            {ok, SpaceId} = n_space_logic:create(?USER(UserId), SpaceName),
            set_default_space(?USER(UserId), UserId, SpaceId);
        _ ->
            ok
    end,

    % Check if global groups are enabled, if so add the user to the groups.
    case application:get_env(?APP_NAME, enable_global_groups) of
        {ok, true} ->
            {ok, GlobalGroups} = application:get_env(?APP_NAME, global_groups),
            lists:foreach(
                fun({GroupId, Privileges}) ->
                    {ok, GroupId} = n_group_logic:add_user(
                        ?ROOT, GroupId, UserId, Privileges
                    )
                end, GlobalGroups);
        _ ->
            ok
    end.