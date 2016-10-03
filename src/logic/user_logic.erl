%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for registry's users.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(user_logic).
-author("Konrad Zemek").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/utils/utils.hrl").
-include_lib("ctool/include/logging.hrl").

-define(MIN_SUFFIX_HASH_LEN, 6).

%% API
-export([create/1, create/2, get_user/1, get_user_doc/1, get_data/2]).
-export([modify/2, set_alias/2, add_oauth_account/2, is_email_occupied/2]).
-export([get_groups/1, get_effective_groups/1, get_providers/1, has_provider/2]).
-export([get_spaces/1, get_handle_services/1, get_handles/1, get_shares/1,
    get_default_space/1, set_default_space/2]).
-export([get_default_provider/1, set_provider_as_default/3]).
-export([get_client_tokens/1, add_client_token/2, delete_client_token/2]).
-export([exists/1, remove/1]).
-export([set_space_name_mapping/4, clean_space_name_mapping/2]).
-export([authenticate_by_basic_credentials/2, change_user_password/3]).
-export([get_all_handle_services/1, get_all_handles/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a user account.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create(UserInfo :: #od_user{}) ->
    {ok, UserId :: od_user:id()}.
create(UserInfo) ->
    create(UserInfo, undefined).


%%--------------------------------------------------------------------
%% @doc Creates a user account with given Id.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create(UserInfo :: #od_user{},
    ProposedUserId :: od_user:id() | undefined) ->
    {ok, UserId :: od_user:id()}.
create(UserInfo, ProposedUserId) ->
    {ok, UserId} = od_user:create(
        #document{key = ProposedUserId, value = UserInfo}
    ),

    % Check if automatic first space is enabled, if so create a space
    % for the user.
    case application:get_env(?APP_Name, enable_automatic_first_space) of
        {ok, true} ->
            SpaceName = case UserInfo#od_user.name of
                <<"">> ->
                    <<"Your First Space">>;
                Name ->
                    <<Name/binary, "'s space">>
            end,
            {ok, SpaceId} = space_logic:create({user, UserId}, SpaceName),
            {ok, _} = od_user:update(UserId, #{
                default_space => SpaceId
            });
        _ ->
            ok
    end,

    % Check if global groups are enabled, if so add the new user to the groups.
    case application:get_env(?APP_Name, enable_global_groups) of
        {ok, true} ->
            {ok, GlobalGroups} = application:get_env(?APP_Name, global_groups),
            lists:foreach(
                fun({GroupId, Privileges}) ->
                    {ok, GroupId} = group_logic:add_user(GroupId, UserId),
                    ok = group_logic:set_privileges(GroupId, UserId, Privileges)
                end, GlobalGroups);
        _ ->
            ok
    end,
    {ok, UserId}.


%%--------------------------------------------------------------------
%% @doc Retrieves user from the database.
%%--------------------------------------------------------------------
-spec get_user(Key :: binary() | {connected_account_user_id, {ProviderID :: atom(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #od_user{}} | {error, any()}.
get_user(Key) ->
    try
        {ok, #document{value = #od_user{} = User}} = get_user_doc(Key),
        {ok, User}
    catch
        T:M ->
            {error, {T, M}}
    end.

%%--------------------------------------------------------------------
%% @doc Retrieves user doc from the database.
%%--------------------------------------------------------------------
-spec get_user_doc(Key :: binary() | {connected_account_user_id, {ProviderID :: atom(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #document{}} | {error, any()}.
get_user_doc(Key) ->
    try
        case is_binary(Key) of
            true -> od_user:get(Key);
            false -> od_user:get_by_criterion(Key)
        end
    catch
        T:M ->
            {error, {T, M}}
    end.


%%--------------------------------------------------------------------
%% @doc Modifies user details. Second argument is proplist with keys
%% corresponding to record field names. The proplist may contain any
%% subset of fields to change.
%% @end
%%--------------------------------------------------------------------
-spec modify(UserId :: binary(), Proplist :: [{atom(), term()}]) ->
    ok | {error, Reason} when
    Reason :: disallowed_alias_prefix | invalid_alias | alias_occupied | any().
modify(UserId, Proplist) ->
    % Name update should always succeed, start with it
    case proplists:get_value(name, Proplist) of
        undefined ->
            % Name did not change
            ok;
        NewName ->
            % New name requested, set it
            {ok, _} = od_user:update(UserId, #{name => NewName})
    end,
    % Update alias if needed
    case proplists:get_value(alias, Proplist) of
        undefined ->
            % Alias did not change
            ok;
        NewAlias ->
            % New alias requested, try to set it
            set_alias(UserId, NewAlias)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Modifies user alias, and makes sure that alias cannot be duplicated.
%% @end
%%--------------------------------------------------------------------
-spec set_alias(UserId :: od_user:id(), Alias :: binary()) ->
    ok | {error, disallowed_alias_prefix | invalid_alias | alias_occupied}.
set_alias(UserId, Alias) ->
    % Fun used to check if alias has a disallowed prefix
    DisallowedPrefix = fun() ->
        case Alias of
            <<?NO_ALIAS_UUID_PREFIX, _/binary>> -> true;
            _ -> false
        end
    end,
    % Fun used to check if alias is valid
    InvalidAlias = fun() ->
        case Alias of
            ?EMPTY_ALIAS ->
                false;
            _ ->
                case re:run(Alias, ?ALIAS_VALIDATION_REGEXP) of
                    {match, _} -> false;
                    _ -> true
                end
        end
    end,
    % Fun used to perform alias update
    UpdateAlias = fun() ->
        critical_section:run({alias, Alias}, fun() ->
            % Check if this alias is occupied
            case get_user_doc({alias, Alias}) of
                {ok, #document{key = UserId}} ->
                    % DB returned the same user, so the
                    % alias was modified but is identical, don't report errors.
                    ok;
                {ok, #document{}} ->
                    % Alias is occupied by another user
                    {error, alias_occupied};
                _ ->
                    % Alias is not occupied, update user doc
                    {ok, _} = od_user:update(UserId, #{alias => Alias}),
                    ok
            end
        end)
    end,
    % Throw everything together
    case DisallowedPrefix() of
        true ->
            {error, disallowed_alias_prefix};
        false ->
            case InvalidAlias() of
                true ->
                    {error, invalid_alias};
                false ->
                    UpdateAlias()
            end
    end.


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
    case get_user_doc({email, Email}) of
        {ok, #document{key = UserId}} ->
            false;
        {ok, #document{}} ->
            true;
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc Returns user details.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(UserId :: binary(), Type :: provider | user) ->
    {ok, [proplists:property()]}.
get_data(UserId, provider) ->
    {ok, #document{
        value = #od_user{
            name = Name, login = Login
        }}} = od_user:get(UserId),
    {ok, [
        {userId, UserId},
        {login, Login},
        {name, Name}
    ]};
get_data(UserId, user) ->
    {ok, #document{
        value = #od_user{
            name = Name,
            login = Login,
            connected_accounts = ConnectedAccounts,
            alias = Alias,
            email_list = EmailList
        }}} = od_user:get(UserId),
    ConnectedAccountsMaps = lists:map(fun(Account) ->
        ?record_to_list(oauth_account, Account) end, ConnectedAccounts),
    {ok, [
        {userId, UserId},
        {login, Login},
        {name, Name},
        {connectedAccounts, ConnectedAccountsMaps},
        {alias, Alias},
        {emailList, EmailList}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns user's spaces.
%% Throws exception when call to the datastore fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_spaces(UserId) ->
    {ok, Doc} = od_user:get(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    EffectiveDefaultSpace = effective_default_space(AllUserSpaces, Doc),
    {ok, [
        {spaces, AllUserSpaces},
        {default, EffectiveDefaultSpace}
    ]}.


%%--------------------------------------------------------------------
%% @doc Returns user's handle_services.
%% Throws exception when call to the datastore fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_services(UserId :: od_user:id()) -> {ok, [proplists:property()]}.
get_handle_services(UserId) ->
    {ok, Doc} = od_user:get(UserId),
    AllUserHandleServices = get_all_handle_services(Doc),
    {ok, [{handle_services, AllUserHandleServices}]}.

%%--------------------------------------------------------------------
%% @doc Returns user's handles.
%% Throws exception when call to the datastore fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_handles(UserId :: od_user:id()) ->
    {ok, [proplists:property()]}.
get_handles(UserId) ->
    {ok, Doc} = od_user:get(UserId),
    AllUserHandles = get_all_handles(Doc),
    {ok, [{handles, AllUserHandles}]}.

%%--------------------------------------------------------------------
%% @doc Returns user's shares, i.e. all shares of all spaces in which user has
%% the privilege to manage shares.
%% Throws exception when call to the datastore fails, or user doesn't exist,
%% or his groups don't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_shares(UserId :: binary()) -> {ok, [proplists:property()]}.
get_shares(UserId) ->
    {ok, Doc} = od_user:get(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    % Resolve user shares - he can only view the shares in spaces where
    % he has proper privileges.
    UserShares = lists:foldl(
        fun(SpaceId, Acc) ->
            case space_logic:has_effective_privilege(
                SpaceId, UserId, space_manage_shares) of
                true ->
                    {ok, SharesInSpace} = space_logic:get_shares(SpaceId),
                    SharesInSpace ++ Acc;
                false ->
                    Acc
            end
        end, [], AllUserSpaces),
    {ok, UserShares}.

%%--------------------------------------------------------------------
%% @doc Returns user's groups.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_groups(UserId :: binary()) -> {ok, [proplists:property()]}.
get_effective_groups(UserId) ->
    {ok, #document{value = #od_user{eff_groups = Groups}}} = od_user:get(UserId),
    {ok, [{effective_groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns user's groups.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_groups(UserId) ->
    {ok, #document{value = #od_user{groups = Groups}}} = od_user:get(UserId),
    {ok, [{groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns providers of user's spaces.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(UserId :: binary()) -> {ok, [proplists:property()]}.
get_providers(UserId) ->
    {ok, Doc} = od_user:get(UserId),
    Spaces = get_all_spaces(Doc),
    UserProviders = lists:foldl(fun(Space, Providers) ->
        {ok, #document{
            value = #od_space{
                providers_supports = Supports
            }}} = od_space:get(Space),
        {SpaceProviders, _} = lists:unzip(Supports),
        ordsets:union(ordsets:from_list(SpaceProviders), Providers)
    end, ordsets:new(), Spaces),
    {ok, [{providers, UserProviders}]}.


%%--------------------------------------------------------------------
%% @doc
%% Predicate telling if given user is effectively supported by given provider.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec has_provider(UserId :: od_user:id(), ProviderId :: od_provider:id()) ->
    boolean().
has_provider(UserId, ProviderId) ->
    {ok, [{providers, UserProviders}]} = get_providers(UserId),
    lists:member(ProviderId, UserProviders).


%%--------------------------------------------------------------------
%% @doc Returns true if user was found by a given key.
%%--------------------------------------------------------------------
-spec exists(Key :: binary() | {connected_account_user_id, {ProviderID :: atom(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    boolean().
exists(Key) ->
    case get_user_doc(Key) of
        {ok, _} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Remove user's account.
%% Throws exception when call to the datastore fails, or user is already deleted.
%% @end
%%--------------------------------------------------------------------
-spec remove(UserId :: binary()) ->
    true.
remove(UserId) ->
    {ok, #document{value = #od_user{groups = Groups, spaces = Spaces}}} = od_user:get(UserId),
    lists:foreach(fun(GroupId) ->
        {ok, _} = od_group:update(GroupId, fun(Group) ->
            #od_group{users = Users} = Group,
            {ok, Group#od_group{users = lists:keydelete(UserId, 1, Users)}}
        end),
        group_logic:cleanup(GroupId)
    end, Groups),
    lists:foreach(fun(SpaceId) ->
        {ok, _} = od_space:update(SpaceId, fun(Space) ->
            #od_space{users = Users} = Space,
            {ok, Space#od_space{users = lists:keydelete(UserId, 1, Users)}}
        end)
    end, Spaces),
    auth_logic:invalidate_token({user_id, UserId}),
    od_user:delete(UserId),
    true.

%%--------------------------------------------------------------------
%% @doc Retrieve user's default space ID.
%% Throws exception when call to the datastore fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_default_space(UserId :: binary()) ->
    {ok, SpaceId :: binary() | undefined}.
get_default_space(UserId) ->
    {ok, Doc} = od_user:get(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    {ok, effective_default_space(AllUserSpaces, Doc)}.

%%--------------------------------------------------------------------
%% @doc Set user's default space ID.
%% Throws exception when call to the datastore fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_default_space(UserId :: binary(), SpaceId :: binary()) ->
    boolean().
set_default_space(UserId, SpaceId) ->
    {ok, Doc} = od_user:get(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    case ordsets:is_element(SpaceId, AllUserSpaces) of
        false ->
            false;
        true ->
            {ok, _} = od_user:update(UserId, fun(User) ->
                {ok, User#od_user{default_space = SpaceId}}
            end),
            true
    end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves user's default provider (or returns undefined).
%% @end
%%--------------------------------------------------------------------
-spec get_default_provider(UserId :: binary()) ->
    {ok, ProviderId :: binary() | undefined}.
get_default_provider(UserId) ->
    {ok, #od_user{default_provider = DefProv}} = get_user(UserId),
    {ok, DefProv}.


%%--------------------------------------------------------------------
%% @doc Retrieves user's list of client tokens.
%% @end
%%--------------------------------------------------------------------
-spec get_client_tokens(UserId :: binary()) ->
    {ok, Tokens :: [binary()]}.
get_client_tokens(UserId) ->
    {ok, #od_user{client_tokens = ClientTokens}} = get_user(UserId),
    {ok, ClientTokens}.


%%--------------------------------------------------------------------
%% @doc
%% Adds a token to the list of user's client tokens.
%% @end
%%--------------------------------------------------------------------
-spec add_client_token(UserId :: binary(), Token :: binary()) -> ok.
add_client_token(UserId, Token) ->
    {ok, #od_user{client_tokens = ClientTokens}} = get_user(UserId),
    {ok, _} = od_user:update(UserId, fun(User) ->
        {ok, User#od_user{client_tokens = ClientTokens ++ [Token]}}
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a token from the list of user's client tokens.
%% @end
%%--------------------------------------------------------------------
-spec delete_client_token(UserId :: binary(), Token :: binary()) -> ok.
delete_client_token(UserId, Token) ->
    {ok, #od_user{client_tokens = ClientTokens}} = get_user(UserId),
    {ok, _} = od_user:update(UserId, fun(User) ->
        {ok, User#od_user{client_tokens = ClientTokens -- [Token]}}
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc Set given provider as default for user or un-sets it.
%% (It is allowed to not have a default provider)
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_provider_as_default(UserId :: binary(), ProviderId :: binary(),
    Flag :: boolean()) -> boolean().
set_provider_as_default(UserId, ProviderId, Flag) ->
    {ok, [{providers, AllUserProviders}]} = get_providers(UserId),
    case ordsets:is_element(ProviderId, AllUserProviders) of
        false ->
            false;
        true ->
            Diff = fun(#od_user{default_provider = CrrntDefProv} = User) ->
                NewDefProv = case Flag of
                    true ->
                        % Setting ProviderId to default
                        ProviderId;
                    false ->
                        case ProviderId of
                            CrrntDefProv ->
                                % Un-setting ProviderId as default - the user
                                % no longer has a default provider.
                                undefined;
                            _ ->
                                % Un-setting ProviderId as default - but current
                                % default provider is different, so it stays.
                                CrrntDefProv
                        end
                end,
                {ok, User#od_user{
                    default_provider = NewDefProv}}
            end,
            {ok, _} = od_user:update(UserId, Diff),
            true
    end.

%%--------------------------------------------------------------------
%% @doc Sets name of a space, so that it is unique for the user. If user already
%% is a member of another space with provided name, a '#' character and prefix
%% of space ID it prepended to the name, so that the new name is unique and the
%% prefix of a space ID satisfies minimal length.
%% The Overwrite argument decides if SpaceName should overwrite the existing
%% name mapping for given SpaceId, if such exists. If there is no mapping, it is
%% added no matter the Overwrite arg.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_space_name_mapping(UserId :: binary(), SpaceId :: binary(),
    SpaceName :: binary(), Overwrite :: boolean()) -> ok.
set_space_name_mapping(UserId, SpaceId, SpaceName, Overwrite) ->
    SpaceNameLen = size(SpaceName),
    UniqueSpaceName = <<SpaceName/binary, "#", SpaceId/binary>>,

    {ok, _} = od_user:update(UserId, fun(User) ->
        #od_user{space_aliases = SpaceNames} = User,
        case (not Overwrite) andalso maps:is_key(SpaceId, SpaceNames) of
            true ->
                % If the map already contains the key and Overwrite is false,
                % nothing needs to be done.
                {ok, User};
            _ ->
                % Else, we have to update or add the name mapping and check for
                % duplicated names.
                {ShortestUniquePfxLen, FilteredSpaces} = maps:fold(fun
                    (Id, _, {UniquePrefLen, SpacesAcc}) when Id == SpaceId ->
                        {UniquePrefLen, SpacesAcc};
                    (Id, Name, {UniquePrefLen, SpacesAcc}) ->
                        PrefLen = binary:longest_common_prefix(
                            [UniqueSpaceName, Name]),
                        {
                            max(PrefLen + 1, UniquePrefLen),
                            maps:put(Id, Name, SpacesAcc)
                        }
                end, {SpaceNameLen, #{}}, SpaceNames),

                ValidUniquePrefLen = case ShortestUniquePfxLen > SpaceNameLen of
                    true ->
                        min(max(ShortestUniquePfxLen,
                            SpaceNameLen + 1 + ?MIN_SUFFIX_HASH_LEN),
                            size(UniqueSpaceName));
                    false ->
                        ShortestUniquePfxLen
                end,

                ShortestUniqueSpaceName =
                    <<UniqueSpaceName:ValidUniquePrefLen/binary>>,
                NewMapping = maps:put(
                    SpaceId, ShortestUniqueSpaceName, FilteredSpaces),
                NewUser = User#od_user{
                    space_aliases = NewMapping
                },

                {ok, NewUser}
        end
    end),
    ok.

%%--------------------------------------------------------------------
%% @doc Removes space name mapping if user does not effectively belongs to the space.
%% Returns true if space name has been removed from the map, otherwise false.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec clean_space_name_mapping(UserId :: binary(), SpaceId :: binary()) -> boolean().
clean_space_name_mapping(UserId, SpaceId) ->
    case space_logic:has_effective_user(SpaceId, UserId) of
        true ->
            false;
        false ->
            {ok, _} = od_user:update(UserId, fun(User) ->
                #od_user{space_aliases = SpaceNames} = User,
                NewUser = User#od_user{space_aliases = maps:remove(SpaceId, SpaceNames)},
                {ok, NewUser}
            end),
            true
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
    Password :: binary()) -> {ok, UserDoc :: #document{}} | {error, term()}.
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
            UserDocument = case od_user:get(UserId) of
                {error, {not_found, onedata_user}} ->
                    UserRecord = #od_user{
                        name = Login,
                        login = Login,
                        basic_auth_enabled = true
                    },
                    {ok, UserId} = create(UserRecord, UserId),
                    ?info("Created new account for user '~s' from onepanel "
                    "(role: '~s')", [Login, UserRole]),
                    {ok, UserDoc} = od_user:get(UserId),
                    UserDoc;
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
                    NewDoc
            end,
            % Check if user's role entitles him to belong to any groups
            {ok, GroupMapping} = application:get_env(
                ?APP_Name, onepanel_role_to_group_mapping),
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
            {ok, UserDocument}
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
    case http_client:put(URL, Headers, Body, [insecure]) of
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
        application:get_env(?APP_Name, onepanel_rest_url),
    {ok, OnepanelGetUsersEndpoint} =
        application:get_env(?APP_Name, onepanel_users_endpoint),
    <<(str_utils:to_binary(OnepanelRESTURL))/binary,
        (str_utils:to_binary(OnepanelGetUsersEndpoint))/binary, Login/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Returns a list of all spaces that a user belongs to, directly or through
%% a group.
%% Throws exception when call to the datastore fails, or user's groups don't
%% exist.
%% @end
%%--------------------------------------------------------------------
-spec get_all_spaces(Doc :: datastore:document()) ->
    ordsets:ordset(SpaceId :: binary()).
get_all_spaces(#document{value = #od_user{} = User}) ->
    #od_user{spaces = UserSpaces, groups = Groups} = User,

    UserSpacesSet = ordsets:from_list(UserSpaces),
    GroupSpacesSets = lists:map(
        fun(GroupId) ->
            {ok, GroupDoc} = od_group:get(GroupId),
            #document{value = #od_group{spaces = GroupSpaces}} = GroupDoc,
            ordsets:from_list(GroupSpaces)
        end, Groups),

    ordsets:union([UserSpacesSet | GroupSpacesSets]).

%%--------------------------------------------------------------------
%% @private
%% @doc Returns a list of all handle_services that a user belongs to, directly or through
%% a group.
%% Throws exception when call to the datastore fails, or user's groups don't
%% exist.
%% @end
%%--------------------------------------------------------------------
-spec get_all_handle_services(Doc :: datastore:document()) ->
    ordsets:ordset(HandleServiceId :: od_handle_service:id()).
get_all_handle_services(#document{value = #od_user{
    handle_services = UserHandleServices, groups = Groups}}) ->

    UserHandleServicesSet = ordsets:from_list(UserHandleServices),
    GroupHandleServicesSets = lists:map(
        fun(GroupId) ->
            {ok, GroupDoc} = od_group:get(GroupId),
            #document{value = #od_group{handle_services = GroupHandleServices}} = GroupDoc,
            ordsets:from_list(GroupHandleServices)
        end, Groups),

    ordsets:union([UserHandleServicesSet | GroupHandleServicesSets]).

%%--------------------------------------------------------------------
%% @private
%% @doc Returns a list of all handles that a user belongs to, directly or through
%% a group.
%% Throws exception when call to the datastore fails, or user's groups don't
%% exist.
%% @end
%%--------------------------------------------------------------------
-spec get_all_handles(Doc :: od_user:doc()) ->
    ordsets:ordset(HandleId :: od_handle:id()).
get_all_handles(#document{value = #od_user{
    handles = UserHandles, groups = Groups}}) ->

    UserHandlesSet = ordsets:from_list(UserHandles),
    GroupHandlesSets = lists:map(
        fun(GroupId) ->
            {ok, GroupDoc} = od_group:get(GroupId),
            #document{value = #od_group{handles = GroupHandles}} = GroupDoc,
            ordsets:from_list(GroupHandles)
        end, Groups),

    ordsets:union([UserHandlesSet | GroupHandlesSets]).

%%--------------------------------------------------------------------
%% @private
%% @doc Returns an effective default space id; i.e. validates and changes
%% (if needed) the default space id set in the user doc. Returns the new, valid
%% space id.
%% Throws exception when call to the datastore fails, or user's groups don't exist.
%% @end
%%--------------------------------------------------------------------
-spec effective_default_space(AllUserSpaces :: ordsets:ordset(binary()),
    UserDoc :: datastore:document()) ->
    EffectiveDefaultSpaceId :: binary() | undefined.
effective_default_space(_, #document{value = #od_user{default_space = undefined}}) ->
    undefined;
effective_default_space(AllUserSpaces, #document{} = UserDoc) ->
    #document{value = #od_user{default_space = DefaultSpaceId} = User} = UserDoc,
    case ordsets:is_element(DefaultSpaceId, AllUserSpaces) of
        true -> DefaultSpaceId;
        false ->
            UserNew = User#od_user{default_space = undefined},
            od_user:save(UserDoc#document{value = UserNew}),
            undefined
    end.
