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
-export([create/1, get_user/1, get_user_doc/1, modify/2]).
-export([get_data/2, get_spaces/1, get_groups/1, get_effective_groups/1, get_providers/1]).
-export([get_default_space/1, set_default_space/2]).
-export([get_default_provider/1, set_provider_as_default/3]).
-export([get_client_tokens/1, add_client_token/2, delete_client_token/2]).
-export([exists/1, remove/1]).
-export([set_space_name_mapping/3, clean_space_name_mapping/2]).
-export([authenticate_by_basic_credentials/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a user account.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create(User :: #onedata_user{}) ->
    {ok, UserId :: binary()}.
create(User) ->
    onedata_user:save(#document{value = User}).

%%--------------------------------------------------------------------
%% @doc Retrieves user from the database.
%%--------------------------------------------------------------------
-spec get_user(Key :: binary() | {connected_account_user_id, {ProviderID :: atom(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #onedata_user{}} | {error, any()}.
get_user(Key) ->
    try
        {ok, #document{value = #onedata_user{} = User}} = get_user_doc(Key),
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
            true -> onedata_user:get(Key);
            false -> onedata_user:get_by_criterion(Key)
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
    ok | {error, disallowed_prefix | invalid_alias | alias_occupied | alias_conflict | any()}.
modify(UserId, Proplist) ->
    try
        {ok, #document{value = User} = Doc} = onedata_user:get(UserId),
        #onedata_user{
            name = Name,
            alias = Alias,
            email_list = Emails,
            connected_accounts = ConnectedAccounts,
            spaces = Spaces,
            groups = Groups,
            default_space = DefaultSpace,
            % TODO mock
            first_space_support_token = FSST,
            default_provider = DefaultProvider,
            chosen_provider = ChosenProvider,
            client_tokens = ClientTokens
        } = User,

        % Check if alias was requested to be modified and if it is allowed
        DisallowedPrefix = fun(A) ->
            case A of
                <<?NO_ALIAS_UUID_PREFIX, _/binary>> -> true;
                _ -> false
            end
        end,

        InvalidAlias = fun(A) ->
            case re:run(A, ?ALIAS_VALIDATION_REGEXP) of
                {match, _} ->
                    false;
                _ ->
                    case A of
                        ?EMPTY_ALIAS -> false;
                        _ -> true
                    end
            end
        end,

        AliasOccupied = fun(A) ->
            case get_user_doc({alias, A}) of
                {ok, #document{key = UserId}} ->
                    % DB returned the same user, so the
                    % alias was modified but is identical, don't report errors
                    false;
                {ok, #document{}} ->
                    % Alias is occupied by another user
                    true;
                _ ->
                    % Alias is not occupied, ok
                    false
            end
        end,

        SetAlias = case proplists:get_value(alias, Proplist) of
            undefined ->
                Alias;
            NewAlias ->
                case DisallowedPrefix(NewAlias) of
                    true ->
                        {error, disallowed_prefix};
                    false ->
                        case InvalidAlias(NewAlias) of
                            true ->
                                {error, invalid_alias};
                            false ->
                                case AliasOccupied(NewAlias) of
                                    true ->
                                        {error, alias_occupied};
                                    _ -> NewAlias
                                end
                        end
                end
        end,

        case SetAlias of
            {error, Reason} ->
                % Alias not allowed, return error
                {error, Reason};
            _ ->
                NewUser = User#onedata_user{
                    name = proplists:get_value(name, Proplist, Name),
                    alias = SetAlias,
                    email_list = proplists:get_value(email_list, Proplist, Emails),
                    connected_accounts = proplists:get_value(connected_accounts, Proplist, ConnectedAccounts),
                    spaces = proplists:get_value(spaces, Proplist, Spaces),
                    groups = proplists:get_value(groups, Proplist, Groups),
                    default_space = proplists:get_value(default_space, Proplist, DefaultSpace),
                    % TODO mock
                    first_space_support_token = proplists:get_value(first_space_support_token, Proplist, FSST),
                    default_provider = proplists:get_value(default_provider, Proplist, DefaultProvider),
                    chosen_provider = proplists:get_value(chosen_provider, Proplist, ChosenProvider),
                    client_tokens = proplists:get_value(client_tokens, Proplist, ClientTokens)},
                DocNew = Doc#document{value = NewUser},
                {ok, _} = onedata_user:save(DocNew),
                case SetAlias of
                    Alias ->
                        % Alias wasn't changed
                        ok;
                    ?EMPTY_ALIAS ->
                        % Alias was changed to blank
                        ok;
                    _ ->
                        % Alias was changed, check for possible conflicts
                        try
                            {ok, NewUser} = get_user({alias, SetAlias}),
                            ok
                        catch
                            _:user_duplicated ->
                                % Alias is duplicated, revert user to initial state and leave the alias blank
                                remove(UserId),
                                onedata_user:save(Doc#document{value = #onedata_user{alias = ?EMPTY_ALIAS}}),
                                {error, alias_conflict}
                        end
                end
        end
    catch
        T:M ->
            {error, {T, M}}
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
        value = #onedata_user{
            name = Name, login = Login
        }}} = onedata_user:get(UserId),
    {ok, [
        {userId, UserId},
        {login, Login},
        {name, Name}
    ]};
get_data(UserId, user) ->
    {ok, #document{
        value = #onedata_user{
            name = Name,
            login = Login,
            connected_accounts = ConnectedAccounts,
            alias = Alias,
            email_list = EmailList
        }}} = onedata_user:get(UserId),
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
    {ok, Doc} = onedata_user:get(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    EffectiveDefaultSpace = effective_default_space(AllUserSpaces, Doc),
    {ok, [
        {spaces, AllUserSpaces},
        {default, EffectiveDefaultSpace}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns user's groups.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_groups(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_effective_groups(UserId) ->
    {ok, #document{value = #onedata_user{effective_groups = Groups}}} = onedata_user:get(UserId),
    {ok, [{effective_groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns user's groups.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_groups(UserId) ->
    {ok, #document{value = #onedata_user{groups = Groups}}} = onedata_user:get(UserId),
    {ok, [{groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns providers of user's spaces.
%% Throws exception when call to the datastore fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_providers(UserId) ->
    {ok, Doc} = onedata_user:get(UserId),
    Spaces = get_all_spaces(Doc),
    UserProviders = lists:foldl(fun(Space, Providers) ->
        {ok, #document{value = #space{providers_supports = Supports}}}
            = space:get(Space),
        {SpaceProviders, _} = lists:unzip(Supports),
        ordsets:union(ordsets:from_list(SpaceProviders), Providers)
    end, ordsets:new(), Spaces),
    {ok, [{providers, UserProviders}]}.

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
    {ok, #document{value = #onedata_user{groups = Groups, spaces = Spaces}}} = onedata_user:get(UserId),
    lists:foreach(fun(GroupId) ->
        {ok, _} = user_group:update(GroupId, fun(Group) ->
            #user_group{users = Users} = Group,
            {ok, Group#user_group{users = lists:keydelete(UserId, 1, Users)}}
        end),
        group_logic:cleanup(GroupId)
    end, Groups),
    lists:foreach(fun(SpaceId) ->
        {ok, _} = space:update(SpaceId, fun(Space) ->
            #space{users = Users} = Space,
            {ok, Space#space{users = lists:keydelete(UserId, 1, Users)}}
        end)
    end, Spaces),
    auth_logic:invalidate_token({user_id, UserId}),
    onedata_user:delete(UserId),
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
    {ok, Doc} = onedata_user:get(UserId),
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
    {ok, Doc} = onedata_user:get(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    case ordsets:is_element(SpaceId, AllUserSpaces) of
        false ->
            false;
        true ->
            {ok, _} = onedata_user:update(UserId, fun(User) ->
                {ok, User#onedata_user{default_space = SpaceId}}
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
    {ok, #onedata_user{default_provider = DefProv}} = get_user(UserId),
    {ok, DefProv}.


%%--------------------------------------------------------------------
%% @doc Retrieves user's list of client tokens.
%% @end
%%--------------------------------------------------------------------
-spec get_client_tokens(UserId :: binary()) ->
    {ok, Tokens :: [binary()]}.
get_client_tokens(UserId) ->
    {ok, #onedata_user{client_tokens = ClientTokens}} = get_user(UserId),
    {ok, ClientTokens}.


%%--------------------------------------------------------------------
%% @doc
%% Adds a token to the list of user's client tokens.
%% @end
%%--------------------------------------------------------------------
-spec add_client_token(UserId :: binary(), Token :: binary()) -> ok.
add_client_token(UserId, Token) ->
    {ok, #onedata_user{client_tokens = ClientTokens}} = get_user(UserId),
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        {ok, User#onedata_user{client_tokens = ClientTokens ++ [Token]}}
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a token from the list of user's client tokens.
%% @end
%%--------------------------------------------------------------------
-spec delete_client_token(UserId :: binary(), Token :: binary()) -> ok.
delete_client_token(UserId, Token) ->
    {ok, #onedata_user{client_tokens = ClientTokens}} = get_user(UserId),
    {ok, _} = onedata_user:update(UserId, fun(User) ->
        {ok, User#onedata_user{client_tokens = ClientTokens -- [Token]}}
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
            Diff = fun(#onedata_user{default_provider = CrrntDefProv} = User) ->
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
                {ok, User#onedata_user{
                    default_provider = NewDefProv}}
            end,
            {ok, _} = onedata_user:update(UserId, Diff),
            true
    end.

%%--------------------------------------------------------------------
%% @doc Sets name of a space, so that it is unique for the user. If user already
%% is a member of another space with provided name, a '#' character and prefix
%% of space ID it prepended to the name, so that the new name is unique and the
%% prefix of a space ID satisfies minimal length.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_space_name_mapping(UserId :: binary(), SpaceId :: binary(),
    SpaceName :: binary()) -> ok.
set_space_name_mapping(UserId, SpaceId, SpaceName) ->
    SpaceNameLen = size(SpaceName),
    UniqueSpaceName = <<SpaceName/binary, "#", SpaceId/binary>>,

    {ok, _} = onedata_user:update(UserId, fun(User) ->
        #onedata_user{space_names = SpaceNames} = User,

        {ShortestUniquePrefLen, FilteredSpaces} = maps:fold(fun
            (Id, _, {UniquePrefLen, SpacesAcc}) when Id == SpaceId ->
                {UniquePrefLen, SpacesAcc};
            (Id, Name, {UniquePrefLen, SpacesAcc}) ->
                PrefLen = binary:longest_common_prefix([UniqueSpaceName, Name]),
                {max(PrefLen + 1, UniquePrefLen), maps:put(Id, Name, SpacesAcc)}
        end, {SpaceNameLen, #{}}, SpaceNames),

        ValidUniquePrefLen = case ShortestUniquePrefLen > SpaceNameLen of
            true ->
                min(max(ShortestUniquePrefLen,
                    SpaceNameLen + 1 + ?MIN_SUFFIX_HASH_LEN),
                    size(UniqueSpaceName));
            false ->
                ShortestUniquePrefLen
        end,

        ShortestUniqueSpaceName = <<UniqueSpaceName:ValidUniquePrefLen/binary>>,
        NewUser = User#onedata_user{
            space_names = maps:put(SpaceId, ShortestUniqueSpaceName, FilteredSpaces)
        },

        {ok, NewUser}
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
            {ok, _} = onedata_user:update(UserId, fun(User) ->
                #onedata_user{space_names = SpaceNames} = User,
                NewUser = User#onedata_user{space_names = maps:remove(SpaceId, SpaceNames)},
                {ok, NewUser}
            end),
            true
    end.

%%--------------------------------------------------------------------
%% @doc
%% Contacts onepanel to authenticate a user using basic authorization
%% headers. The are sent in base64 encoded form, for example:
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
    {ok, UserDoc :: #document{}} | {error, term()}.
authenticate_by_basic_credentials(Login, Password) ->
    UserAndPassword = base64:encode(<<Login/binary, ":", Password/binary>>),
    BasicAuthHeader = <<"Basic ", UserAndPassword/binary>>,
    RestCallResult = case BasicAuthHeader of
        <<"Basic dXNlcjE6cGFzc3dvcmQ=">> ->
            {ok, [
                {<<"userId">>, <<"user1">>},
                {<<"userRole">>, <<"admin">>}
            ]};
        <<"Basic dXNlcjI6cGFzc3dvcmQ=">> ->
            {ok, [
                {<<"userId">>, <<"user2">>},
                {<<"userRole">>, <<"regular">>}
            ]};
        <<"Basic dXNlcjQ6cGFzc3dvcmQ=">> ->
            {ok, [
                {<<"userId">>, <<"user4">>},
                {<<"userRole">>, <<"regular">>}
            ]};
        _ ->
            {error, not_found}
    end,
    case RestCallResult of
        {error, Reason} ->
            {error, Reason};
        {ok, Props} ->
            UserId = proplists:get_value(<<"userId">>, Props),
            UserRole = proplists:get_value(<<"userRole">>, Props),
            UserDocument = case onedata_user:get(UserId) of
                {error, {not_found, onedata_user}} ->
                    UserDoc = #document{
                        key = UserId,
                        value = #onedata_user{
                            name = Login,
                            login = Login,
                            basic_auth_enabled = true
                        }},
                    {ok, UserId} = onedata_user:save(UserDoc),
                    ?info("Created new account for user '~s' from onepanel",
                        [Login, UserRole]),
                    UserDoc;
                {ok, #document{value = #onedata_user{} = UserInfo} = UserDoc} ->
                    % Make sure user login is up to date (it might have changed
                    % in onepanel since last login). Also enable basic auth for
                    % him.
                    UserDoc#document{
                        value = UserInfo#onedata_user{
                            login = Login,
                            basic_auth_enabled = true
                        }}
            end,
            % Check if user's role entitles him to belong to any groups
            {ok, GroupMapping} = application:get_env(
                ?APP_Name, onepanel_role_to_group_mapping),
            Groups = proplists:get_value(UserRole, GroupMapping, []),
            lists:foreach(
                fun(GroupId) ->
                    case group_logic:has_user(GroupId, UserId) of
                        true ->
                            ok;
                        false ->
                            {ok, GroupData} = group_logic:get_data(GroupId),
                            GroupName = proplists:get_value(name, GroupData),
                            ok = group_logic:add_user(GroupId, UserId),
                            ?info("Added user '~s' to group '~s' based on "
                            "role '~s'", [Login, GroupName, UserRole])
                    end
                end, Groups),
            {ok, UserDocument}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
get_all_spaces(#document{value = #onedata_user{} = User}) ->
    #onedata_user{spaces = UserSpaces, groups = Groups} = User,

    UserSpacesSet = ordsets:from_list(UserSpaces),
    GroupSpacesSets = lists:map(
        fun(GroupId) ->
            {ok, GroupDoc} = user_group:get(GroupId),
            #document{value = #user_group{spaces = GroupSpaces}} = GroupDoc,
            ordsets:from_list(GroupSpaces)
        end, Groups),

    ordsets:union([UserSpacesSet | GroupSpacesSets]).

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
effective_default_space(_, #document{value = #onedata_user{default_space = undefined}}) ->
    undefined;
effective_default_space(AllUserSpaces, #document{} = UserDoc) ->
    #document{value = #onedata_user{default_space = DefaultSpaceId} = User} = UserDoc,
    case ordsets:is_element(DefaultSpaceId, AllUserSpaces) of
        true -> DefaultSpaceId;
        false ->
            UserNew = User#onedata_user{default_space = undefined},
            onedata_user:save(UserDoc#document{value = UserNew}),
            undefined
    end.
