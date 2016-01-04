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

-include_lib("ctool/include/logging.hrl").
-include("datastore/datastore_types.hrl").
-include("datastore/gr_datastore_models_def.hrl").

%% API
-export([create/1, get_user/1, get_user_doc/1, modify/2, merge/2]).
-export([get_data/1, get_spaces/1, get_groups/1, get_providers/1]).
-export([get_default_space/1, set_default_space/2]).
-export([exists/1, remove/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a user account.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec create(User :: #onedata_user{}) ->
    {ok, UserId :: binary()}.
create(User) ->
    onedata_user:save(#document{value = User}).

%%--------------------------------------------------------------------
%% @doc Retrieves user from the database.
%%--------------------------------------------------------------------
-spec get_user(Key :: binary() | {connected_account_user_id, {ProviderID :: binary(), UserID :: binary()}} |
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
-spec get_user_doc(Key :: binary() | {connected_account_user_id, {ProviderID :: binary(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #document{}} | {error, any()}.
get_user_doc(Key) ->
    try
        case is_binary(Key) of
            true -> {ok, #document{value = #onedata_user{}} = UserDoc} = onedata_user:get(Key);
            false -> {ok, #document{value = #onedata_user{}} = UserDoc} = onedata_user:get_by_criterion(Key)
        end,
        {ok, UserDoc}
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
        {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),
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
            default_provider = DefaultProvider
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
                NewUser = #onedata_user{
                    name = proplists:get_value(name, Proplist, Name),
                    alias = SetAlias,
                    email_list = proplists:get_value(email_list, Proplist, Emails),
                    connected_accounts = proplists:get_value(connected_accounts, Proplist, ConnectedAccounts),
                    spaces = proplists:get_value(spaces, Proplist, Spaces),
                    groups = proplists:get_value(groups, Proplist, Groups),
                    default_space = proplists:get_value(default_space, Proplist, DefaultSpace),
                    % TODO mock
                    first_space_support_token = proplists:get_value(first_space_support_token, Proplist, FSST),
                    default_provider = proplists:get_value(default_provider, Proplist, DefaultProvider)},
                DocNew = Doc#document{value = NewUser},
                onedata_user:save(DocNew),
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
                            op_channel_logic:user_modified(UserProviders, UserId, NewUser),
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
%% @doc Merges an account identified by token into current user's account.
%%--------------------------------------------------------------------
-spec merge(UserId :: binary(), Macaroon :: macaroon:macaroon()) ->
    ok.
merge(_UserId, _Macaroon) ->
    %% @todo: a functional merge
    ok.

%%--------------------------------------------------------------------
%% @doc Returns user details.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_data(UserId) ->
    {ok, #document{value = #onedata_user{name = Name}}} = onedata_user:get(UserId),
    {ok, [
        {userId, UserId},
        {name, Name}
    ]}.

%%--------------------------------------------------------------------
%% @doc Returns user's spaces.
%% Throws exception when call to dao fails, or user doesn't exist, or his groups
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
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_groups(UserId) ->
    {ok, #document{value = #onedata_user{groups = Groups}}} = onedata_user:get(UserId),
    {ok, [{groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns providers of user's spaces.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(UserId :: binary()) ->
    {ok, [proplists:property()]}.
get_providers(UserId) ->
    {ok, Doc} = onedata_user:get(UserId),
    Spaces = get_all_spaces(Doc),
    UserProviders = lists:foldl(fun(Space, Providers) ->
        {ok, #document{value = #space{providers = SpaceProviders}}} = space:get(Space),
        ordsets:union(ordsets:from_list(SpaceProviders), Providers)
    end, ordsets:new(), Spaces),
    {ok, [{providers, UserProviders}]}.

%%--------------------------------------------------------------------
%% @doc Rreturns true if user was found by a given key.
%%--------------------------------------------------------------------
-spec exists(Key :: binary() | {connected_account_user_id, binary()} |
{email, binary()} | {alias, binary()}) ->
    boolean().
exists(Key) ->
    case get_user_doc(Key) of
        {ok, _} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Remove user's account.
%% Throws exception when call to dao fails, or user is already deleted.
%% @end
%%--------------------------------------------------------------------
-spec remove(UserId :: binary()) ->
    true.
remove(UserId) ->
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),
    {ok, #document{value = #onedata_user{groups = Groups, spaces = Spaces}}} = onedata_user:get(UserId),

    lists:foreach(fun(GroupId) ->
        {ok, GroupDoc} = user_group:get(GroupId),
        #document{value = #user_group{users = Users} = Group} = GroupDoc,
        GroupNew = Group#user_group{users = lists:keydelete(UserId, 1, Users)},
        user_group:save(GroupDoc#document{value = GroupNew}),
        group_logic:cleanup(GroupId)
    end, Groups),

    lists:foreach(fun(SpaceId) ->
        {ok, SpaceDoc} = space:get(SpaceId),
        #document{value = #space{users = Users, providers = SpaceProviders} = Space} = SpaceDoc,
        SpaceNew = Space#space{users = lists:keydelete(UserId, 1, Users)},
        space:save(SpaceDoc#document{value = SpaceNew}),
        space_logic:cleanup(SpaceId),

        op_channel_logic:space_modified(SpaceProviders, SpaceId, SpaceNew)
    end, Spaces),

    auth_logic:invalidate_token({user_id, UserId}),

    onedata_user:delete(UserId),
    op_channel_logic:user_removed(UserProviders, UserId),
    true.

%%--------------------------------------------------------------------
%% @doc Retrieve user's default space ID.
%% Throws exception when call to dao fails, or user doesn't exist, or his groups
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
%% Throws exception when call to dao fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%%--------------------------------------------------------------------
-spec set_default_space(UserId :: binary(), SpaceId :: binary()) ->
    true.
set_default_space(UserId, SpaceId) ->
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),
    {ok, Doc} = onedata_user:get(UserId),
    #document{value = User} = Doc,

    AllUserSpaces = get_all_spaces(Doc),
    case ordsets:is_element(SpaceId, AllUserSpaces) of
        false -> false;
        true ->
            UpdatedUser = User#onedata_user{default_space = SpaceId},
            onedata_user:save(Doc#document{value = UpdatedUser}),
            op_channel_logic:user_modified(UserProviders, UserId, UpdatedUser),
            true
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Returns a list of all spaces that a user belongs to, directly or through
%% a group.
%% Throws exception when call to dao fails, or user's groups don't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_all_spaces(Doc :: db_doc()) ->
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
%% Throws exception when call to dao fails, or user's groups don't exist.
%% @end
%%--------------------------------------------------------------------
-spec effective_default_space(AllUserSpaces :: ordsets:ordset(binary()),
    UserDoc :: db_doc()) ->
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
