%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for registry's users.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(user_logic).
-author("Konrad Zemek").

-include_lib("ctool/include/logging.hrl").
-include("dao/dao_types.hrl").


%% API
-export([create/1, get_user/1, get_user_doc/1, modify/2, merge/2]).
-export([get_data/1, get_spaces/1, get_groups/1]).
-export([get_default_space/1, set_default_space/2]).
-export([exists/1, remove/1]).


%% ====================================================================
%% API functions
%% ====================================================================


%% create/1
%% ====================================================================
%% @doc Creates a user account.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec create(User :: #user{}) ->
    {ok, UserId :: binary()}.
%% ====================================================================
create(User) ->
    UserId = dao_adapter:save(User),
    {ok, UserId}.


%% get_user/1
%% ====================================================================
%% @doc Retrieves user from the database.
%% ====================================================================
-spec get_user(Key :: binary() | {connected_account_user_id, {ProviderID :: binary(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #user{}} | {error, any()}.
%% ====================================================================
get_user(Key) ->
    try
        {ok, #db_document{record = #user{} = User}} = get_user_doc(Key),
        {ok, User}
    catch
        T:M ->
            {error, {T, M}}
    end.


%% get_user_doc/1
%% ====================================================================
%% @doc Retrieves user doc from the database.
%% ====================================================================
-spec get_user_doc(Key :: binary() | {connected_account_user_id, {ProviderID :: binary(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #db_document{}} | {error, any()}.
%% ====================================================================
get_user_doc(Key) ->
    try
        #db_document{record = #user{}} = UserDoc = dao_adapter:user_doc(Key),
        {ok, UserDoc}
    catch
        T:M ->
            {error, {T, M}}
    end.


%% modify/2
%% ====================================================================
%% @doc Modifies user details. Second argument is proplist with keys
%% corresponding to record field names. The proplist may contain any
%% subset of fields to change.
%% @end
%% ====================================================================
-spec modify(UserId :: binary(), Proplist :: [{atom(), binary()}]) ->
    ok | {error, disallowed_prefix | invalid_alias | alias_occupied | alias_conflict | any()}.
%% ====================================================================
modify(UserId, Proplist) ->
    try
        #db_document{record = User} = Doc = dao_adapter:user_doc(UserId),
        #user{
            name = Name,
            alias = Alias,
            email_list = Emails,
            connected_accounts = ConnectedAccounts,
            spaces = Spaces,
            groups = Groups,
            default_space = DefaultSpace,
            % TODO mock
            first_space_support_token = FSST} = User,

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
            UserIdStr = gui_str:to_list(UserId),
            case get_user_doc({alias, A}) of
                {ok, #db_document{uuid = UserIdStr}} ->
                    % DB returned the same user, so the
                    % alias was modified but is identical, don't report errors
                    false;
                {ok, #db_document{}} ->
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
                NewUser = #user{
                    name = proplists:get_value(name, Proplist, Name),
                    alias = SetAlias,
                    email_list = proplists:get_value(email_list, Proplist, Emails),
                    connected_accounts = proplists:get_value(connected_accounts, Proplist, ConnectedAccounts),
                    spaces = proplists:get_value(spaces, Proplist, Spaces),
                    groups = proplists:get_value(groups, Proplist, Groups),
                    default_space = proplists:get_value(default_space, Proplist, DefaultSpace),
                    % TODO mock
                    first_space_support_token = proplists:get_value(first_space_support_token, Proplist, FSST)},
                DocNew = Doc#db_document{record = NewUser},
                dao_adapter:save(DocNew),
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
                                dao_adapter:save(Doc#db_document{record = #user{alias = ?EMPTY_ALIAS}}),
                                {error, alias_conflict}
                        end
                end
        end
    catch
        T:M ->
            {error, {T, M}}
    end.


%% merge/2
%% ====================================================================
%% @doc Merges an account identified by token into current user's account.
%% ====================================================================
-spec merge(UserId :: binary(), Token :: binary()) ->
    ok.
%% ====================================================================
merge(_UserId, _Token) ->
    %% @todo: a functional merge
    ok.


%% get_data/1
%% ====================================================================
%% @doc Returns user details.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec get_data(UserId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_data(UserId) ->
    #user{name = Name} = dao_adapter:user(UserId),
    {ok, [
        {userId, UserId},
        {name, Name}
    ]}.


%% get_spaces/1
%% ====================================================================
%% @doc Returns user's spaces.
%% Throws exception when call to dao fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%% ====================================================================
-spec get_spaces(UserId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_spaces(UserId) ->
    Doc = dao_adapter:user_doc(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    EffectiveDefaultSpace = effective_default_space(AllUserSpaces, Doc),
    {ok, [
        {spaces, AllUserSpaces},
        {default, EffectiveDefaultSpace}
    ]}.


%% get_groups/1
%% ====================================================================
%% @doc Returns user's groups.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec get_groups(UserId :: binary()) ->
    {ok, [proplists:property()]}.
%% ====================================================================
get_groups(UserId) ->
    #user{groups = Groups} = dao_adapter:user(UserId),
    {ok, [{groups, Groups}]}.


%% exists/1
%% ====================================================================
%% @doc Rreturns true if user was found by a given key.
%% ====================================================================
-spec exists(Key :: binary() | {connected_account_user_id, binary()} |
{email, binary()} | {alias, binary()}) ->
    boolean().
%% ====================================================================
exists(Key) ->
    case get_user_doc(Key) of
        {ok, _} -> true;
        _ -> false
    end.


%% remove/1
%% ====================================================================
%% @doc Remove user's account.
%% Throws exception when call to dao fails, or user is already deleted.
%% @end
%% ====================================================================
-spec remove(UserId :: binary()) ->
    true.
%% ====================================================================
remove(UserId) ->
    #user{groups = Groups, spaces = Spaces} = dao_adapter:user(UserId),

    lists:foreach(fun(GroupId) ->
        GroupDoc = dao_adapter:group_doc(GroupId),
        #db_document{record = #user_group{users = Users} = Group} = GroupDoc,
        GroupNew = Group#user_group{users = lists:keydelete(UserId, 1, Users)},
        dao_adapter:save(GroupDoc#db_document{record = GroupNew}),
        group_logic:cleanup(GroupId)
    end, Groups),

    lists:foreach(fun(SpaceId) ->
        SpaceDoc = dao_adapter:space_doc(SpaceId),
        #db_document{record = #space{users = Users} = Space} = SpaceDoc,
        SpaceNew = Space#space{users = lists:keydelete(UserId, 1, Users)},
        dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),
        space_logic:cleanup(SpaceId)
    end, Spaces),

    dao_adapter:user_remove(UserId).


%% get_default_space/1
%% ====================================================================
%% @doc Retrieve user's default space ID.
%% Throws exception when call to dao fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%% ====================================================================
-spec get_default_space(UserId :: binary()) ->
    {ok, SpaceId :: binary() | undefined}.
%% ====================================================================
get_default_space(UserId) ->
    Doc = dao_adapter:user_doc(UserId),
    AllUserSpaces = get_all_spaces(Doc),
    {ok, effective_default_space(AllUserSpaces, Doc)}.


%% set_default_space/2
%% ====================================================================
%% @doc Set user's default space ID.
%% Throws exception when call to dao fails, or user doesn't exist, or his groups
%% don't exist.
%% @end
%% ====================================================================
-spec set_default_space(UserId :: binary(), SpaceId :: binary()) ->
    true.
%% ====================================================================
set_default_space(UserId, SpaceId) ->
    Doc = dao_adapter:user_doc(UserId),
    #db_document{record = User} = Doc,

    AllUserSpaces = get_all_spaces(Doc),
    case ordsets:is_element(SpaceId, AllUserSpaces) of
        false -> false;
        true ->
            UpdatedUser = User#user{default_space = SpaceId},
            dao_adapter:save(Doc#db_document{record = UpdatedUser}),
            true
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% get_all_spaces/1
%% ====================================================================
%% @doc Returns a list of all spaces that a user belongs to, directly or through
%% a group.
%% Throws exception when call to dao fails, or user's groups don't exist.
%% @end
%% ====================================================================
-spec get_all_spaces(Doc :: db_doc()) ->
    ordsets:ordset(SpaceId :: binary()).
%% ====================================================================
get_all_spaces(#db_document{record = #user{} = User}) ->
    #user{spaces = UserSpaces, groups = Groups} = User,

    UserSpacesSet = ordsets:from_list(UserSpaces),
    GroupSpacesSets = lists:map(
        fun(GroupId) ->
            GroupDoc = dao_adapter:group_doc(GroupId),
            #db_document{record = #user_group{spaces = GroupSpaces}} = GroupDoc,
            ordsets:from_list(GroupSpaces)
        end, Groups),

    ordsets:union([UserSpacesSet | GroupSpacesSets]).


%% effective_default_space/2
%% ====================================================================
%% @doc Returns an effective default space id; i.e. validates and changes
%% (if needed) the default space id set in the user doc. Returns the new, valid
%% space id.
%% Throws exception when call to dao fails, or user's groups don't exist.
%% @end
%% ====================================================================
-spec effective_default_space(AllUserSpaces :: ordsets:ordset(binary()),
    UserDoc :: db_doc()) ->
    EffectiveDefaultSpaceId :: binary() | undefined.
%% ====================================================================
effective_default_space(_, #db_document{record = #user{default_space = undefined}}) ->
    undefined;
effective_default_space(AllUserSpaces, #db_document{} = UserDoc) ->
    #db_document{record = #user{default_space = DefaultSpaceId} = User} = UserDoc,
    case ordsets:is_element(DefaultSpaceId, AllUserSpaces) of
        true -> DefaultSpaceId;
        false ->
            UserNew = User#user{default_space = undefined},
            dao_adapter:save(UserDoc#db_document{record = UserNew}),
            undefined
    end.
