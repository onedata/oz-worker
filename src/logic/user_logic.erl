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


%% create/1
%% ====================================================================
%% @doc Creates a user account.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec create(User :: #user{}) ->
    {ok, UserId :: binary()} | no_return().
%% ====================================================================
create(User) ->
    UserId = dao_adapter:save(User),
    {ok, UserId}.


%% get_user/1
%% ====================================================================
%% @doc Retrieves user from the database.
%% ====================================================================
-spec get_user(Key) -> {ok, #user{}} | {error, term()} when
    Key :: UserId :: binary() | {connected_account_user_id, binary()} | {email, binary()}.
%% ====================================================================
get_user(Key) ->
    try
        {ok, #veil_document{record = #user{} = User}} = get_user_doc(Key),
        {ok, User}
    catch
        T:M ->
            {error, {T, M}}
    end.


%% get_user_doc/1
%% ====================================================================
%% @doc Retrieves user doc from the database.
%% ====================================================================
-spec get_user_doc(Key) -> {ok, #veil_document{}} | {error, term()} when
    Key :: UserId :: binary() | {connected_account_user_id, binary()} | {email, binary()}.
%% ====================================================================
get_user_doc(Key) ->
    try
        #veil_document{record = #user{}} = UserDoc =
            case Key of
                Bin when is_binary(Bin) ->
                    dao_adapter:user_doc(Key);
                Key ->
                    dao_adapter:user_doc_from_view(Key)
            end,
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
    ok | {error, term()}.
%% ====================================================================
modify(UserId, Proplist) ->
    try
        #veil_document{record = User} = Doc = dao_adapter:user_doc(UserId),
        #user{
            name = Name,
            email_list = Emails,
            connected_accounts = ConnectedAccounts,
            spaces = Spaces,
            groups = Groups,
        % TODO mock
            first_space_support_token = FSST} = User,
        NewUser = #user{
            name = proplists:get_value(name, Proplist, Name),
            email_list = proplists:get_value(email_list, Proplist, Emails),
            connected_accounts = proplists:get_value(connected_accounts, Proplist, ConnectedAccounts),
            spaces = proplists:get_value(spaces, Proplist, Spaces),
            groups = proplists:get_value(groups, Proplist, Groups),
            % TODO mock
            first_space_support_token = proplists:get_value(first_space_support_token, Proplist, FSST)},
        DocNew = Doc#veil_document{record = NewUser},
        dao_adapter:save(DocNew),
        ok
    catch
        T:M ->
            {error, {T, M}}
    end.


%% merge/2
%% ====================================================================
%% @doc Merges an account identified by token into current user's account.
%% ====================================================================
-spec merge(UserId :: binary(), Token :: binary()) ->
    ok | no_return().
%% ====================================================================
merge(_UserId, _Token) ->
    %% @todo: a proper authentication must come first, so that a certificate
    %% or whatever is redirected to new user id
    ok.


%% get_data/1
%% ====================================================================
%% @doc Returns user details.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec get_data(UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_data(UserId) ->
    #veil_document{record = User} = dao_adapter:user_doc(UserId),
    #user{name = Name} = User,
    {ok, [
        {userId, UserId},
        {name, Name}
    ]}.


%% get_spaces/1
%% ====================================================================
%% @doc Returns user's spaces.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec get_spaces(UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_spaces(UserId) ->
    Doc = dao_adapter:user_doc(UserId),
    #veil_document{record = #user{spaces = Spaces}} = Doc,
    {ok, [{spaces, Spaces}]}.


%% get_groups/1
%% ====================================================================
%% @doc Returns user's groups.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec get_groups(UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_groups(UserId) ->
    Doc = dao_adapter:user_doc(UserId),
    #veil_document{record = #user{groups = Groups}} = Doc,
    {ok, [{groups, Groups}]}.


%% exists/1
%% ====================================================================
%% @doc Rreturns true if a user by given key was found.
%% @end
%% ====================================================================
-spec exists(Key) -> true | false when
    Key :: UserId :: binary() | {connected_account_user_id, binary()} | {email, binary()}.
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
-spec remove(UserId :: binary()) -> true | no_return().
%% ====================================================================
remove(UserId) ->
    Doc = dao_adapter:user_doc(UserId),
    #veil_document{record = #user{groups = Groups, spaces = Spaces}} = Doc,

    lists:foreach(fun(GroupId) ->
        GroupDoc = dao_adapter:group_doc(GroupId),
        #veil_document{record = #user_group{users = Users} = Group} = GroupDoc,
        GroupNew = Group#user_group{users = lists:keydelete(UserId, 1, Users)},
        dao_adapter:save(GroupDoc#veil_document{record = GroupNew}),
        group_logic:cleanup(GroupId)
    end, Groups),

    lists:foreach(fun(SpaceId) ->
        SpaceDoc = dao_adapter:space_doc(SpaceId),
        #veil_document{record = #space{users = Users} = Space} = SpaceDoc,
        SpaceNew = Space#space{users = lists:keydelete(UserId, 1, Users)},
        dao_adapter:save(SpaceDoc#veil_document{record = SpaceNew}),
        space_logic:cleanup(SpaceId)
    end, Spaces),

    dao_adapter:user_remove(UserId).


%% get_default_space/1
%% ====================================================================
%% @doc Retrieve user's default space ID.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec get_default_space(UserId :: binary()) ->
    {ok, SpaceId :: binary() | undefined} | no_return().
%% ====================================================================
get_default_space(UserId) ->
    Doc = dao_adapter:user_doc(UserId),
    #veil_document{record = #user{default_space = DefaultSpaceId} = User} = Doc,

    EffectiveDefaultSpaceId = case DefaultSpaceId of
        undefined -> undefined;
        _ ->
            case is_a_member(UserId, DefaultSpaceId) of
                true -> DefaultSpaceId;
                false ->
                    UserNew = User#user{default_space = undefined},
                    dao_adapter:save(Doc#veil_document{record = UserNew}),
                    undefined
            end
    end,

    {ok, EffectiveDefaultSpaceId}.


%% set_default_space/2
%% ====================================================================
%% @doc Set user's default space ID.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec set_default_space(UserId :: binary(), SpaceId :: binary()) ->
    true.
%% ====================================================================
set_default_space(UserId, SpaceId) ->
    Doc = dao_adapter:user_doc(UserId),
    #veil_document{record = User} = Doc,

    true = is_a_member(UserId, SpaceId), %% @TODO: error handling

    UpdatedUser = User#user{default_space = SpaceId},
    dao_adapter:save(Doc#veil_document{record = UpdatedUser}),

    true.


%% is_a_member/2
%% ====================================================================
%% @doc Determines if a user is a direct or indirect member of a space.
%% Throws exception when call to dao fails, or user doesn't exist.
%% @end
%% ====================================================================
-spec is_a_member(UserId :: binary(), SpaceId :: binary()) ->
    boolean().
%% ====================================================================
is_a_member(UserId, SpaceId) ->
    Doc = dao_adapter:user_doc(UserId),
    #veil_document{record = #user{spaces = Spaces, groups = Groups}} = Doc,

    case lists:member(SpaceId, Spaces) of
        true -> true;
        false ->
            case space_logic:exists(SpaceId) of
                false -> false;
                true -> is_a_member_by_proxy(SpaceId, Groups)
            end
    end.

%% is_a_member_by_proxy/2
%% ====================================================================
%% @doc Determines if a user is an indirect member of a space.
%% ====================================================================
-spec is_a_member_by_proxy(SpaceId :: binary(), Groupd :: [binary()]) ->
    boolean().
%% ====================================================================
is_a_member_by_proxy(_SpaceId, []) -> false;
is_a_member_by_proxy(SpaceId, [Group | Groups]) ->
    case space_logic:has_group(SpaceId, Group) of
        true -> true;
        false -> is_a_member_by_proxy(SpaceId, Groups)
    end.