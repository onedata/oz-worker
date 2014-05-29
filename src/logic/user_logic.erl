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

-include("dao/dao_types.hrl").
-include("logging.hrl").


%% API
-export([create/1, get_user/1, modify/2, merge/2]).
-export([get_data/1, get_spaces/1, get_groups/1]).
-export([remove/1]).


%% create/1
%% ====================================================================
%% @doc Creates a user account.
%% ====================================================================
-spec create(Name :: binary()) ->
    {ok, UserId :: binary()} | no_return().
%% ====================================================================
create(#user{} = User) ->
    UserId = logic_helper:save(User),
    {ok, UserId}.


%% get_user/1
%% ====================================================================
%% @doc Creates a user account.
%% ====================================================================
-spec get_user(Key) -> {ok, #user{}} | {error, term()} when
    Key :: binary() | {global_id, binary()}.
%% ====================================================================
get_user(Key) ->
    try
    case Key of
        Bin when is_binary(Bin) ->
            logic_helper:user_doc(Key);
        Key ->
            logic_helper:user_doc_from_view(Key)
    end
catch T:M -> ?error_stacktrace("~p:~p", [T, M]) end.


%% modify/2
%% ====================================================================
%% @doc Modifies user details. Second argument is proplist with keys
%% corresponding to record field names. The proplist may contain any
%% subset of fields to change.
%% ====================================================================
-spec modify(UserId :: binary(), Proplist :: [{atom(), binary()}]) ->
    ok | no_return().
%% ====================================================================
modify(UserId, Proplist) ->
    {ok, #veil_document{record = User} = Doc} = logic_helper:user_doc(UserId),
    #user{
        name = Name,
        email_list = Emails,
        connected_accounts = ConnectedAccounts,
        spaces = Spaces,
        groups = Groups} = User,
    NewUser = #user{
        name = proplists:get_value(name, Proplist, Name),
        email_list = proplists:get_value(email_list, Proplist, Emails),
        connected_accounts = proplists:get_value(connected_accounts, Proplist, ConnectedAccounts),
        spaces = proplists:get_value(spaces, Proplist, Spaces),
        groups = proplists:get_value(groups, Proplist, Groups)},
    DocNew = Doc#veil_document{record = NewUser},
    logic_helper:save(DocNew),
    ok.


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
%% ====================================================================
-spec get_data(UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_data(UserId) ->
    #veil_document{record = User} = logic_helper:user_doc(UserId),
    #user{name = Name} = User,
    {ok, [
        {userId, UserId},
        {name, Name}
    ]}.


%% get_spaces/1
%% ====================================================================
%% @doc Returns user's spaces.
%% ====================================================================
-spec get_spaces(UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_spaces(UserId) ->
    Doc = logic_helper:user_doc(UserId),
    #veil_document{record = #user{spaces = Spaces}} = Doc,
    {ok, [{spaces, Spaces}]}.


%% get_groups/1
%% ====================================================================
%% @doc Returns user's groups.
%% ====================================================================
-spec get_groups(UserId :: binary()) ->
    {ok, [proplists:property()]} | no_return().
%% ====================================================================
get_groups(UserId) ->
    Doc = logic_helper:user_doc(UserId),
    #veil_document{record = #user{groups = Groups}} = Doc,
    {ok, [{groups, Groups}]}.


%% remove/1
%% ====================================================================
%% @doc Remove user's account.
%% ====================================================================
-spec remove(UserId :: binary()) -> boolean().
%% ====================================================================
remove(UserId) ->
    Doc = logic_helper:user_doc(UserId),
    #veil_document{record = #user{groups = Groups, spaces = Spaces}} = Doc,

    lists:foreach(fun(GroupId) ->
        GroupDoc = logic_helper:group_doc(GroupId),
        #veil_document{record = #user_group{users = Users} = Group} = GroupDoc,
        GroupNew = Group#user_group{users = lists:keydelete(UserId, 1, Users)},
        logic_helper:save(GroupDoc#veil_document{record = GroupNew}),
        group_logic:cleanup(GroupId)
    end, Groups),

    lists:foreach(fun(SpaceId) ->
        SpaceDoc = logic_helper:space_doc(SpaceId),
        #veil_document{record = #space{users = Users} = Space} = SpaceDoc,
        SpaceNew = Space#space{users = lists:keydelete(UserId, 1, Users)},
        logic_helper:save(SpaceDoc#veil_document{record = SpaceNew}),
        space_logic:cleanup(SpaceId)
    end, Spaces),

    logic_helper:user_remove(UserId).
