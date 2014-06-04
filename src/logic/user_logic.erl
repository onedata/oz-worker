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


%% API
-export([create/1, modify/2, merge/2]).
-export([get_data/1, get_spaces/1, get_groups/1]).
-export([remove/1]).


%% create/1
%% ====================================================================
%% @doc Creates a user account.
%% ====================================================================
-spec create(Name :: binary()) ->
    {ok, UserId :: binary()} | no_return().
%% ====================================================================
create(Name) ->
    UserId = logic_helper:save(#user{name = Name}),
    {ok, UserId}.


%% modify/2
%% ====================================================================
%% @doc Modifies user details.
%% ====================================================================
-spec modify(UserId :: binary(), Name :: binary()) ->
    ok | no_return().
%% ====================================================================
modify(UserId, Name) ->
    #veil_document{record = User} = Doc = logic_helper:user_doc(UserId),
    DocNew = Doc#veil_document{record = User#user{name = Name}},
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
-spec remove(UserId :: binary()) -> true | no_return().
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
