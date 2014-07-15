%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains helper functions for logic modules.
%% ===================================================================
-module(logic_helper).
-author("Konrad Zemek").

-include_lib("ctool/include/logging.hrl").
-include("dao/dao_types.hrl").


-type dao_module() :: dao_users | dao_groups | dao_tokens | dao_spaces | dao_providers.


%% API
-export([space_exists/1, user_exists/1, group_exists/1, token_exists/1,
    provider_exists/1]).
-export([save/1]).
-export([space/1, user/1, group/1, token/1, provider/1]).
-export([space_doc/1, user_doc/1, user_doc_from_view/1, group_doc/1, token_doc/1, provider_doc/1]).
-export([user_remove/1, space_remove/1, group_remove/1, token_remove/1,
    provider_remove/1]).


%% space_exists/1
%% ====================================================================
%% @doc Returns whether a Space exists in the database.
%% ====================================================================
-spec space_exists(SpaceId :: binary()) -> boolean() | no_return().
%% ====================================================================
space_exists(SpaceId) ->
    exists(SpaceId, dao_spaces, exist_space).


%% user_exists/1
%% ====================================================================
%% @doc Returns whether a user exists in the database.
%% ====================================================================
-spec user_exists(UserId :: binary()) -> boolean() | no_return().
%% ====================================================================
user_exists(UserId) ->
    exists(UserId, dao_users, exist_user).


%% group_exists/1
%% ====================================================================
%% @doc Returns whether a group exists in the database.
%% ====================================================================
-spec group_exists(GroupId :: binary()) -> boolean() | no_return().
%% ====================================================================
group_exists(GroupId) ->
    exists(GroupId, dao_groups, exist_group).


%% token_exists/1
%% ====================================================================
%% @doc Returns whether a token exists in the database.
%% ====================================================================
-spec token_exists(TokenId :: binary()) -> boolean() | no_return().
%% ====================================================================
token_exists(TokenId) ->
    exists(TokenId, dao_tokens, exist_token).


%% provider_exists/1
%% ====================================================================
%% @doc Returns whether a provider exists in the database.
%% ====================================================================
-spec provider_exists(ProviderId :: binary()) -> boolean() | no_return().
%% ====================================================================
provider_exists(ProviderId) ->
    exists(ProviderId, dao_providers, exist_provider).


%% save/1
%% ====================================================================
%% @doc Creates a new document or updates an existing one.
%% ====================================================================
-spec save(Doc :: veil_doc() | space_info() | user_info() | group_info() |
token_info() | provider_info()) -> Id :: binary() | no_return().
%% ====================================================================
save(#veil_document{} = Doc) ->
    {ok, StrId} = save_doc(Doc),
    binary:list_to_bin(StrId);
save(Resource) ->
    save(#veil_document{record = Resource}).


%% space/1
%% ====================================================================
%% @doc Returns a space object from the database.
%% ====================================================================
-spec space(SpaceId :: binary()) -> space_info() | no_return().
%% ====================================================================
space(SpaceId) ->
    #veil_document{record = Space} = space_doc(SpaceId),
    Space.


%% user/1
%% ====================================================================
%% @doc Returns a user object from the database.
%% ====================================================================
-spec user(UserId :: binary()) -> user_info() | no_return().
%% ====================================================================
user(UserId) ->
    #veil_document{record = User} = user_doc(UserId),
    User.


%% group/1
%% ====================================================================
%% @doc Returns a group object from the database.
%% ====================================================================
-spec group(GroupId :: binary()) -> group_info() | no_return().
%% ====================================================================
group(GroupId) ->
    #veil_document{record = Group} = group_doc(GroupId),
    Group.


%% token/1
%% ====================================================================
%% @doc Returns a token object from the database.
%% ====================================================================
-spec token(TokenId :: binary()) -> token_info() | no_return().
%% ====================================================================
token(TokenId) ->
    #veil_document{record = Token} = token_doc(TokenId),
    Token.


%% provider/1
%% ====================================================================
%% @doc Returns a provider object from the database.
%% ====================================================================
-spec provider(ProviderId :: binary()) -> provider_info() | no_return().
%% ====================================================================
provider(ProviderId) ->
    #veil_document{record = Provider} = provider_doc(ProviderId),
    Provider.


%% space_doc/1
%% ====================================================================
%% @doc Returns a space document from the database.
%% ====================================================================
-spec space_doc(SpaceId :: binary()) -> space_doc() | no_return().
%% ====================================================================
space_doc(SpaceId) ->
    #veil_document{record = #space{}} = SpaceDoc = get_doc(SpaceId, dao_spaces, get_space),
    SpaceDoc.


%% user_doc/1
%% ====================================================================
%% @doc Returns a user document from the database.
%% ====================================================================
-spec user_doc(UserId :: binary()) -> user_doc() | no_return().
%% ====================================================================
user_doc(UserId) ->

    #veil_document{record = #user{}} = UserDoc = get_doc(UserId, dao_users, get_user),
    {ok, UserDoc}.


%% user_doc_from_view/1
%% ====================================================================
%% @doc Returns a user document from the database, querying a view.
%% ====================================================================
-spec user_doc_from_view(Key :: {email, Email :: binary()}) -> user_doc() | no_return().
%% ====================================================================
user_doc_from_view(Key) ->
    dao_lib:apply(dao_users, get_user, [Key], 1).


%% group_doc/1
%% ====================================================================
%% @doc Returns a group document from the database.
%% ====================================================================
-spec group_doc(GroupId :: binary()) -> group_doc() | no_return().
%% ====================================================================
group_doc(GroupId) ->
    #veil_document{record = #user_group{}} = GroupDoc = get_doc(GroupId, dao_groups, get_group),
    GroupDoc.


%% token_doc/1
%% ====================================================================
%% @doc Returns a token document from the database.
%% ====================================================================
-spec token_doc(TokenId :: binary()) -> token_doc() | no_return().
%% ====================================================================
token_doc(TokenId) ->
    #veil_document{record = #token{}} = TokenDoc = get_doc(TokenId, dao_tokens, get_token),
    TokenDoc.


%% provider_doc/1
%% ====================================================================
%% @doc Returns a provider document from the database.
%% ====================================================================
-spec provider_doc(ProviderId :: binary()) -> provider_doc() | no_return().
%% ====================================================================
provider_doc(ProviderId) ->
    #veil_document{record = #provider{}} = ProviderDoc = get_doc(ProviderId, dao_providers, get_provider),
    ProviderDoc.


%% user_remove/1
%% ====================================================================
%% @doc Removes a user document from the database.
%% ====================================================================
-spec user_remove(UserId :: binary()) -> true | no_return().
%% ====================================================================
user_remove(UserId) ->
    remove(UserId, dao_users, remove_user).


%% space_remove/1
%% ====================================================================
%% @doc Removes a space document from the database.
%% ====================================================================
-spec space_remove(SpaceId :: binary()) -> true | no_return().
%% ====================================================================
space_remove(SpaceId) ->
    remove(SpaceId, dao_spaces, remove_space).


%% group_remove/1
%% ====================================================================
%% @doc Removes a group document from the database.
%% ====================================================================
-spec group_remove(GroupId :: binary()) -> true | no_return().
%% ====================================================================
group_remove(GroupId) ->
    remove(GroupId, dao_groups, remove_group).


%% token_remove/1
%% ====================================================================
%% @doc Removes a token document from the database.
%% ====================================================================
-spec token_remove(TokenId :: binary()) -> true | no_return().
%% ====================================================================
token_remove(TokenId) ->
    remove(TokenId, dao_tokens, remove_token).


%% provider_remove/1
%% ====================================================================
%% @doc Removes a provider document from the database.
%% ====================================================================
-spec provider_remove(ProviderId :: binary()) -> true | no_return().
%% ====================================================================
provider_remove(ProviderId) ->
    remove(ProviderId, dao_providers, remove_provider).


%% exists/3
%% ====================================================================
%% @doc Returns whether a resource exists in the database. Internal function.
%% ====================================================================
-spec exists(Id :: binary(), Module :: dao_module(), Method :: atom()) ->
    boolean() | no_return().
%% ====================================================================
exists(Id, Module, Method) ->
    StrId = binary:bin_to_list(Id),
    {ok, Exists} = dao_lib:apply(Module, Method, [StrId], 1),
    Exists.


%% save_doc/1
%% ====================================================================
%% @doc Creates a new document or updates an existing one. Internal function.
%% ====================================================================
-spec save_doc(Doc :: veil_doc()) ->
    {ok, uuid()} | {error, any()} | no_return().
%% ====================================================================
save_doc(#veil_document{record = #space{}} = Doc) ->
    dao_lib:apply(dao_spaces, save_space, [Doc], 1);
save_doc(#veil_document{record = #user{}} = Doc) ->
    dao_lib:apply(dao_users, save_user, [Doc], 1);
save_doc(#veil_document{record = #user_group{}} = Doc) ->
    dao_lib:apply(dao_groups, save_group, [Doc], 1);
save_doc(#veil_document{record = #token{}} = Doc) ->
    dao_lib:apply(dao_tokens, save_token, [Doc], 1);
save_doc(#veil_document{record = #provider{}} = Doc) ->
    dao_lib:apply(dao_providers, save_provider, [Doc], 1).


%% get_doc/3
%% ====================================================================
%% @doc Returns a document from the database. Internal function.
%% ====================================================================
-spec get_doc(Id :: binary(), Module :: dao_module(), Method :: atom()) ->
    veil_doc() | no_return().
%% ====================================================================
get_doc(Id, Module, Method) ->
    StrId = binary:bin_to_list(Id),
    {ok, #veil_document{} = Doc} = dao_lib:apply(Module, Method, [StrId], 1),
    Doc.


%% remove/3
%% ====================================================================
%% @doc Removes a document from the database. Internal function.
%% ====================================================================
-spec remove(Id :: binary(), Module :: dao_module(), Method :: atom()) ->
    true | no_return().
%% ====================================================================
remove(Id, Module, Method) ->
    StrId = binary:bin_to_list(Id),
    ok = dao_lib:apply(Module, Method, [StrId], 1),
    true.
