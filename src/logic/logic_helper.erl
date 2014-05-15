%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains helper functions for *_logic modules.
%% ===================================================================
-module(logic_helper).
-author("Konrad Zemek").

-include("dao/dao_types.hrl").

-type dao_module() :: dao_users | dao_groups | dao_tokens | dao_spaces |
    dao_providers.


%% API
-export([space_doc/1, user_doc/1, group_doc/1, token_doc/1, provider_doc/1, save/1, user_remove/1, space_remove/1, group_remove/1, token_remove/1, provider_remove/1, space/1, user/1, group/1, token/1, provider/1]).


-spec save(Doc :: veil_doc() | space_info() | user_info() | group_info() |
    token_info() | provider_info()) -> Id :: binary() | no_return().
save(#veil_document{} = Doc) ->
    {ok, StrId} = save_doc(Doc),
    binary:list_to_bin(StrId);
save(Resource) ->
    save(#veil_document{record = Resource}).

-spec save_doc(Doc :: veil_doc()) -> {ok, uuid()} | {error, any()} | no_return().
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

-spec space(SpaceId :: binary()) -> space_info() | no_return().
space(SpaceId) ->
    #veil_document{record = Space} = space_doc(SpaceId),
    Space.

-spec user(UserId :: binary()) -> user_info() | no_return().
user(UserId) ->
    #veil_document{record = User} = user_doc(UserId),
    User.

-spec group(GroupId :: binary()) -> group_info() | no_return().
group(GroupId) ->
    #veil_document{record = Group} = group_doc(GroupId),
    Group.

-spec token(TokenId :: binary()) -> token_info() | no_return().
token(TokenId) ->
    #veil_document{record = Token} = token_doc(TokenId),
    Token.

-spec provider(ProviderId :: binary()) -> provider_info() | no_return().
provider(ProviderId) ->
    #veil_document{record = Provider} = provider_doc(ProviderId),
    Provider.


-spec space_doc(SpaceId :: binary()) -> space_doc() | no_return().
space_doc(SpaceId) ->
    #veil_document{record = #space{}} = SpaceDoc = get_doc(SpaceId, dao_spaces, get_space),
    SpaceDoc.


-spec user_doc(UserId :: binary()) -> user_doc() | no_return().
user_doc(UserId) ->
    #veil_document{record = #user{}} = UserDoc = get_doc(UserId, dao_users, get_user),
    UserDoc.


-spec group_doc(GroupId :: binary()) -> group_doc() | no_return().
group_doc(GroupId) ->
    #veil_document{record = #user_group{}} = GroupDoc = get_doc(GroupId, dao_groups, get_group),
    GroupDoc.


-spec token_doc(TokenId :: binary()) -> token_doc() | no_return().
token_doc(TokenId) ->
    #veil_document{record = #token{}} = TokenDoc = get_doc(TokenId, dao_tokens, get_token),
    TokenDoc.


-spec provider_doc(ProviderId :: binary()) -> provider_doc() | no_return().
provider_doc(ProviderId) ->
    #veil_document{record = #provider{}} = ProviderDoc = get_doc(ProviderId, dao_providers, get_provider),
    ProviderDoc.


-spec get_doc(Id :: binary(), Module :: dao_module(), Method :: atom()) ->
    veil_doc() | no_return().
get_doc(Id, Module, Method) ->
    StrId = binary:bin_to_list(Id),
    {ok, #veil_document{} = Doc} = dao_lib:apply(Module, Method, [StrId], 1),
    Doc.


user_remove(UserId) ->
    remove(UserId, dao_users, remove_user).

space_remove(SpaceId) ->
    remove(SpaceId, dao_spaces, remove_space).
group_remove(GroupId) ->
    remove(GroupId, dao_groups, remove_group).
token_remove(TokenId) ->
    remove(TokenId, dao_tokens, remove_token).
provider_remove(ProviderId) ->
    remove(ProviderId, dao_providers, remove_provider).

remove(Id, Module, Method) ->
    StrId = binary:bin_to_list(Id),
    ok = dao_lib:apply(Module, Method, [StrId], 1),
    true.
