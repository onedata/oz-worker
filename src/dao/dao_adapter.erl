%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains adapters for all high level dao modules.
%% ===================================================================
-module(dao_adapter).
-author("Konrad Zemek").

-include("dao/dao_types.hrl").


-type dao_module() :: dao_users | dao_groups | dao_tokens | dao_spaces |
    dao_providers | dao_auth.

-type dao_record() :: veil_doc() | user_info() | provider_info() |
    group_info() |space_info() | token_info() | authorization_info() |
    access_info().


%% API
-export([space_exists/1, user_exists/1, group_exists/1, token_exists/1,
    provider_exists/1]).
-export([save/1]).
-export([space/1, user/1, group/1, token/1, provider/1]).
-export([space_doc/1, user_doc/1, group_doc/1, token_doc/1, provider_doc/1]).
-export([user_remove/1, space_remove/1, group_remove/1, token_remove/1,
    provider_remove/1]).


%% space_exists/1
%% ====================================================================
%% @doc Returns whether a Space exists in the database.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec space_exists(Key :: term()) -> boolean() | no_return().
%% ====================================================================
space_exists(Key) ->
    exists(Key, dao_spaces, exist_space).


%% user_exists/1
%% ====================================================================
%% @doc Returns whether a user exists in the database.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec user_exists(Key :: term()) -> boolean() | no_return().
%% ====================================================================
user_exists(Key) ->
    exists(Key, dao_users, exist_user).


%% group_exists/1
%% ====================================================================
%% @doc Returns whether a group exists in the database.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec group_exists(Key :: term()) -> boolean() | no_return().
%% ====================================================================
group_exists(Key) ->
    exists(Key, dao_groups, exist_group).


%% token_exists/1
%% ====================================================================
%% @doc Returns whether a token exists in the database.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec token_exists(Key :: term()) -> boolean() | no_return().
%% ====================================================================
token_exists(Key) ->
    exists(Key, dao_tokens, exist_token).


%% provider_exists/1
%% ====================================================================
%% @doc Returns whether a provider exists in the database.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec provider_exists(Key :: term()) -> boolean() | no_return().
%% ====================================================================
provider_exists(Key) ->
    exists(Key, dao_providers, exist_provider).


%% save/1
%% ====================================================================
%% @doc Creates a new document or updates an existing one.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec save(Doc :: dao_record()) -> Id :: binary() | no_return().
%% ====================================================================
save(#veil_document{} = Doc) ->
    {ok, StrId} = save_doc(Doc),
    binary:list_to_bin(StrId);
save(Resource) ->
    save(#veil_document{record = Resource}).


%% space/1
%% ====================================================================
%% @doc Returns a space object from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec space(Key :: term()) -> space_info() | no_return().
%% ====================================================================
space(Key) ->
    #veil_document{record = Space} = space_doc(Key),
    Space.


%% user/1
%% ====================================================================
%% @doc Returns a user object from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec user(Key :: term()) -> user_info() | no_return().
%% ====================================================================
user(Key) ->
    #veil_document{record = User} = user_doc(Key),
    User.


%% group/1
%% ====================================================================
%% @doc Returns a group object from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec group(Key :: term()) -> group_info() | no_return().
%% ====================================================================
group(Key) ->
    #veil_document{record = Group} = group_doc(Key),
    Group.


%% token/1
%% ====================================================================
%% @doc Returns a token object from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec token(Key :: term()) -> token_info() | no_return().
%% ====================================================================
token(Key) ->
    #veil_document{record = Token} = token_doc(Key),
    Token.


%% provider/1
%% ====================================================================
%% @doc Returns a provider object from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec provider(Key :: term()) -> provider_info() | no_return().
%% ====================================================================
provider(Key) ->
    #veil_document{record = Provider} = provider_doc(Key),
    Provider.


%% space_doc/1
%% ====================================================================
%% @doc Returns a space document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec space_doc(Key :: term()) -> space_doc() | no_return().
%% ====================================================================
space_doc(Key) ->
    #veil_document{record = #space{}} = SpaceDoc = get_doc(Key, dao_spaces, get_space),
    SpaceDoc.


%% user_doc/1
%% ====================================================================
%% @doc Returns a user document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec user_doc(Key :: term()) -> user_doc() | no_return().
%% ====================================================================
user_doc(Key) ->
    #veil_document{record = #user{}} = UserDoc = get_doc(Key, dao_users, get_user),
    UserDoc.


%% group_doc/1
%% ====================================================================
%% @doc Returns a group document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec group_doc(Key :: term()) -> group_doc() | no_return().
%% ====================================================================
group_doc(Key) ->
    #veil_document{record = #user_group{}} = GroupDoc = get_doc(Key, dao_groups, get_group),
    GroupDoc.


%% token_doc/1
%% ====================================================================
%% @doc Returns a token document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec token_doc(Key :: term()) -> token_doc() | no_return().
%% ====================================================================
token_doc(Key) ->
    #veil_document{record = #token{}} = TokenDoc = get_doc(Key, dao_tokens, get_token),
    TokenDoc.


%% provider_doc/1
%% ====================================================================
%% @doc Returns a provider document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec provider_doc(Key :: term()) -> provider_doc() | no_return().
%% ====================================================================
provider_doc(Key) ->
    #veil_document{record = #provider{}} = ProviderDoc = get_doc(Key, dao_providers, get_provider),
    ProviderDoc.


%% user_remove/1
%% ====================================================================
%% @doc Removes a user document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec user_remove(Key :: term()) -> true | no_return().
%% ====================================================================
user_remove(Key) ->
    remove(Key, dao_users, remove_user).


%% space_remove/1
%% ====================================================================
%% @doc Removes a space document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec space_remove(Key :: term()) -> true | no_return().
%% ====================================================================
space_remove(Key) ->
    remove(Key, dao_spaces, remove_space).


%% group_remove/1
%% ====================================================================
%% @doc Removes a group document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec group_remove(Key :: term()) -> true | no_return().
%% ====================================================================
group_remove(Key) ->
    remove(Key, dao_groups, remove_group).


%% token_remove/1
%% ====================================================================
%% @doc Removes a token document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec token_remove(Key :: term()) -> true | no_return().
%% ====================================================================
token_remove(Key) ->
    remove(Key, dao_tokens, remove_token).


%% provider_remove/1
%% ====================================================================
%% @doc Removes a provider document from the database.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec provider_remove(Key :: term()) -> true | no_return().
%% ====================================================================
provider_remove(Key) ->
    remove(Key, dao_providers, remove_provider).


%% ====================================================================
%% Internal Functions
%% ====================================================================

%% exists/3
%% ====================================================================
%% @doc Returns whether a resource exists in the database. Internal function.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec exists(Key :: term(), Module :: dao_module(), Method :: atom()) ->
    boolean() | no_return().
%% ====================================================================
exists(Key, Module, Method) when is_binary(Key) ->
    exists(binary_to_list(Key),Module,Method);
exists(Key, Module, Method) ->
    {ok, Exists} = dao_lib:apply(Module, Method, [Key], 1),
    Exists.


%% save_doc/1
%% ====================================================================
%% @doc Creates a new document or updates an existing one. Internal function.
%% ====================================================================
-spec save_doc(Doc :: veil_doc()) ->
    {ok, uuid()} | {error, any()}.
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
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec get_doc(Key :: term(), Module :: dao_module(), Method :: atom()) ->
    veil_doc() | no_return().
%% ====================================================================
get_doc(Key, Module, Method) when is_binary(Key) ->
    get_doc(binary_to_list(Key),Module,Method);
get_doc(Key, Module, Method)  ->
    {ok, #veil_document{} = Doc} = dao_lib:apply(Module, Method, [Key], 1),
    Doc.


%% remove/3
%% ====================================================================
%% @doc Removes a document from the database. Internal function.
%% Throws exception when call to dao fails, or document doesn't exist.
%% @end
%% ====================================================================
-spec remove(Key :: term(), Module :: dao_module(), Method :: atom()) ->
    true | no_return().
%% ====================================================================
remove(Key, Module, Method) when is_binary(Key) ->
    remove(binary_to_list(Key), Module, Method);
remove(Key, Module, Method) ->
    ok = dao_lib:apply(Module, Method, [Key], 1),
    true.
