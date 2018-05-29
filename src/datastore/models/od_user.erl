%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for od_user record - representing a user in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_user).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([get_by_criterion/1]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_user{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type name() :: binary().
-type alias() :: undefined | binary().
-type linked_account() :: #linked_account{}.
-type criterion() :: {linked_account, {auth_utils:idp(), UserId :: binary()}} |
                     {email, binary()} |
                     {alias, alias()}.
-export_type([name/0, alias/0]).

-define(CTX, #{
    model => ?MODULE,
    fold_enabled => true,
    sync_enabled => true
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates user.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Saves user.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns user by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(UserId) ->
    datastore_model:get(?CTX, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether user given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(UserId) ->
    datastore_model:exists(?CTX, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Updates user by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(UserId, Diff) ->
    datastore_model:update(?CTX, UserId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes user by ID.
%% WARNING: Must not be used directly, as deleting a user that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a user use user_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(UserId) ->
    datastore_model:delete(?CTX, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all users.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Gets first user matching given criterion.
%% @end
%%--------------------------------------------------------------------
-spec get_by_criterion(criterion()) -> {ok, doc()} | {error, term()}.
get_by_criterion({email, Value}) ->
    Fun = fun(Doc = #document{value = #od_user{email_list = EmailList}}, Acc) ->
        case lists:member(Value, EmailList) of
            true -> {stop, [Doc | Acc]};
            false -> {ok, Acc}
        end
    end,
    case datastore_model:fold(?CTX, Fun, []) of
        {ok, []} ->
            {error, not_found};
        {ok, [Doc | _]} ->
            {ok, Doc}
    end;
get_by_criterion({alias, Value}) ->
    Fun = fun(Doc = #document{value = #od_user{alias = Alias}}, Acc) ->
        case Alias of
            Value -> {stop, [Doc | Acc]};
            _ -> {ok, Acc}
        end
    end,
    case datastore_model:fold(?CTX, Fun, []) of
        {ok, []} ->
            {error, not_found};
        {ok, [Doc | _]} ->
            {ok, Doc}
    end;
get_by_criterion({linked_account, {ProviderID, UserID}}) ->
    Fun = fun(Doc = #document{value = #od_user{linked_accounts = Accounts}}, Acc) ->
        Found = lists:any(fun
            (#linked_account{idp = IDP, subject_id = UID}) ->
                case {IDP, UID} of
                    {ProviderID, UserID} -> true;
                    _ -> false
                end;
            (_) -> false
        end, Accounts),
        case Found of
            true -> {stop, [Doc | Acc]};
            _ -> {ok, Acc}
        end
    end,
    case datastore_model:fold(?CTX, Fun, []) of
        {ok, []} ->
            {error, not_found};
        {ok, [Doc | _]} ->
            {ok, Doc}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the user with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(UserId :: id()) -> binary().
to_string(UserId) ->
    <<"user:", UserId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    user_logic_plugin.

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    6.

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {name, string},
        {login, string},
        {basic_auth_enabled, boolean},
        {alias, string},
        {email_list, [string]},
        {connected_accounts, [{record, [
            {provider_id, atom},
            {user_id, string},
            {login, string},
            {name, string},
            {email_list, [string]}
        ]}]},
        {default_space, string},
        {default_provider, string},
        {chosen_provider, string},
        {client_tokens, [string]},
        {space_aliases, #{string => string}},
        {oz_privileges, [atom]},
        {eff_oz_privileges, [atom]},
        {groups, [string]},
        {spaces, [string]},
        {handle_services, [string]},
        {handles, [string]},
        {eff_groups, [string]},
        {eff_spaces, [string]},
        {eff_shares, [string]},
        {eff_providers, [string]},
        {eff_handle_services, [string]},
        {eff_handles, [string]},
        {top_down_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {name, string},
        {login, string},
        {alias, string},
        {email_list, [string]},
        {basic_auth_enabled, boolean},
        {connected_accounts, [{record, [
            {provider_id, atom},
            {user_id, string},
            {login, string},
            {name, string},
            {email_list, [string]}
        ]}]},
        {default_space, string},
        {default_provider, string},
        {chosen_provider, string},
        {client_tokens, [string]},
        {space_aliases, #{string => string}},
        {oz_privileges, [atom]},
        {eff_oz_privileges, [atom]},
        {groups, [string]},
        {spaces, [string]},
        {handle_services, [string]},
        {handles, [string]},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_spaces, #{string => [{atom, string}]}},
        {eff_providers, #{string => [{atom, string}]}},
        {eff_handle_services, #{string => [{atom, string}]}},
        {eff_handles, #{string => [{atom, string}]}},
        {top_down_dirty, boolean}
    ]};
get_record_struct(3) ->
    {record, Struct} = get_record_struct(2),
    LinkedAccStruct = {linked_accounts, [{record, [
        {provider_id, atom},
        {user_id, string},
        {login, string},
        {name, string},
        {email_list, [string]}
    ]}]},
    {record, lists:keyreplace(connected_accounts, 1, Struct, LinkedAccStruct)};
get_record_struct(4) ->
    {record, Struct} = get_record_struct(3),
    LinkedAccStruct = {linked_accounts, [{record, [
        {provider_id, atom},
        {user_id, string},
        {login, string},
        {name, string},
        {email_list, [string]},
        {groups, [string]}
    ]}]},
    {record, lists:keyreplace(linked_accounts, 1, Struct, LinkedAccStruct)};
get_record_struct(5) ->
    {record, Struct} = get_record_struct(4),
    LinkedAccStruct = {linked_accounts, [{record, [
        {idp, atom},
        {subject_id, string},
        {login, string},
        {name, string},
        {email_list, [string]},
        {groups, [string]}
    ]}]},
    {record, lists:keydelete(alias, 1, lists:keyreplace(linked_accounts, 1, Struct, LinkedAccStruct))};
get_record_struct(6) ->
    {record, Struct} = get_record_struct(5),
    % Remove chosen_provider field, rename login to alias
    {record, lists:keydelete(chosen_provider, 1, lists:keyreplace(login, 1, Struct, {alias, string}))}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, User) ->
    {od_user,
        Name,
        Login,
        BasicAuthEnabled,
        Alias,
        EmailList,
        ConnectedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,
        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        _EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        _EffGroups,
        _EffSpaces,
        _EffShares,
        _EffProviders,
        _EffHandleServices,
        _EffHandles,

        _TopDownDirty
    } = User,
    {2, {od_user,
        Name,
        Login,
        Alias,
        EmailList,
        BasicAuthEnabled,
        ConnectedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,
        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        [],

        Groups,
        Spaces,
        HandleServices,
        Handles,

        #{},
        #{},
        #{},
        #{},
        #{},

        true
    }};
upgrade_record(2, User) ->
    {od_user,
        Name,
        Login,
        Alias,
        EmailList,
        BasicAuthEnabled,
        ConnectedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,
        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        EffGroups,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty
    } = User,

    LinkedAccounts = lists:map(
        fun({oauth_account, ProviderId, UserId, OALogin, OAName, OAEmails}) ->
            {linked_account, ProviderId, UserId, OALogin, OAName, OAEmails}
        end, ConnectedAccounts
    ),

    {3, {od_user,
        Name,
        Login,
        Alias,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,
        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        EffGroups,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty
    }};
upgrade_record(3, User) ->
    {od_user,
        Name,
        Login,
        Alias,
        EmailList,
        BasicAuthEnabled,
        ConnectedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,
        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        EffGroups,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty
    } = User,

    LinkedAccounts = lists:map(
        fun({linked_account, ProviderId, UserId, OALogin, OAName, OAEmails}) ->
            {linked_account,
                ProviderId,
                UserId,
                OALogin,
                OAName,
                OAEmails,
                []
            }
        end, ConnectedAccounts
    ),

    {4, {od_user,
        Name,
        Login,
        Alias,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,
        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        EffGroups,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty
    }};
upgrade_record(4, User) ->
    {od_user,
        Name,
        Login,
        _Alias,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,
        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        EffGroups,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty
    } = User,

    NewLinkedAccounts = lists:map(
        fun({linked_account, ProviderId, UserId, OALogin, OAName, OAEmails, OAGroups}) ->
            #linked_account{
                idp = ProviderId,
                subject_id = UserId,
                login = OALogin,
                name = OAName,
                email_list = OAEmails,
                groups = OAGroups
            }
        end, LinkedAccounts
    ),

    {5, {od_user,
        Name,
        Login,
        EmailList,
        BasicAuthEnabled,
        NewLinkedAccounts,

        DefaultSpace,
        DefaultProvider,
        ChosenProvider,

        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        EffGroups,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty
    }};
upgrade_record(5, User) ->
    {od_user,
        Name,
        Login,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,
        _ChosenProvider,

        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        EffGroups,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty
    } = User,

    {6, #od_user{
        name = Name,
        alias = Login,
        email_list = EmailList,
        basic_auth_enabled = BasicAuthEnabled,
        linked_accounts = LinkedAccounts,

        default_space = DefaultSpace,
        default_provider = DefaultProvider,

        client_tokens = ClientTokens,
        space_aliases = SpaceAliases,

        oz_privileges = OzPrivileges,
        eff_oz_privileges = EffOzPrivileges,

        groups = Groups,
        spaces = Spaces,
        handle_services = HandleServices,
        handles = Handles,

        eff_groups = EffGroups,
        eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHandleServices,
        eff_handles = EffHandles,

        top_down_dirty = TopDownDirty
    }}.
