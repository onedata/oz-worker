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

-include("idp_group_mapping.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([get_by_criterion/1]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([add_session/2, remove_session/2, get_all_sessions/1]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_user{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type name() :: binary().
-type alias() :: undefined | binary().
-type email() :: binary().
-type linked_account() :: #linked_account{}.
-type criterion() :: {linked_account, {auth_config:idp(), SubjectId :: binary()}} |
    {email, email()} |
    {alias, alias()}.
-export_type([name/0, alias/0]).

% Delay before all session connections are terminated when user is deleted.
-define(SESSION_CLEANUP_GRACE_PERIOD, 3000).

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
    {ok, Sessions} = get_all_sessions(UserId),
    [session:delete(S, ?SESSION_CLEANUP_GRACE_PERIOD, false) || S <- Sessions],
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
    Fun = fun(Doc = #document{value = #od_user{emails = EmailList}}, Acc) ->
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

%%--------------------------------------------------------------------
%% @doc
%% Adds a new session for given user.
%% @end
%%--------------------------------------------------------------------
-spec add_session(id(), session:id()) -> ok | {error, term()}.
add_session(UserId, SessionId) ->
    Result = update(UserId, fun(User = #od_user{active_sessions = ActiveSessions}) ->
        {ok, User#od_user{active_sessions = [SessionId | ActiveSessions]}}
    end),
    case Result of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes a session of given user.
%% @end
%%--------------------------------------------------------------------
-spec remove_session(id(), session:id()) -> ok | {error, term()}.
remove_session(UserId, SessionId) ->
    Result = update(UserId, fun(User = #od_user{active_sessions = ActiveSessions}) ->
        {ok, User#od_user{active_sessions = ActiveSessions -- [SessionId]}}
    end),
    case Result of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns all sessions of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_all_sessions(id()) -> {ok, [session:id()]} | {error, term()}.
get_all_sessions(UserId) ->
    case ?MODULE:get(UserId) of
        {ok, #document{value = #od_user{active_sessions = ActiveSessions}}} ->
            {ok, ActiveSessions};
        {error, _} ->
            {ok, []}
    end.

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
    9.

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
    {record, lists:keydelete(chosen_provider, 1, lists:keyreplace(login, 1, Struct, {alias, string}))};
get_record_struct(7) ->
    % There are no changes, but all records must be marked dirty to recalculate
    % effective relations (as intermediaries computing logic has changed).
    get_record_struct(6);
get_record_struct(8) ->
    % * added entitlements field
    % * renamed email_list to emails
    % * linked_account:
    %       * modified the fields order
    %       * renamed groups to entitlements
    %       * renamed email_list to emails
    %       * renamed login to alias
    %       * added custom field.
    {record, [
        {name, string},
        {alias, string},
        {emails, [string]},
        {basic_auth_enabled, boolean},
        {linked_accounts, [{record, [
            {idp, atom},
            {subject_id, string},
            {name, string},
            {alias, string},
            {emails, [string]},
            {entitlements, [string]},
            {custom, {custom, {json_utils, encode, decode}}}
        ]}]},
        {entitlements, [string]},

        {default_space, string},
        {default_provider, string},

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
get_record_struct(9) ->
    % Changes:
    %   * new field - active sessions
    %   * the privileges are translated
    {record, [
        {name, string},
        {alias, string},
        {email_list, [string]},
        {basic_auth_enabled, boolean},
        {linked_accounts, [{record, [
            {idp, atom},
            {subject_id, string},
            {name, string},
            {alias, string},
            {emails, [string]},
            {entitlements, [string]},
            {custom, {custom, {json_utils, encode, decode}}}
        ]}]},
        {active_sessions, [string]}, % New field
        {default_space, string},
        {default_provider, string},
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
    ]}.


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

    NewLinkedAccounts = lists:map(fun({linked_account, ProviderId, UserId, OALogin, OAName, OAEmails, OAGroups}) ->
        {linked_account,
            ProviderId,
            UserId,
            OALogin,
            OAName,
            OAEmails,
            OAGroups
        }
    end, LinkedAccounts),

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

    {6, {od_user,
        Name,
        Login,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,

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
upgrade_record(6, User) ->
    {od_user,
        Name,
        Alias,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,

        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

        Groups,
        Spaces,
        HandleServices,
        Handles,

        _EffGroups,
        _EffSpaces,
        _EffProviders,
        _EffHandleServices,
        _EffHandles,

        _TopDownDirty
    } = User,

    {7, {od_user,
        Name,
        Alias,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,

        ClientTokens,
        SpaceAliases,

        OzPrivileges,
        EffOzPrivileges,

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
upgrade_record(7, User) ->
    {od_user,
        Name,
        Alias,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,

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

    TransformedLinkedAccounts = lists:map(fun(LinkedAccount) ->
        {linked_account, IdP, SubjectId, LALogin, LAName, LAEmailList, _LAGroups} = LinkedAccount,

        #linked_account{
            idp = IdP,
            subject_id = SubjectId,
            name = LAName,
            alias = LALogin,
            emails = LAEmailList,
            % Cannot be translated, but users will not lose their current entitlements
            % (resulting Onedata group id is the same as before)
            entitlements = []
        }
    end, LinkedAccounts),

    {8, {od_user,
        Name,
        Alias,
        EmailList,
        BasicAuthEnabled,
        TransformedLinkedAccounts,
        [],

        DefaultSpace,
        DefaultProvider,

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
upgrade_record(8, User) ->
    {od_user,
        Name,
        Alias,
        EmailList,
        BasicAuthEnabled,
        LinkedAccounts,

        DefaultSpace,
        DefaultProvider,

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

        _TopDownDirty
    } = User,

    TranslatePrivileges = fun(Privileges) ->
        lists:usort(lists:flatten(lists:map(fun
            (oz_users_list) -> [?OZ_USERS_LIST, ?OZ_USERS_VIEW];

            (oz_groups_list) -> [?OZ_GROUPS_LIST, ?OZ_GROUPS_VIEW];
            (oz_groups_list_users) -> ?OZ_GROUPS_LIST_RELATIONSHIPS;
            (oz_groups_list_groups) -> ?OZ_GROUPS_LIST_RELATIONSHIPS;
            (oz_groups_add_members) -> ?OZ_GROUPS_ADD_RELATIONSHIPS;
            (oz_groups_remove_members) -> ?OZ_GROUPS_REMOVE_RELATIONSHIPS;

            (oz_spaces_list) -> [?OZ_SPACES_LIST, ?OZ_SPACES_VIEW];
            (oz_spaces_list_users) -> ?OZ_SPACES_LIST_RELATIONSHIPS;
            (oz_spaces_list_groups) -> ?OZ_SPACES_LIST_RELATIONSHIPS;
            (oz_spaces_list_providers) -> ?OZ_SPACES_LIST_RELATIONSHIPS;
            (oz_spaces_add_members) -> ?OZ_SPACES_ADD_RELATIONSHIPS;
            (oz_spaces_remove_members) -> ?OZ_SPACES_REMOVE_RELATIONSHIPS;

            (oz_providers_list) -> [?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_VIEW];
            (oz_providers_list_users) -> ?OZ_PROVIDERS_LIST_RELATIONSHIPS;
            (oz_providers_list_groups) -> ?OZ_PROVIDERS_LIST_RELATIONSHIPS;
            (oz_providers_list_spaces) -> ?OZ_PROVIDERS_LIST_RELATIONSHIPS;

            (Other) -> Other
        end, Privileges)))
    end,

    {9, #od_user{
        name = Name,
        alias = Alias,
        emails = EmailList,
        basic_auth_enabled = BasicAuthEnabled,
        linked_accounts = LinkedAccounts,

        active_sessions = [],

        default_space = DefaultSpace,
        default_provider = DefaultProvider,

        client_tokens = ClientTokens,
        space_aliases = SpaceAliases,

        oz_privileges = TranslatePrivileges(OzPrivileges),
        eff_oz_privileges = TranslatePrivileges(EffOzPrivileges),

        groups = Groups,
        spaces = Spaces,
        handle_services = HandleServices,
        handles = Handles,
        eff_groups = EffGroups,
        eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHandleServices,
        eff_handles = EffHandles
    }}.