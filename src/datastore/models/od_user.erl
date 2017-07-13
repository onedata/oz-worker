%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
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
-behaviour(model_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_model.hrl").

-type doc() :: datastore:document().
-type info() :: #od_user{}.
-type id() :: binary().
-export_type([doc/0, info/0, id/0]).

-type name() :: binary().
-export_type([name/0]).

%% model_behaviour callbacks
-export([save/1, get/1, list/0, exists/1, delete/1, update/2, create/1,
    model_init/0, 'after'/5, before/4]).
-export([record_struct/1, record_upgrade/2]).

%% API
-export([get_by_criterion/1]).
-export([to_string/1]).

%%--------------------------------------------------------------------
%% @doc
%% Returns structure of the record in specified version.
%% @end
%%--------------------------------------------------------------------
-spec record_struct(datastore_json:record_version()) -> datastore_json:record_struct().
record_struct(1) ->
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
record_struct(2) ->
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
record_struct(3) ->
    {record, Struct} = record_struct(2),
    LinkedAccStruct = {linked_accounts, [{record, [
        {provider_id, atom},
        {user_id, string},
        {login, string},
        {name, string},
        {email_list, [string]}
    ]}]},
    {record, lists:keyreplace(connected_accounts, 1, Struct, LinkedAccStruct)};
record_struct(4) ->
    {record, Struct} = record_struct(3),
    LinkedAccStruct = {linked_accounts, [{record, [
        {provider_id, atom},
        {user_id, string},
        {login, string},
        {name, string},
        {email_list, [string]},
        {groups, [string]}
    ]}]},
    {record, lists:keyreplace(linked_accounts, 1, Struct, LinkedAccStruct)}.

%%%===================================================================
%%% model_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback save/1.
%% @end
%%--------------------------------------------------------------------
-spec save(datastore:document()) ->
    {ok, datastore:ext_key()} | datastore:generic_error().
save(Document) ->
    model:execute_with_default_context(?MODULE, save, [Document]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback update/2.
%% @end
%%--------------------------------------------------------------------
-spec update(datastore:ext_key(), Diff :: datastore:document_diff()) ->
    {ok, datastore:ext_key()} | datastore:update_error().
update(Key, Diff) ->
    model:execute_with_default_context(?MODULE, update, [Key, Diff]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback create/1.
%% @end
%%--------------------------------------------------------------------
-spec create(datastore:document()) ->
    {ok, datastore:ext_key()} | datastore:create_error().
create(Document) ->
    model:execute_with_default_context(?MODULE, create, [Document]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback get/1.
%% @end
%%--------------------------------------------------------------------
-spec get(datastore:ext_key()) -> {ok, datastore:document()} | datastore:get_error().
get(Key) ->
    model:execute_with_default_context(?MODULE, get, [Key]).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all records.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [datastore:document()]} | datastore:generic_error() | no_return().
list() ->
    model:execute_with_default_context(?MODULE, list, [?GET_ALL, []]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback delete/1.
%% @end
%%--------------------------------------------------------------------
-spec delete(datastore:ext_key()) -> ok | datastore:generic_error().
delete(Key) ->
    model:execute_with_default_context(?MODULE, delete, [Key]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback exists/1.
%% @end
%%--------------------------------------------------------------------
-spec exists(datastore:ext_key()) -> datastore:exists_return().
exists(Key) ->
    ?RESPONSE(model:execute_with_default_context(?MODULE, exists, [Key])).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback model_init/0.
%% @end
%%--------------------------------------------------------------------
-spec model_init() -> model_behaviour:model_config().
model_init() ->
    Hooks = record_location_hooks:get_hooks(),
    Config = ?MODEL_CONFIG(od_user_bucket, Hooks, ?GLOBALLY_CACHED_LEVEL),
    Config#model_config{
        version = 4,
        list_enabled = {true, return_errors},
        sync_enabled = true
    }.

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback 'after'/5.
%% @end
%%--------------------------------------------------------------------
-spec 'after'(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term(),
    ReturnValue :: term()) -> ok.
'after'(ModelName, Method, _Level, Context, ReturnValue) ->
    record_location_hooks:handle_after(ModelName, Method, Context, ReturnValue).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback before/4.
%% @end
%%--------------------------------------------------------------------
-spec before(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term()) -> ok | datastore:generic_error().
before(ModelName, Method, _Level, Context) ->
    record_location_hooks:handle_before(ModelName, Method, Context).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% Gets first user matching given criterion.
%% todo: change implementation to something fast (connected with VFS-1498)
%% @end
%%--------------------------------------------------------------------

-spec get_by_criterion(Criterion :: {linked_account, {ProviderID :: atom(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #document{}} | {error, any()}.

get_by_criterion({email, Value}) ->
    Filter = fun
        ('$end_of_table', Acc) ->
            {abort, Acc};
        (#document{value = #od_user{email_list = EmailList}} = Doc, Acc) ->
            case lists:member(Value, EmailList) of
                true -> {abort, [Doc | Acc]};
                false -> {next, Acc}
            end;
        (_, Acc) ->
            {next, Acc}
    end,
    case model:execute_with_default_context(?MODULE, list, [Filter, []]) of
        {ok, []} ->
            {error, {not_found, od_user}};
        {ok, [Result | _]} ->
            {ok, Result}
    end;


get_by_criterion({alias, Value}) ->
    Filter = fun
        ('$end_of_table', Acc) ->
            {abort, Acc};
        (#document{value = #od_user{alias = Alias}} = Doc, Acc) ->
            case Alias of
                Value -> {abort, [Doc | Acc]};
                _ -> {next, Acc}
            end;
        (_, Acc) ->
            {next, Acc}
    end,
    case model:execute_with_default_context(?MODULE, list, [Filter, []]) of
        {ok, []} ->
            {error, {not_found, od_user}};
        {ok, [Result | _]} ->
            {ok, Result}
    end;

get_by_criterion({linked_account, {ProviderID, UserID}}) ->
    Filter = fun
        ('$end_of_table', Acc) ->
            {abort, Acc};
        (#document{value = #od_user{linked_accounts = Accounts}} = Doc, Acc) ->
            Found = lists:any(fun
                (#linked_account{provider_id = PID, user_id = UID}) ->
                    case {PID, UID} of
                        {ProviderID, UserID} -> true;
                        _ -> false
                    end;
                (_) -> false
            end, Accounts),
            case Found of
                true -> {abort, [Doc | Acc]};
                _ -> {next, Acc}
            end;
        (_, Acc) ->
            {next, Acc}
    end,
    case model:execute_with_default_context(?MODULE, list, [Filter, []]) of
        {ok, []} ->
            {error, {not_found, od_user}};
        {ok, [Result | _]} ->
            {ok, Result}
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
%% Upgrades record from specified version.
%% @end
%%--------------------------------------------------------------------
-spec record_upgrade(datastore_json:record_version(), tuple()) ->
    {datastore_json:record_version(), tuple()}.
record_upgrade(1, User) ->
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
record_upgrade(2, User) ->
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

    {3, #od_user{
        name = Name,
        login = Login,
        alias = Alias,
        email_list = EmailList,
        basic_auth_enabled = BasicAuthEnabled,
        linked_accounts = LinkedAccounts,

        default_space = DefaultSpace,
        default_provider = DefaultProvider,
        chosen_provider = ChosenProvider,
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
    }};
record_upgrade(3, User) ->
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
            #linked_account{
                provider_id = ProviderId,
                user_id = UserId,
                login = OALogin,
                name = OAName,
                email_list = OAEmails,
                groups = []
            }
        end, ConnectedAccounts
    ),

    {4, #od_user{
        name = Name,
        login = Login,
        alias = Alias,
        email_list = EmailList,
        basic_auth_enabled = BasicAuthEnabled,
        linked_accounts = LinkedAccounts,

        default_space = DefaultSpace,
        default_provider = DefaultProvider,
        chosen_provider = ChosenProvider,
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
