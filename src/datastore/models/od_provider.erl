%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for provider record - representing a provider in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_provider).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([get_ctx/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_provider{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type name() :: binary().
-export_type([doc/0, name/0]).

-define(CTX, #{
    model => od_provider,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates provider.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns provider by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(ProviderId) ->
    datastore_model:get(?CTX, ProviderId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether provider given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(ProviderId) ->
    datastore_model:exists(?CTX, ProviderId).

%%--------------------------------------------------------------------
%% @doc
%% Updates provider by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(ProviderId, Diff) ->
    datastore_model:update(?CTX, ProviderId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes provider by ID.
%% WARNING: Must not be used directly, as deleting a provider that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a provider use provider_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(ProviderId) ->
    datastore_model:delete(?CTX, ProviderId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all providers.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the provider with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(ProviderId :: id()) -> binary().
to_string(ProviderId) ->
    <<"provider:", ProviderId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    provider_logic_plugin.

-spec get_ctx() -> datastore:ctx().
get_ctx() ->
    ?CTX.

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
    7.

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {client_name, string},
        {redirection_point, string},
        {urls, [string]},
        {serial, string},
        {latitude, float},
        {longitude, float},
        {spaces, [string]},
        {eff_users, [string]},
        {eff_groups, [string]},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {name, string},
        {redirection_point, string},
        {urls, [string]},
        {serial, string},
        {latitude, float},
        {longitude, float},
        {spaces, #{string => integer}},
        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(3) ->
    {record, [
        {name, string},
        {admin_email, string},
        % 'serial' field changes to 'root_macaroon'
        {root_macaroon, string},
        {subdomain_delegation, boolean},
        {domain, string},
        {subdomain, string},
        {latitude, float},
        {longitude, float},
        {spaces, #{string => integer}},
        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(4) ->
    % There are no changes, but all records must be marked dirty to recalculate
    % effective relations (as intermediaries computing logic has changed).
    get_record_struct(3);
get_record_struct(5) ->
    % Changes:
    %   * new field - creation_time
    %   * new field - eff harvesters
    {record, [
        {name, string},
        {admin_email, string},
        {root_macaroon, string},

        {subdomain_delegation, boolean},
        {domain, string},
        {subdomain, string},

        {latitude, float},
        {longitude, float},

        {spaces, #{string => integer}},

        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}}, % New field

        {creation_time, integer}, % New field

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(6) ->
    % Changes:
    %   * new field - last_activity
    {record, [
        {name, string},
        {admin_email, string},
        {root_macaroon, string},

        {subdomain_delegation, boolean},
        {domain, string},
        {subdomain, string},

        {latitude, float},
        {longitude, float},

        {spaces, #{string => integer}},

        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}},

        {creation_time, integer},
        {last_activity, integer}, % new field

        {bottom_up_dirty, boolean}
    ]};
get_record_struct(7) ->
    % * root_macaroon renamed to root_token
    % * renamed field - spaces -> legacy_spaces
    % * new field - storages
    % * new field - eff spaces
    {record, [
        {name, string},
        {admin_email, string},
        {root_token, string},

        {subdomain_delegation, boolean},
        {domain, string},
        {subdomain, string},

        {latitude, float},
        {longitude, float},

        {legacy_spaces, #{string => integer}}, % Renamed field
        {storages, [string]}, % New field

        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_spaces, #{string => {integer, [{atom, string}]}}}, % New field
        {eff_harvesters, #{string => [{atom, string}]}},

        {creation_time, integer},
        {last_activity, integer},

        {bottom_up_dirty, boolean}
    ]}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Provider) ->
    {
        od_provider,
        Name,
        RedirectionPoint,
        Urls,
        Serial,
        Latitude,
        Longitude,

        Spaces,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = Provider,
    {2, {od_provider,
        Name,
        RedirectionPoint,
        Urls,
        Serial,
        Latitude,
        Longitude,

        % Set support sizes to 0 as there is no access to this information
        % from here.
        maps:from_list([{SpaceId, 0} || SpaceId <- Spaces]),

        #{},
        #{},

        true
    }};
upgrade_record(2, Provider) ->
    {
        od_provider,
        Name,
        RedirectionPoint,
        _Urls,
        _Serial,
        Latitude,
        Longitude,

        Spaces,

        EffUsers,
        EffGroups,

        BottomUpDirty
    } = Provider,
    #{host := Domain} = url_utils:parse(RedirectionPoint),
    {3, {od_provider,
        Name,
        undefined,
        undefined,
        false,
        Domain,
        undefined,

        Latitude,
        Longitude,

        Spaces,

        EffUsers,
        EffGroups,

        BottomUpDirty
    }};
upgrade_record(3, Provider) ->
    {od_provider,
        Name,
        AdminEmail,
        RootMacaroon,
        SubdomainDelegation,
        Domain,
        Subdomain,

        Latitude,
        Longitude,

        Spaces,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = Provider,

    {4, {od_provider,
        Name,
        AdminEmail,
        RootMacaroon,
        SubdomainDelegation,
        Domain,
        Subdomain,

        Latitude,
        Longitude,

        Spaces,

        #{},
        #{},

        true
    }};
upgrade_record(4, Provider) ->
    {od_provider,
        Name,
        AdminEmail,
        RootMacaroon,
        SubdomainDelegation,
        Domain,
        Subdomain,

        Latitude,
        Longitude,

        Spaces,

        EffUsers,
        EffGroups,

        BottomUpDirty
    } = Provider,
    {5, {od_provider,
        Name,
        AdminEmail,
        RootMacaroon,
        SubdomainDelegation,
        Domain,
        Subdomain,

        Latitude,
        Longitude,

        Spaces,

        EffUsers,
        EffGroups,
        #{},

        time_utils:timestamp_seconds(),

        BottomUpDirty
    }};
upgrade_record(5, Provider) ->
    {od_provider,
        Name,
        AdminEmail,
        RootMacaroon,
        SubdomainDelegation,
        Domain,
        Subdomain,

        Latitude,
        Longitude,

        Spaces,

        EffUsers,
        EffGroups,
        EffHarvesters,

        CreationTime,

        BottomUpDirty
    } = Provider,
    {6, {od_provider,
        Name,
        AdminEmail,
        RootMacaroon,
        SubdomainDelegation,
        Domain,
        Subdomain,

        Latitude,
        Longitude,

        Spaces,

        EffUsers,
        EffGroups,
        EffHarvesters,

        CreationTime,
        0,

        BottomUpDirty
    }};
upgrade_record(6, Provider) ->
    {od_provider,
        Name,
        AdminEmail,
        RootMacaroon,
        SubdomainDelegation,
        Domain,
        Subdomain,

        Latitude,
        Longitude,

        Spaces,

        _EffUsers,
        _EffGroups,
        _EffHarvesters,

        CreationTime,
        LastActivity,

        _BottomUpDirty
    } = Provider,
    %% Eff relations are recalculated during cluster upgrade procedure
    {7, #od_provider{
        name = Name,
        admin_email = AdminEmail,
        root_token = RootMacaroon,
        subdomain_delegation = SubdomainDelegation,
        domain = Domain,
        subdomain = Subdomain,

        latitude = Latitude,
        longitude = Longitude,

        legacy_spaces = Spaces,
        storages = [],

        eff_users = #{},
        eff_groups = #{},
        eff_harvesters = #{},
        eff_spaces = #{},

        creation_time = CreationTime,
        last_activity = LastActivity,

        bottom_up_dirty = true
    }}.
