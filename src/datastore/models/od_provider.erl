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
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1, print_summary/0, print_summary/1]).
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
-export_type([name/0]).

-define(CTX, #{
    model => od_provider,
    fold_enabled => true,
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
%% Saves provider.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

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
%% Prints all provider records to the console in a nicely-formatted manner.
%% Sorts the records in a default manner.
%% @end
%%--------------------------------------------------------------------
-spec print_summary() -> ok.
print_summary() ->
    print_summary(name).

%%--------------------------------------------------------------------
%% @doc
%% Prints all provider records to the console in a nicely-formatted manner.
%% Sorts the records by given attribute (specified by name or position).
%% @end
%%--------------------------------------------------------------------
-spec print_summary(id | status | name | domain | spaces | support | users | groups | pos_integer()) -> ok.
print_summary(id) -> print_summary(1);
print_summary(status) -> print_summary(2);
print_summary(name) -> print_summary(3);
print_summary(domain) -> print_summary(4);
print_summary(spaces) -> print_summary(5);
print_summary(support) -> print_summary(6);
print_summary(users) -> print_summary(7);
print_summary(groups) -> print_summary(8);
print_summary(SortPos) when is_integer(SortPos) ->
    {ok, Providers} = list(),
    ProviderAttrs = lists:map(fun(#document{key = Id, value = P}) ->
        {
            Id,
            case provider_connection:is_online(Id) of true -> "online"; false -> "-" end,
            P#od_provider.name,
            P#od_provider.domain,
            maps:size(P#od_provider.eff_spaces),
            lists:foldl(fun({Support, _}, TotalSupport) -> TotalSupport + Support end, 0, maps:values(P#od_provider.eff_spaces)),
            maps:size(P#od_provider.eff_users),
            maps:size(P#od_provider.eff_groups)
        }
    end, Providers),
    Sorted = lists:keysort(SortPos, ProviderAttrs),
    io:format("-------------------------------------------------------------------------------------------------------------------------------------------------------~n"),
    io:format("Id                                Status   Name                      Domain                              Spaces   Tot. support   Eff users   Eff groups~n"),
    io:format("-------------------------------------------------------------------------------------------------------------------------------------------------------~n"),
    lists:foreach(fun({Id, Status, Name, Domain, Spaces, Support, EffUsers, EffGroups}) ->
        io:format("~-33s ~-8s ~-25ts ~-35ts ~-8B ~-14s ~-11B ~-12B~n", [
            Id, Status, Name, Domain, Spaces, str_utils:format_byte_size(Support), EffUsers, EffGroups
        ])
    end, Sorted),
    io:format("-------------------------------------------------------------------------------------------------------------------------------------------------------~n"),
    io:format("~B providers in total~n", [length(Sorted)]).

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
    % * new field - creation_time
    % * new field - eff harvesters
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
    % * root_macaroon renamed to root_token
    % * removed field - spaces
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

        {storages, [string]}, % New field

        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_spaces, #{string => {integer, [{atom, string}]}}}, % New field
        {eff_harvesters, #{string => [{atom, string}]}},

        {creation_time, integer},

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

        time_utils:system_time_seconds(),

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

        _Spaces,

        EffUsers,
        EffGroups,
        EffHarvesters,

        CreationTime,

        BottomUpDirty
    } = Provider,
    %% @TODO VFS-5854 Implement upgrade procedure using cluster upgrade
    {6, #od_provider{
        name = Name,
        admin_email = AdminEmail,
        root_token = RootMacaroon,
        subdomain_delegation = SubdomainDelegation,
        domain = Domain,
        subdomain = Subdomain,

        latitude = Latitude,
        longitude = Longitude,

        storages = [],

        eff_users = EffUsers,
        eff_groups = EffGroups,
        eff_harvesters = EffHarvesters,
        eff_spaces = #{},

        creation_time = CreationTime,

        bottom_up_dirty = BottomUpDirty
    }}.
