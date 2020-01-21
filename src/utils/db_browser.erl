%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Functions for viewing the Onezone database contents using the
%%% erlang VM console. Includes an interface for bash script.
%%% @end
%%%-------------------------------------------------------------------
-module(db_browser).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

% Denotes certain collection, e.g. 'users' or {'space_groups', <<"space_id">>}
-type collection() :: atom() | {atom(), binary()}.
% List of entity ids in the collection, or 'all' for all known ids.
-type entries() :: all | [gri:entity_id()].
% Basic table types - all collections reuse these tables to display the data
-type table_type() :: users | groups | spaces | shares | providers | clusters
| handle_services | handles | harvesters.
% Id of a column in displayed table, also used as display name
-type column_id() :: atom().
% Number of the column, counting from the left, starting with 1
-type column_number() :: pos_integer().
% By which column should data be sorted
-type sort_by() :: default | column_id().
% ascending | descending
-type sort_order() :: asc | desc.
% Type of the data in column, used for formatting
-type data_type() :: text | integer
| {boolean, TrueStr :: string(), FalseStr :: string()}
| creation_date | byte_size | direct_and_eff
| {privileges, privileges:privileges(any())} | admin_privileges.
% Value of a field, corresponds to the data_type()
-type value() :: binary() | integer() | {integer(), integer()}.
% Width in chars
-type width() :: pos_integer().
% Function used to retrieve the value for a field
-type get_value_fun() :: fun((datastore:doc()) -> value()).
% Specification of a single field in a row
-type field_spec() :: {column_id(), data_type(), width(), get_value_fun()}.

% Padding between columns
-define(PADDING, "  ").
-define(TO_BIN(Term), str_utils:to_binary(Term)).

%% API
-export([users/0]).
-export([groups/0]).
-export([spaces/0]).
-export([shares/0]).
-export([providers/0]).
-export([clusters/0]).
-export([handle_services/0]).
-export([handles/0]).
-export([harvesters/0]).

-export([pr/1, pr/2, pr/3]).
-export([format/1, format/2, format/3]).
-export([call_from_script/1]).
-export([print_help/0]).
-export([all_collections/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec users() -> ok.
users() -> pr(users).

-spec groups() -> ok.
groups() -> pr(groups).

-spec spaces() -> ok.
spaces() -> pr(spaces).

-spec shares() -> ok.
shares() -> pr(shares).

-spec providers() -> ok.
providers() -> pr(providers).

-spec clusters() -> ok.
clusters() -> pr(clusters).

-spec handle_services() -> ok.
handle_services() -> pr(handle_services).

-spec handles() -> ok.
handles() -> pr(handles).

-spec harvesters() -> ok.
harvesters() -> pr(harvesters).


%%--------------------------------------------------------------------
%% @doc
%% Prints requested collection to the console in form of a table.
%% @end
%%--------------------------------------------------------------------
-spec pr(collection()) -> ok.
pr(Collection) ->
    pr(Collection, default).

-spec pr(collection(), sort_by() | sort_order()) -> ok.
pr(Collection, asc) ->
    pr(Collection, default, asc);
pr(Collection, desc) ->
    pr(Collection, default, desc);
pr(Collection, SortBy) ->
    pr(Collection, SortBy, asc).

-spec pr(collection(), sort_by(), sort_order()) -> ok.
pr(Collection, SortBy, SortOrder) ->
    catch io:format("~s", [format(Collection, SortBy, SortOrder)]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns a formatted string representing a collection in form of a table.
%% @end
%%--------------------------------------------------------------------
-spec format(collection()) -> string().
format(Collection) ->
    format(Collection, default).

-spec format(collection(), sort_by() | sort_order()) -> string().
format(Collection, asc) ->
    format(Collection, default, asc);
format(Collection, desc) ->
    format(Collection, default, desc);
format(Collection, SortBy) ->
    format(Collection, SortBy, asc).

-spec format(collection(), sort_by(), sort_order()) -> string().
format(Collection, SortBy, SortOrder) ->
    try
        parse_and_format_collection(Collection, SortBy, SortOrder)
    catch Type:Reason ->
        io:format(
            "~s crashed with ~w:~w.~n"
            "Stacktrace: ~ts~n"
            "~n"
            "~s",
            [
                ?MODULE, Type, Reason,
                lager:pr_stacktrace(erlang:get_stacktrace()),
                format_help()
            ]
        ),
        error(badarg)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Interface for db_browser.sh script that can be used without manually
%% attaching to the erlang console.
%% @end
%%--------------------------------------------------------------------
-spec call_from_script(ArgsString :: string()) -> ok.
call_from_script(ArgsString) ->
    try string:split(ArgsString, " ", all) of
        [A] -> pr(list_to_atom(A));
        [A, B] -> pr(list_to_atom(A), list_to_atom(B));
        [A, B, C] -> pr(list_to_atom(A), list_to_atom(B), list_to_atom(C));
        _ -> print_help()
    catch _:_ ->
        print_help()
    end.


-spec print_help() -> ok.
print_help() ->
    io:format("~s", [format_help()]).


-spec all_collections() -> [collection()].
all_collections() -> [
    users, groups, spaces, shares, providers, clusters, handle_services, handles, harvesters,

    {user_groups, <<"user_id">>}, {user_spaces, <<"user_id">>},
    {user_providers, <<"user_id">>}, {user_clusters, <<"user_id">>},
    {user_handle_services, <<"user_id">>}, {user_handles, <<"user_id">>},
    {user_harvesters, <<"user_id">>},

    {group_users, <<"group_id">>}, {group_children, <<"group_id">>},
    {group_parents, <<"group_id">>}, {group_spaces, <<"group_id">>},
    {group_providers, <<"group_id">>}, {group_clusters, <<"group_id">>},
    {group_handle_services, <<"group_id">>}, {group_handles, <<"group_id">>},
    {group_harvesters, <<"group_id">>},

    {space_users, <<"space_id">>}, {space_groups, <<"space_id">>},
    {space_shares, <<"space_id">>}, {space_providers, <<"space_id">>},
    {space_harvesters, <<"space_id">>},

    {provider_users, <<"provider_id">>}, {provider_groups, <<"provider_id">>},
    {provider_spaces, <<"provider_id">>}, {provider_harvesters, <<"provider_id">>},

    {cluster_users, <<"cluster_id">>}, {cluster_groups, <<"cluster_id">>},

    {handle_service_users, <<"handle_service_id">>},
    {handle_service_groups, <<"handle_service_id">>},
    {handle_service_handles, <<"handle_service_id">>},

    {handle_users, <<"handle_id">>}, {handle_groups, <<"handle_id">>},

    {harvester_users, <<"harvester_id">>}, {harvester_groups, <<"harvester_id">>},
    {harvester_spaces, <<"harvester_id">>}, {harvester_providers, <<"harvester_id">>}
].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec format_help() -> string().
format_help() ->
    lists:flatten([
        str_utils:format("---------------------------------------------------------------------------~n", []),
        str_utils:format("~n"),
        str_utils:format("Usage (from console): ~s:pr/format(collection, [sort_by], [sort_order])~n", [?MODULE]),
        str_utils:format("             example: ~s:pr(help)    - print this help~n", [?MODULE]),
        str_utils:format("             example: ~s:pr(users)~n", [?MODULE]),
        str_utils:format("             example: ~s:pr(space_groups@da09aa798c7bd35d, type)~n", [?MODULE]),
        str_utils:format("             example: ~s:pr(handles, created, desc)~n", [?MODULE]),
        str_utils:format("~n"),
        str_utils:format("Usage  (from script): ~s.sh collection [sort_by] [sort_order]~n", [?MODULE]),
        str_utils:format("             example: ~s:sh help     - print this help~n", [?MODULE]),
        str_utils:format("             example: ~s:sh users~n", [?MODULE]),
        str_utils:format("             example: ~s:sh space_groups@da09aa798c7bd35d type~n", [?MODULE]),
        str_utils:format("             example: ~s:sh handles created desc~n", [?MODULE]),
        str_utils:format("~n"),
        str_utils:format("collection - collection to be printed, denoted by a single keyword or a pair~n"),
        str_utils:format("             keyword@entity_id, where entity_id should be substituted for an~n"),
        str_utils:format("             id of the corresponding entity (e.g. space_groups@da09aa798c7bd35d).~n"),
        str_utils:format("~n"),
        str_utils:format("sort_by [optional] - name of the column to be sorted (allowed values depend on the collection).~n"),
        str_utils:format("             Use 'default' for default column (the same as omitting the argument).~n"),
        str_utils:format("~n"),
        str_utils:format("sort_order [optional] - 'asc' or 'desc' (ascending or descending). Default: ascending.~n"),
        str_utils:format("~n"),
        str_utils:format("Available collections:~n"),
        lists:map(fun
            ({Collection, Id}) -> str_utils:format("    * ~s@~s~n", [Collection, Id]);
            (Collection) -> str_utils:format("    * ~s~n", [Collection])
        end, all_collections())
    ]).


%% @private
-spec parse_and_format_collection(collection(), sort_by(), sort_order()) -> string().
parse_and_format_collection(Collection, SortBy, SortOrder) ->
    Tokens = [string:trim(S) || S <- string:split(atom_to_list(Collection), "@", all)],
    ParsedCollection = case Tokens of
        [Res] -> list_to_atom(Res);
        [Res, Id] -> {list_to_atom(Res), list_to_binary(Id)}
    end,
    format_collection(ParsedCollection, SortBy, SortOrder).


%% @private
-spec format_collection(collection(), sort_by(), sort_order()) -> string().
format_collection(help, _SortBy, _SortOrder) ->
    format_help();


format_collection(TableType, SortBy, SortOrder) when is_atom(TableType) ->
    format_table(TableType, all, SortBy, SortOrder);


format_collection({user_groups, UserId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_user{groups = Groups, eff_groups = EffGroups}}} = od_user:get(UserId),
    format_memberships(groups, maps:keys(EffGroups), SortBy, SortOrder, Groups);

format_collection({user_spaces, UserId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_user{spaces = Spaces, eff_spaces = EffSpaces}}} = od_user:get(UserId),
    format_memberships(spaces, maps:keys(EffSpaces), SortBy, SortOrder, Spaces);

format_collection({user_providers, UserId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_user{eff_providers = EffProviders}}} = od_user:get(UserId),
    format_table(providers, maps:keys(EffProviders), SortBy, SortOrder);

format_collection({user_clusters, UserId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_user{clusters = Clusters, eff_clusters = EffClusters}}} = od_user:get(UserId),
    format_memberships(clusters, maps:keys(EffClusters), SortBy, SortOrder, Clusters);

format_collection({user_handle_services, UserId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_user{handle_services = HServices, eff_handle_services = EffHServices}}} = od_user:get(UserId),
    format_memberships(handle_services, maps:keys(EffHServices), SortBy, SortOrder, HServices);

format_collection({user_handles, UserId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_user{handles = Handles, eff_handles = EffHandles}}} = od_user:get(UserId),
    format_memberships(handles, maps:keys(EffHandles), SortBy, SortOrder, Handles);

format_collection({user_harvesters, UserId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_user{harvesters = Harvesters, eff_harvesters = EffHarvesters}}} = od_user:get(UserId),
    format_memberships(harvesters, maps:keys(EffHarvesters), SortBy, SortOrder, Harvesters);


format_collection({group_users, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{users = Users, eff_users = EffUsers}}} = od_group:get(GroupId),
    format_members(users, maps:keys(EffUsers), SortBy, SortOrder, Users, EffUsers, privileges:group_privileges());

format_collection({group_children, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{children = Chldrn, eff_children = EffChldrn}}} = od_group:get(GroupId),
    format_members(groups, maps:keys(EffChldrn), SortBy, SortOrder, Chldrn, EffChldrn, privileges:group_privileges());

format_collection({group_parents, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{parents = Parents, eff_parents = EffParents}}} = od_group:get(GroupId),
    format_memberships(groups, maps:keys(EffParents), SortBy, SortOrder, Parents);

format_collection({group_spaces, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{spaces = Spaces, eff_spaces = EffSpaces}}} = od_group:get(GroupId),
    format_memberships(spaces, maps:keys(EffSpaces), SortBy, SortOrder, Spaces);

format_collection({group_providers, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{eff_providers = EffProviders}}} = od_group:get(GroupId),
    format_table(providers, maps:keys(EffProviders), SortBy, SortOrder);

format_collection({group_clusters, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{clusters = Clusters, eff_clusters = EffClusters}}} = od_group:get(GroupId),
    format_memberships(clusters, maps:keys(EffClusters), SortBy, SortOrder, Clusters);

format_collection({group_handle_services, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{handle_services = HSrvs, eff_handle_services = EffHSrvs}}} = od_group:get(GroupId),
    format_memberships(handle_services, maps:keys(EffHSrvs), SortBy, SortOrder, HSrvs);

format_collection({group_handles, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{handles = Handles, eff_handles = EffHandles}}} = od_group:get(GroupId),
    format_memberships(handles, maps:keys(EffHandles), SortBy, SortOrder, Handles);

format_collection({group_harvesters, GroupId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_group{harvesters = Harvesters, eff_harvesters = EffHarvesters}}} = od_group:get(GroupId),
    format_memberships(harvesters, maps:keys(EffHarvesters), SortBy, SortOrder, Harvesters);


format_collection({space_users, SpaceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_space{users = Users, eff_users = EffUsers}}} = od_space:get(SpaceId),
    format_members(users, maps:keys(EffUsers), SortBy, SortOrder, Users, EffUsers, privileges:space_privileges());

format_collection({space_groups, SpaceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_space{groups = Groups, eff_groups = EffGroups}}} = od_space:get(SpaceId),
    format_members(groups, maps:keys(EffGroups), SortBy, SortOrder, Groups, EffGroups, privileges:space_privileges());

format_collection({space_shares, SpaceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_space{shares = Shares}}} = od_space:get(SpaceId),
    format_table(shares, Shares, SortBy, SortOrder);

format_collection({space_providers, SpaceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_space{providers = Providers}}} = od_space:get(SpaceId),
    format_table(providers, maps:keys(Providers), SortBy, SortOrder, [id, online, version, name, domain], [
        {support, byte_size, 11, fun(Doc) -> maps:get(SpaceId, Doc#document.value#od_provider.spaces) end}
    ]);

format_collection({space_harvesters, SpaceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_space{harvesters = Harvesters}}} = od_space:get(SpaceId),
    format_table(harvesters, Harvesters, SortBy, SortOrder);


format_collection({provider_users, ProviderId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_provider{eff_users = EffUsers}}} = od_provider:get(ProviderId),
    format_table(users, maps:keys(EffUsers), SortBy, SortOrder);

format_collection({provider_groups, ProviderId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_provider{eff_groups = EffGroups}}} = od_provider:get(ProviderId),
    format_table(groups, maps:keys(EffGroups), SortBy, SortOrder);

format_collection({provider_spaces, ProviderId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_provider{spaces = Spaces}}} = od_provider:get(ProviderId),
    format_table(spaces, maps:keys(Spaces), SortBy, SortOrder, [id, name, users, groups, providers], [
        {granted_support, byte_size, 15, fun(#document{key = SpaceId}) ->
            maps:get(SpaceId, Spaces)
        end},
        {total_support, byte_size, 13, fun(#document{value = Space}) ->
            lists:sum(maps:values(Space#od_space.providers))
        end}
    ]);

format_collection({provider_harvesters, ProviderId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_provider{eff_harvesters = Harvesters}}} = od_provider:get(ProviderId),
    format_table(harvesters, maps:keys(Harvesters), SortBy, SortOrder);


format_collection({cluster_users, ClusterId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_cluster{users = Users, eff_users = EffUsers}}} = od_cluster:get(ClusterId),
    format_members(users, maps:keys(EffUsers), SortBy, SortOrder, Users, EffUsers, privileges:cluster_privileges());

format_collection({cluster_groups, ClusterId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_cluster{groups = Groups, eff_groups = EffGroups}}} = od_cluster:get(ClusterId),
    format_members(groups, maps:keys(EffGroups), SortBy, SortOrder, Groups, EffGroups, privileges:cluster_privileges());


format_collection({handle_service_users, HServiceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_handle_service{users = Users, eff_users = EffUsers}}} = od_handle_service:get(HServiceId),
    format_members(users, maps:keys(EffUsers), SortBy, SortOrder, Users, EffUsers, privileges:handle_service_privileges());

format_collection({handle_service_groups, HServiceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_handle_service{groups = Groups, eff_groups = EffGroups}}} = od_handle_service:get(HServiceId),
    format_members(groups, maps:keys(EffGroups), SortBy, SortOrder, Groups, EffGroups, privileges:handle_service_privileges());

format_collection({handle_service_handles, HServiceId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_handle_service{handles = Handles}}} = od_handle_service:get(HServiceId),
    format_table(handles, Handles, SortBy, SortOrder);


format_collection({handle_users, HandleId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_handle{users = Users, eff_users = EffUsers}}} = od_handle:get(HandleId),
    format_members(users, maps:keys(EffUsers), SortBy, SortOrder, Users, EffUsers, privileges:handle_privileges());

format_collection({handle_groups, HandleId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_handle{groups = Groups, eff_groups = EffGroups}}} = od_handle:get(HandleId),
    format_members(groups, maps:keys(EffGroups), SortBy, SortOrder, Groups, EffGroups, privileges:handle_privileges());


format_collection({harvester_users, HarvesterId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_harvester{users = Users, eff_users = EffUsers}}} = od_harvester:get(HarvesterId),
    format_members(users, maps:keys(EffUsers), SortBy, SortOrder, Users, EffUsers, privileges:harvester_privileges());

format_collection({harvester_groups, HarvesterId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_harvester{groups = Groups, eff_groups = EffGroups}}} = od_harvester:get(HarvesterId),
    format_members(groups, maps:keys(EffGroups), SortBy, SortOrder, Groups, EffGroups, privileges:harvester_privileges());

format_collection({harvester_spaces, HarvesterId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_harvester{spaces = Spaces}}} = od_harvester:get(HarvesterId),
    format_table(spaces, Spaces, SortBy, SortOrder);

format_collection({harvester_providers, HarvesterId}, SortBy, SortOrder) ->
    {ok, #document{value = #od_harvester{eff_providers = Providers}}} = od_harvester:get(HarvesterId),
    format_table(providers, maps:keys(Providers), SortBy, SortOrder).


%% @private
-spec format_members(table_type(), entries(), sort_by(), sort_order(),
    DirectMembers :: list() | map(), EffMembers :: map(),
    AllPrivileges :: privileges:privileges(any())) -> string().
format_members(TableType, Entries, SortBy, SortOrder, DirectMembers, EffMembers, AllPrivileges) ->
    Fields = case TableType of
        users -> [id, full_name, username, email];
        groups -> [id, name, type, parents, children]
    end,
    format_table(TableType, Entries, SortBy, SortOrder, Fields, [
        {membership, {boolean, "direct", "effective"}, 10, fun(Doc) ->
            is_member(Doc#document.key, DirectMembers)
        end},
        {direct_privs, {privileges, AllPrivileges}, max(12, length(AllPrivileges)), fun(Doc) ->
            get_privileges(Doc#document.key, DirectMembers)
        end},
        {eff_privs, {privileges, AllPrivileges}, max(9, length(AllPrivileges)), fun(Doc) ->
            get_privileges(Doc#document.key, EffMembers)
        end}
    ]).


%% @private
-spec format_memberships(table_type(), entries(), sort_by(), sort_order(),
    DirectMemberships :: [gri:entity_id()]) -> string().
format_memberships(TableType, Entries, SortBy, SortOrder, DirectMemberships) ->
    format_table(TableType, Entries, SortBy, SortOrder, all, [
        {membership, {boolean, "direct", "effective"}, 10, fun(Doc) ->
            is_member(Doc#document.key, DirectMemberships)
        end}
    ]).


%% @private
-spec format_table(table_type(), entries(), sort_by(), sort_order()) ->
    string().
format_table(TableType, Entries, SortBy, SortOrder) ->
    format_table(TableType, Entries, SortBy, SortOrder, all, []).

%% @private
-spec format_table(table_type(), entries(), sort_by(), sort_order(),
    all | [column_id()], [field_spec()]) -> string().
format_table(TableType, Entries, SortBy, SortOrder, Fields, ExtraSpecs) ->
    Docs = fetch_docs(TableType, Entries),
    DefaultFieldSpecs = field_specs(TableType),
    RequestedFieldSpecs = case Fields of
        all -> DefaultFieldSpecs;
        _ -> [F || F = {ColumnId, _, _, _} <- DefaultFieldSpecs, lists:member(ColumnId, Fields)]
    end,
    FieldsToInclude = RequestedFieldSpecs ++ ExtraSpecs,
    ColumnsWidth = lists:sum([Width || {_, _, Width, _} <- FieldsToInclude]),
    TotalWidth = length(?PADDING) * (length(FieldsToInclude) - 1) + ColumnsWidth,
    RowValuesList = [get_row_values(Doc, FieldsToInclude) || Doc <- Docs],
    SortPos = find_sort_pos(TableType, SortBy, FieldsToInclude),
    SortedValues = lists:sort(fun(A, B) -> compare_values(A, B, SortPos, SortOrder) end, RowValuesList),
    lists:flatten([
        format_separator_line(TotalWidth),
        format_table_header(FieldsToInclude),
        format_separator_line(TotalWidth),
        [format_row(Row, FieldsToInclude) || Row <- SortedValues],
        format_separator_line(TotalWidth),
        str_utils:format("~B entries in total~n~n", [length(Docs)]),
        case bottom_note(TableType) of
            undefined -> [];
            BottomNode -> str_utils:format("~s~n~n", [BottomNode])
        end
    ]).


%% @private
-spec field_specs(table_type()) -> [field_spec()].
field_specs(users) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {full_name, text, 25, fun(Doc) -> Doc#document.value#od_user.full_name end},
    {username, text, 20, fun(Doc) ->
        case Doc#document.value#od_user.username of
            undefined -> <<"-">>;
            Username -> Username
        end
    end},
    {email, text, 30, fun(Doc) ->
        case Doc#document.value#od_user.emails of
            [] -> <<"-">>;
            Emails -> hd(Emails)
        end
    end},
    {groups, direct_and_eff, 9, fun(#document{value = User}) ->
        {length(User#od_user.groups), maps:size(User#od_user.eff_groups)}
    end},
    {spaces, direct_and_eff, 9, fun(#document{value = User}) ->
        {length(User#od_user.spaces), maps:size(User#od_user.eff_spaces)}
    end},
    {eff_providers, integer, 13, fun(Doc) -> maps:size(Doc#document.value#od_user.eff_providers) end},
    {admin_privs, admin_privileges, 11, fun(Doc) -> Doc#document.value#od_user.eff_oz_privileges end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_user.creation_time end}
];
field_specs(groups) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {name, text, 25, fun(Doc) -> Doc#document.value#od_group.name end},
    {type, text, 12, fun(Doc) -> Doc#document.value#od_group.type end},
    {parents, direct_and_eff, 9, fun(#document{value = Group}) ->
        {length(Group#od_group.parents), maps:size(Group#od_group.eff_parents)}
    end},
    {children, direct_and_eff, 9, fun(#document{value = Group}) ->
        {maps:size(Group#od_group.children), maps:size(Group#od_group.eff_children)}
    end},
    {users, direct_and_eff, 9, fun(#document{value = Group}) ->
        {maps:size(Group#od_group.users), maps:size(Group#od_group.eff_users)}
    end},
    {spaces, direct_and_eff, 9, fun(#document{value = Group}) ->
        {length(Group#od_group.spaces), maps:size(Group#od_group.eff_spaces)}
    end},
    {providers, integer, 9, fun(#document{value = Group}) ->
        maps:size(Group#od_group.eff_providers)
    end},
    {admin_privs, admin_privileges, 11, fun(Doc) -> Doc#document.value#od_group.eff_oz_privileges end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_group.creation_time end}
];
field_specs(spaces) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {name, text, 25, fun(Doc) -> Doc#document.value#od_space.name end},
    {users, direct_and_eff, 9, fun(#document{value = Space}) ->
        {maps:size(Space#od_space.users), maps:size(Space#od_space.eff_users)}
    end},
    {groups, direct_and_eff, 9, fun(#document{value = Space}) ->
        {maps:size(Space#od_space.groups), maps:size(Space#od_space.eff_groups)}
    end},
    {shares, integer, 6, fun(#document{value = Space}) ->
        length(Space#od_space.shares)
    end},
    {providers, integer, 11, fun(#document{value = Space}) ->
        maps:size(Space#od_space.providers)
    end},
    {support, byte_size, 11, fun(#document{value = Space}) ->
        lists:sum(maps:values(Space#od_space.providers))
    end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_space.creation_time end}
];
field_specs(shares) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {name, text, 25, fun(Doc) -> Doc#document.value#od_share.name end},
    {space, text, 38, fun(Doc) -> Doc#document.value#od_share.space end},
    {handle, text, 38, fun(Doc) -> Doc#document.value#od_share.handle end}
];
field_specs(providers) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {online, text, 6, fun(Doc) -> case provider_connection:is_online(Doc#document.key) of
        true -> "online";
        false -> "-"
    end end},
    {version, text, 14, fun(Doc) ->
        {ok, Version} = cluster_logic:get_worker_release_version(?ROOT, Doc#document.key),
        Version
    end},
    {name, text, 25, fun(Doc) -> Doc#document.value#od_provider.name end},
    {domain, text, 40, fun(Doc) -> Doc#document.value#od_provider.domain end},
    {spaces, integer, 6, fun(Doc) -> maps:size(Doc#document.value#od_provider.spaces) end},
    {support, byte_size, 11, fun(Doc) -> lists:sum(maps:values(Doc#document.value#od_provider.spaces)) end},
    {eff_users, integer, 9, fun(Doc) -> maps:size(Doc#document.value#od_provider.eff_users) end},
    {eff_groups, integer, 10, fun(Doc) -> maps:size(Doc#document.value#od_provider.eff_groups) end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_provider.creation_time end}
];
field_specs(clusters) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {type, text, 11, fun(Doc) -> Doc#document.value#od_cluster.type end},
    {name, text, 25, fun(Doc) -> case Doc#document.value#od_cluster.type of
        ?ONEZONE -> <<"@ ", (?TO_BIN(oz_worker:get_name()))/binary>>;
        ?ONEPROVIDER -> element(2, {ok, _} = provider_logic:get_name(?ROOT, Doc#document.key))
    end end},
    {users, direct_and_eff, 9, fun(#document{value = Cluster}) ->
        {maps:size(Cluster#od_cluster.users), maps:size(Cluster#od_cluster.eff_users)}
    end},
    {groups, direct_and_eff, 9, fun(#document{value = Cluster}) ->
        {maps:size(Cluster#od_cluster.groups), maps:size(Cluster#od_cluster.eff_groups)}
    end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_cluster.creation_time end}
];
field_specs(handle_services) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {name, text, 25, fun(Doc) -> Doc#document.value#od_handle_service.name end},
    {proxy_endpoint, text, 40, fun(Doc) -> Doc#document.value#od_handle_service.proxy_endpoint end},
    {handles, integer, 7, fun(#document{value = HService}) ->
        length(HService#od_handle_service.handles)
    end},
    {users, direct_and_eff, 9, fun(#document{value = HService}) ->
        {maps:size(HService#od_handle_service.users), maps:size(HService#od_handle_service.eff_users)}
    end},
    {groups, direct_and_eff, 9, fun(#document{value = HService}) ->
        {maps:size(HService#od_handle_service.groups), maps:size(HService#od_handle_service.eff_groups)}
    end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_handle_service.creation_time end}
];
field_specs(handles) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {public_handle, text, 45, fun(Doc) -> Doc#document.value#od_handle.public_handle end},
    {handle_service, text, 38, fun(Doc) -> Doc#document.value#od_handle.handle_service end},
    {share_id, text, 38, fun(Doc) -> Doc#document.value#od_handle.resource_id end},
    {users, direct_and_eff, 9, fun(#document{value = Handle}) ->
        {maps:size(Handle#od_handle.users), maps:size(Handle#od_handle.eff_users)}
    end},
    {groups, direct_and_eff, 9, fun(#document{value = Handle}) ->
        {maps:size(Handle#od_handle.groups), maps:size(Handle#od_handle.eff_groups)}
    end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_handle.creation_time end}
];
field_specs(harvesters) -> [
    {id, text, 38, fun(Doc) -> Doc#document.key end},
    {name, text, 25, fun(Doc) -> Doc#document.value#od_harvester.name end},
    {endpoint, text, 45, fun(Doc) -> Doc#document.value#od_harvester.endpoint end},
    {access, {boolean, "public", "private"}, 10, fun(Doc) -> Doc#document.value#od_harvester.public end},
    {spaces, integer, 6, fun(#document{value = Harvester}) ->
        length(Harvester#od_harvester.spaces)
    end},
    {users, direct_and_eff, 9, fun(#document{value = Harvester}) ->
        {maps:size(Harvester#od_harvester.users), maps:size(Harvester#od_harvester.eff_users)}
    end},
    {groups, direct_and_eff, 9, fun(#document{value = Harvester}) ->
        {maps:size(Harvester#od_harvester.groups), maps:size(Harvester#od_harvester.eff_groups)}
    end},
    {eff_providers, integer, 13, fun(Doc) -> maps:size(Doc#document.value#od_harvester.eff_providers) end},
    {created, creation_date, 10, fun(Doc) -> Doc#document.value#od_harvester.creation_time end}
].


%% @private
-spec bottom_note(table_type()) -> undefined | string().
bottom_note(shares) ->
    str_utils:format(
        "Public share URL is equal to: ~s",
        [share_logic:share_id_to_public_url(<<"${ID}">>)]
    );
bottom_note(_) ->
    undefined.


%% @private
-spec fetch_docs(table_type(), entries()) -> [datastore:doc()].
fetch_docs(TableType, all) ->
    Module = module(TableType),
    {ok, List} = Module:list(),
    List;
fetch_docs(TableType, Ids) ->
    Module = module(TableType),
    lists:map(fun(Id) ->
        {ok, Doc} = Module:get(Id),
        Doc
    end, Ids).


%% @private
-spec get_row_values(datastore:doc(), [field_spec()]) -> [value()].
get_row_values(Doc, FieldSpecs) ->
    lists:map(fun({_, _, _, GetValueFun}) ->
        GetValueFun(Doc)
    end, FieldSpecs).


%% @private
-spec format_separator_line(width()) -> string().
format_separator_line(Width) ->
    str_utils:format("~s~n", [lists:duplicate(Width, "-")]).


%% @private
-spec format_table_header([field_spec()]) -> string().
format_table_header(FieldSpecs) ->
    Header = lists:map(fun({ColumnId, _, Width, _}) ->
        str_utils:format("~-*s", [Width, ColumnId])
    end, FieldSpecs),
    str_utils:format("~s~n", [string:join(Header, ?PADDING)]).


%% @private
-spec format_row([value()], [field_spec()]) -> string().
format_row(RowValues, FieldSpecs) ->
    FormattedValues = lists:map(fun({Value, {_, DataType, Width, _}}) ->
        str_utils:format("~-*ts", [Width, format_value(DataType, Value)])
    end, lists:zip(RowValues, FieldSpecs)),
    str_utils:format("~ts~n", [string:join(FormattedValues, ?PADDING)]).


%% @private
-spec format_value(data_type(), value()) -> string().
format_value(text, Value) ->
    str_utils:format("~ts", [Value]);
format_value(integer, Value) ->
    str_utils:format("~B", [Value]);
format_value({boolean, TrueStr, FalseStr}, Value) ->
    case Value of
        true -> TrueStr;
        false -> FalseStr
    end;
format_value(creation_date, Value) ->
    {{Year, Month, Day}, _} = time_utils:epoch_to_datetime(Value),
    str_utils:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]);
format_value(byte_size, Value) ->
    str_utils:format_byte_size(Value);
format_value(direct_and_eff, {Direct, Effective}) ->
    str_utils:format("~B (~B)", [Direct, Effective]);
format_value({privileges, _AllPrivileges}, undefined) ->
    "";
format_value({privileges, AllPrivileges}, Privileges) ->
    lists:flatten(lists:map(fun(CurrentPriv) ->
        case lists:member(CurrentPriv, Privileges) of
            true -> "x";
            false -> "-"
        end
    end, AllPrivileges));
format_value(admin_privileges, []) ->
    "-";
format_value(admin_privileges, Privileges) ->
    str_utils:format("~B / ~B", [length(Privileges), length(privileges:oz_privileges())]).


%% @private
-spec find_sort_pos(table_type(), sort_by(), [field_spec()]) -> column_number().
find_sort_pos(users, default, FieldSpecs) ->
    find_sort_pos(users, full_name, FieldSpecs);
find_sort_pos(handles, default, FieldSpecs) ->
    find_sort_pos(handles, public_handle, FieldSpecs);
find_sort_pos(TableType, default, FieldSpecs) ->
    find_sort_pos(TableType, name, FieldSpecs);
find_sort_pos(TableType, ColumnId, FieldSpecs) ->
    Columns = [C || {C, _, _, _} <- FieldSpecs],
    ColumnsWithIndices = lists:zip(Columns, lists:seq(1, length(Columns))),
    case proplists:get_value(ColumnId, ColumnsWithIndices, undefined) of
        undefined -> find_sort_pos(TableType, default, FieldSpecs);
        Index -> Index
    end.


%% @private
-spec compare_values([value()], [value()], column_number(), sort_order()) -> boolean().
compare_values(RowValuesA, RowValuesB, SortPos, SortOrder) ->
    ValueA = case lists:nth(SortPos, RowValuesA) of
        BinA when is_binary(BinA) -> string:casefold(BinA);
        ValA -> ValA
    end,
    ValueB = case lists:nth(SortPos, RowValuesB) of
        BinB when is_binary(BinB) -> string:casefold(BinB);
        ValB -> ValB
    end,
    case SortOrder of
        asc -> ValueA < ValueB;
        desc -> ValueB < ValueA
    end.


% Checks membership in a direct or effective relation
%% @private
-spec is_member(gri:entity_id(), list() | map()) -> boolean().
is_member(MemberId, Members) when is_list(Members) -> lists:member(MemberId, Members);
is_member(MemberId, Members) when is_map(Members) -> maps:is_key(MemberId, Members).


% Returns privileges of a direct or effective relation
%% @private
-spec get_privileges(gri:entity_id(), list() | map()) -> [privileges:privileges(any())].
get_privileges(MemberId, MembersWithPrivs) ->
    case maps:get(MemberId, MembersWithPrivs, undefined) of
        {Privs, _} -> Privs;
        Privs -> Privs
    end.


%% @private
-spec module(table_type()) -> module().
module(users) -> od_user;
module(groups) -> od_group;
module(spaces) -> od_space;
module(shares) -> od_share;
module(providers) -> od_provider;
module(clusters) -> od_cluster;
module(handle_services) -> od_handle_service;
module(handles) -> od_handle;
module(harvesters) -> od_harvester.
