%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles manipulation of named token metadata. Each token has
%%% several information in the metadata depending on the token type:
%%%     * Access tokens
%%%         - creationTime :: UNIX timestamp
%%%     * Invite tokens
%%%         - creationTime :: UNIX timestamp
%%%         - usageLimit :: positive_integer | "infinity"
%%%         - usageCount :: non_neg_integer
%%%         - privileges :: [binary()]
%%% Apart from that, arbitrary custom metadata can be attached to any token
%%% and will be stored under the "custom" key in token's metadata.
%%% @end
%%%-------------------------------------------------------------------
-module(token_metadata).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").

%% @formatter:off
% Information about a token
-type metadata() :: json_utils:json_term().
% Section in the token metadata that includes arbitrary custom metadata
-type custom_metadata() :: json_utils:json_term().
% Privileges tied to an invite token - optional, stored in metadata
-type invite_privileges() :: undefined | privileges:privileges(
    privileges:group_privilege() | privileges:space_privilege() |
    privileges:handle_service_privilege() | privileges:handle_privilege() |
    privileges:harvester_privilege() | privileges:cluster_privilege()
).
% Indicates if invite privileges are default or custom for an invite token.
% Custom privileges require additional privileges from the token creator, apart
% from regular invite privileges.
-type privileges_profile() :: default_privileges | custom_privileges.
%% @formatter:on
-export_type([metadata/0, custom_metadata/0, invite_privileges/0, privileges_profile/0]).

% UNIX timestamp of the token creation time
-define(CREATION_TIME_KEY, <<"creationTime">>).
% section in the metadata where user's custom metadata is stored
-define(CUSTOM_KEY, <<"custom">>).
% what privileges are carried by an invite token
-define(PRIVILEGES_KEY, <<"privileges">>).
% the usage limit of an invite token
-define(USAGE_LIMIT_KEY, <<"usageLimit">>).
% the usage count of an invite token (how many times it has been consumed)
-define(USAGE_COUNT_KEY, <<"usageCount">>).
-define(INF_USAGE_LIMIT, <<"infinity">>).

-export([build/3, update_custom_metadata/2]).
-export([inspect_requested_privileges/2]).
-export([inspect_carried_privileges/2]).
-export([is_usage_limit_reached/1]).
-export([increment_usage_count/1]).
-export([optional_invite_token_parameters/1]).
-export([default_invite_privileges/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Builds the metadata of a new token with basic attributes and specific
%% information depending on the token creation parameters.
%% @end
%%--------------------------------------------------------------------
-spec build(tokens:type(), custom_metadata(), entity_logic:data()) -> metadata().
build(?ACCESS_TOKEN, CustomMetadata, _Data) ->
    basic_metadata(CustomMetadata);
build(?INVITE_TOKEN(InviteTokenType, _), CustomMetadata, Data) ->
    Privileges = case are_invite_privileges_applicable(InviteTokenType) of
        false ->
            undefined;
        true ->
            case maps:find(?PRIVILEGES_KEY, Data) of
                {ok, Privs} -> privileges:from_list(Privs);
                error -> default_invite_privileges(InviteTokenType)
            end
    end,

    BasicMetadata = basic_metadata(CustomMetadata),
    BasicMetadata#{
        ?PRIVILEGES_KEY => serialize_privileges(Privileges),
        ?USAGE_LIMIT_KEY => maps:get(?USAGE_LIMIT_KEY, Data, ?INF_USAGE_LIMIT),
        ?USAGE_COUNT_KEY => 0
    }.


%% @private
-spec basic_metadata(custom_metadata()) -> metadata().
basic_metadata(CustomMetadata) ->
    #{
        ?CREATION_TIME_KEY => time_utils:cluster_time_seconds(),
        ?CUSTOM_KEY => CustomMetadata
    }.


-spec update_custom_metadata(metadata(), custom_metadata()) -> metadata().
update_custom_metadata(Metadata, CustomMetadata) ->
    Metadata#{
        ?CUSTOM_KEY => CustomMetadata
    }.


%%--------------------------------------------------------------------
%% @doc
%% Inspects the Data provided during token creation and returns the profile of
%% the privileges requested to be included in an invite token.
%% @end
%%--------------------------------------------------------------------
-spec inspect_requested_privileges(tokens:invite_token_type(), entity_logic:data()) ->
    privileges_profile().
inspect_requested_privileges(InviteTokenType, Data) ->
    case {are_invite_privileges_applicable(InviteTokenType), Data} of
        {false, _} -> default_privileges;
        {true, #{?PRIVILEGES_KEY := _}} -> custom_privileges;
        {true, _} -> default_privileges
    end.


%%--------------------------------------------------------------------
%% @doc
%% Inspects the privileges carried by an invite token (based on Metadata) and
%% returns the resolved privileges along with their profile.
%% @end
%%--------------------------------------------------------------------
-spec inspect_carried_privileges(tokens:invite_token_type(), metadata()) ->
    {privileges_profile(), invite_privileges()}.
inspect_carried_privileges(InviteTokenType, Metadata) ->
    CarriedPrivs = deserialize_privileges(maps:get(?PRIVILEGES_KEY, Metadata, null)),
    DefaultPrivs = default_invite_privileges(InviteTokenType),
    % Privileges are already sorted; carried privileges were sorted during
    % metadata construction, and default privileges are sorted as the privileges
    % module operates on ordsets.
    case CarriedPrivs =:= DefaultPrivs of
        true -> {default_privileges, CarriedPrivs};
        false -> {custom_privileges, CarriedPrivs}
    end.


-spec is_usage_limit_reached(metadata()) -> boolean().
is_usage_limit_reached(Metadata) ->
    UsageCount = maps:get(?USAGE_COUNT_KEY, Metadata, 0),
    case maps:get(?USAGE_LIMIT_KEY, Metadata, ?INF_USAGE_LIMIT) of
        ?INF_USAGE_LIMIT ->
            false;
        UsageLimit when is_integer(UsageLimit) ->
            UsageCount >= UsageLimit
    end.


-spec increment_usage_count(metadata()) -> metadata().
increment_usage_count(Metadata) ->
    UsageCount = maps:get(?USAGE_COUNT_KEY, Metadata, 0),
    Metadata#{
        ?USAGE_COUNT_KEY => UsageCount + 1
    }.


%%--------------------------------------------------------------------
%% @doc
%% Returns the possible optional parameters when creating a named invite token of given type.
%% @end
%%--------------------------------------------------------------------
-spec optional_invite_token_parameters(tokens:invite_token_type()) ->
    #{Key :: binary() => {entity_logic:type_validator(), entity_logic:value_validator()}}.
optional_invite_token_parameters(InviteTokenType) ->
    maps:merge(
        #{?USAGE_LIMIT_KEY => {integer, {not_lower_than, 1}}},
        case allowed_invite_privileges(InviteTokenType) of
            undefined -> #{};
            Privileges -> #{?PRIVILEGES_KEY => {list_of_atoms, Privileges}}
        end
    ).


-spec default_invite_privileges(tokens:invite_token_type()) -> invite_privileges().
default_invite_privileges(?USER_JOIN_GROUP) -> privileges:group_member();
default_invite_privileges(?GROUP_JOIN_GROUP) -> privileges:group_member();
default_invite_privileges(?USER_JOIN_SPACE) -> privileges:space_member();
default_invite_privileges(?GROUP_JOIN_SPACE) -> privileges:space_member();
default_invite_privileges(?USER_JOIN_CLUSTER) -> privileges:cluster_admin(); %% @TODO VFS-5815 temp. solution
default_invite_privileges(?GROUP_JOIN_CLUSTER) -> privileges:cluster_member();
default_invite_privileges(?USER_JOIN_HARVESTER) -> privileges:harvester_member();
default_invite_privileges(?GROUP_JOIN_HARVESTER) -> privileges:harvester_member();
default_invite_privileges(_) -> undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec allowed_invite_privileges(tokens:invite_token_type()) -> invite_privileges().
allowed_invite_privileges(?USER_JOIN_GROUP) -> privileges:group_privileges();
allowed_invite_privileges(?GROUP_JOIN_GROUP) -> privileges:group_privileges();
allowed_invite_privileges(?USER_JOIN_SPACE) -> privileges:space_privileges();
allowed_invite_privileges(?GROUP_JOIN_SPACE) -> privileges:space_privileges();
allowed_invite_privileges(?USER_JOIN_CLUSTER) -> privileges:cluster_privileges();
allowed_invite_privileges(?GROUP_JOIN_CLUSTER) -> privileges:cluster_privileges();
allowed_invite_privileges(?USER_JOIN_HARVESTER) -> privileges:harvester_privileges();
allowed_invite_privileges(?GROUP_JOIN_HARVESTER) -> privileges:harvester_privileges();
allowed_invite_privileges(_) -> undefined.


%% @private
-spec are_invite_privileges_applicable(tokens:invite_token_type()) -> boolean().
are_invite_privileges_applicable(?USER_JOIN_GROUP) -> true;
are_invite_privileges_applicable(?GROUP_JOIN_GROUP) -> true;
are_invite_privileges_applicable(?USER_JOIN_SPACE) -> true;
are_invite_privileges_applicable(?GROUP_JOIN_SPACE) -> true;
are_invite_privileges_applicable(?USER_JOIN_CLUSTER) -> true;
are_invite_privileges_applicable(?GROUP_JOIN_CLUSTER) -> true;
are_invite_privileges_applicable(?USER_JOIN_HARVESTER) -> true;
are_invite_privileges_applicable(?GROUP_JOIN_HARVESTER) -> true;
are_invite_privileges_applicable(_) -> false.


%% @private
-spec serialize_privileges(invite_privileges()) -> null | [binary()].
serialize_privileges(undefined) -> null;
serialize_privileges(Privs) -> [atom_to_binary(P, utf8) || P <- Privs].


%% @private
-spec deserialize_privileges(null | [binary()]) -> invite_privileges().
deserialize_privileges(null) -> undefined;
deserialize_privileges(Privs) -> [binary_to_existing_atom(P, utf8) || P <- Privs].
