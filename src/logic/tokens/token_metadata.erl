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

% Information about a token
-type metadata() :: json_utils:json_term().
% Section in the token metadata that includes arbitrary custom metadata
-type custom_metadata() :: json_utils:json_term().
% Privileges carried by an invite token (stored in metadata)
-type carried_privileges() :: undefined | [atom()].
-export_type([metadata/0, custom_metadata/0, carried_privileges/0]).

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

-define(TO_ATOMS(Privileges), [binary_to_existing_atom(P, utf8) || P <- Privileges]).
-define(TO_BINARIES(Privileges), [atom_to_binary(P, utf8) || P <- Privileges]).

-export([build/3, update_custom_metadata/2]).
-export([is_usage_limit_reached/1]).
-export([increment_usage_count/1]).
-export([carried_invite_privileges/1]).
-export([default_invite_privileges/1]).
-export([optional_invite_token_parameters/1]).

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
build(Type, CustomMetadata, Data) ->
    BasicMetadata = #{
        ?CREATION_TIME_KEY => time_utils:cluster_time_seconds(),
        ?CUSTOM_KEY => CustomMetadata
    },

    case Type of
        ?ACCESS_TOKEN ->
            BasicMetadata;
        ?INVITE_TOKEN(InviteTokenType, _) ->
            Privileges = maps:get(?PRIVILEGES_KEY, Data, default_invite_privileges(InviteTokenType)),
            BasicMetadata#{
                ?PRIVILEGES_KEY => case Privileges of
                    undefined -> null;
                    [_ | _] -> ?TO_BINARIES(Privileges)
                end,
                ?USAGE_LIMIT_KEY => maps:get(?USAGE_LIMIT_KEY, Data, ?INF_USAGE_LIMIT),
                ?USAGE_COUNT_KEY => 0
            }
    end.


-spec update_custom_metadata(metadata(), custom_metadata()) -> metadata().
update_custom_metadata(Metadata, CustomMetadata) ->
    Metadata#{
        ?CUSTOM_KEY => CustomMetadata
    }.


-spec is_usage_limit_reached(metadata()) -> boolean().
is_usage_limit_reached(Metadata) ->
    UsageCount = maps:get(?USAGE_COUNT_KEY, Metadata, 0),
    UsageLimit = case maps:get(?USAGE_LIMIT_KEY, Metadata, ?INF_USAGE_LIMIT) of
        ?INF_USAGE_LIMIT -> 99999999999;
        Int when is_integer(Int) -> Int
    end,
    UsageCount >= UsageLimit.


-spec increment_usage_count(metadata()) -> metadata().
increment_usage_count(Metadata) ->
    UsageCount = maps:get(?USAGE_COUNT_KEY, Metadata, 0),
    Metadata#{
        ?USAGE_COUNT_KEY => UsageCount + 1
    }.


-spec carried_invite_privileges(metadata()) -> carried_privileges().
carried_invite_privileges(Metadata) ->
    case maps:get(?PRIVILEGES_KEY, Metadata, undefined) of
        undefined -> undefined;
        null -> undefined;
        Privileges -> ?TO_ATOMS(Privileges)
    end.


-spec default_invite_privileges(tokens:invite_token_type()) -> carried_privileges().
default_invite_privileges(?USER_JOIN_GROUP) -> privileges:group_member();
default_invite_privileges(?GROUP_JOIN_GROUP) -> privileges:group_member();
default_invite_privileges(?USER_JOIN_SPACE) -> privileges:space_member();
default_invite_privileges(?GROUP_JOIN_SPACE) -> privileges:space_member();
default_invite_privileges(?USER_JOIN_CLUSTER) -> privileges:cluster_admin(); %% @TODO VFS-5815 temp. solution
default_invite_privileges(?GROUP_JOIN_CLUSTER) -> privileges:cluster_member();
default_invite_privileges(?USER_JOIN_HARVESTER) -> privileges:harvester_member();
default_invite_privileges(?GROUP_JOIN_HARVESTER) -> privileges:harvester_member();
default_invite_privileges(_) -> undefined.


%%--------------------------------------------------------------------
%% @doc
%% Returns the possible optional parameters when creating an invite token of given type.
%% @end
%%--------------------------------------------------------------------
-spec optional_invite_token_parameters(tokens:invite_token_type()) ->
    #{Key :: binary() => {entity_logic:type_validator(), entity_logic:value_validator()}}.
optional_invite_token_parameters(InviteTokenType) ->
    AllowedPrivileges = case InviteTokenType of
        ?USER_JOIN_GROUP -> privileges:group_privileges();
        ?GROUP_JOIN_GROUP -> privileges:group_privileges();
        ?USER_JOIN_SPACE -> privileges:space_privileges();
        ?GROUP_JOIN_SPACE -> privileges:space_privileges();
        ?SUPPORT_SPACE -> none;
        ?REGISTER_ONEPROVIDER -> none;
        ?USER_JOIN_CLUSTER -> privileges:cluster_privileges();
        ?GROUP_JOIN_CLUSTER -> privileges:cluster_privileges();
        ?USER_JOIN_HARVESTER -> privileges:harvester_privileges();
        ?GROUP_JOIN_HARVESTER -> privileges:harvester_privileges();
        ?SPACE_JOIN_HARVESTER -> none
    end,
    maps:merge(
        #{?USAGE_LIMIT_KEY => {integer, {not_lower_than, 1}}},
        case AllowedPrivileges of
            none -> #{};
            _ -> #{?PRIVILEGES_KEY => {list_of_atoms, AllowedPrivileges}}
        end
    ).
