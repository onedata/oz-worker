%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides API samples for basic operation on spaces.
%%% API samples are generated for each space separately and the space Id
%%% is automatically included in the samples.
%%%
%%% NOTE: API samples do not cover all available API, only the commonly used
%%% endpoints and options.
%%% @end
%%%-------------------------------------------------------------------
-module(space_api_samples).
-author("Lukasz Opiola").

-include_lib("ctool/include/api_samples/common.hrl").


-export([generate_for/1]).


%%%===================================================================
%%% API
%%%===================================================================


-spec generate_for(od_space:id()) -> json_utils:json_term().
generate_for(SpaceId) ->
    #{
        <<"rest">> => jsonable_record:to_json(gen_samples(SpaceId), rest_api_samples)
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec gen_samples(od_space:id()) -> rest_api_samples:record().
gen_samples(SpaceId) ->
    #rest_api_samples{
        api_root = oz_worker:get_rest_uri(<<"">>),
        samples = rest_api_endpoints(SpaceId)
    }.


%% @private
-spec rest_api_endpoints(od_space:id()) -> [rest_api_request_sample:record()].
rest_api_endpoints(SpaceId) ->
    [
        get_space_details_endpoint(SpaceId),
        list_space_privileges_endpoint(SpaceId),

        list_direct_space_users_endpoint(SpaceId),
        list_effective_space_users_endpoint(SpaceId),
        get_effective_space_user_details_endpoint(SpaceId),
        list_direct_user_space_privileges_endpoint(SpaceId),
        list_effective_user_space_privileges_endpoint(SpaceId),
        update_user_space_privileges_endpoint(SpaceId),

        list_direct_space_groups_endpoint(SpaceId),
        list_effective_space_groups_endpoint(SpaceId),
        get_effective_space_group_details_endpoint(SpaceId),
        list_direct_group_space_privileges_endpoint(SpaceId),
        list_effective_group_space_privileges_endpoint(SpaceId),
        update_group_space_privileges_endpoint(SpaceId),

        list_space_shares_endpoint(SpaceId)
    ].


%% @private
-spec get_space_details_endpoint(od_space:id()) -> rest_api_request_sample:record().
get_space_details_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"Get space details">>,
        description = <<"Returns the details about the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s", [SpaceId]),
        swagger_operation_id = <<"get_space">>
    }.


%% @private
-spec list_space_privileges_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_space_privileges_endpoint(_SpaceId) ->
    #rest_api_request_sample{
        name = <<"List all space privileges">>,
        description = <<"Returns the list of all available space privileges.">>,
        method = 'GET',
        path = <<"/spaces/privileges">>,
        requires_authorization = false,
        swagger_operation_id = <<"list_space_privileges">>
    }.


%% @private
-spec list_direct_space_users_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_direct_space_users_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List direct space users">>,
        description = <<"Returns the list of users that are direct members of the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/users", [SpaceId]),
        swagger_operation_id = <<"list_space_users">>
    }.


%% @private
-spec list_effective_space_users_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_effective_space_users_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List effective space users">>,
        description = <<"Returns the list of users that are effective members of the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/effective_users", [SpaceId]),
        swagger_operation_id = <<"list_effective_space_users">>
    }.


%% @private
-spec get_effective_space_user_details_endpoint(od_space:id()) -> rest_api_request_sample:record().
get_effective_space_user_details_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"Get effective space user details">>,
        description = <<"Returns the details about a specific effective user in the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/effective_users/$USER_ID", [SpaceId]),
        placeholders = #{
            <<"$USER_ID">> => <<"ID of the user.">>
        },
        swagger_operation_id = <<"get_effective_space_user">>
    }.


%% @private
-spec list_direct_user_space_privileges_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_direct_user_space_privileges_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List user's direct space privileges">>,
        description = <<"Returns the list of user's direct privileges in the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/users/$USER_ID/privileges", [SpaceId]),
        placeholders = #{
            <<"$USER_ID">> => <<"ID of the user.">>
        },
        swagger_operation_id = <<"list_user_space_privileges">>
    }.


%% @private
-spec list_effective_user_space_privileges_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_effective_user_space_privileges_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List user's effective space privileges">>,
        description = <<"Returns the list of user's effective privileges in the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/effective_users/$USER_ID/privileges", [SpaceId]),
        placeholders = #{
            <<"$USER_ID">> => <<"ID of the user.">>
        },
        swagger_operation_id = <<"list_effective_user_space_privileges">>
    }.


%% @private
-spec update_user_space_privileges_endpoint(od_space:id()) -> rest_api_request_sample:record().
update_user_space_privileges_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"Update user's space privileges">>,
        description = <<"Updates user's privileges in the space.">>,
        method = 'PATCH',
        path = str_utils:format_bin("/spaces/~s/users/$USER_ID/privileges", [SpaceId]),
        data = <<"{\"grant\": $PRIVS_TO_GRANT_LIST, \"revoke\": $PRIVS_TO_REVOKE_LIST}">>,
        headers = #{
            <<"content-type">> => <<"application/json">>
        },
        placeholders = #{
            <<"$USER_ID">> => <<"ID of the user.">>,
            <<"$PRIVS_TO_GRANT_LIST">> => <<"Names of privileges to grant (a JSON array of strings).">>,
            <<"$PRIVS_TO_REVOKE_LIST">> => <<"Names of privileges to revoke (a JSON array of strings).">>
        },
        swagger_operation_id = <<"update_user_space_privileges">>
    }.


%% @private
-spec list_direct_space_groups_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_direct_space_groups_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List direct space groups">>,
        description = <<"Returns the list of groups that are direct members of the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/groups", [SpaceId]),
        swagger_operation_id = <<"list_space_groups">>
    }.


%% @private
-spec list_effective_space_groups_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_effective_space_groups_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List effective space groups">>,
        description = <<"Returns the list of groups that are effective members of the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/effective_groups", [SpaceId]),
        swagger_operation_id = <<"list_effective_space_groups">>
    }.


%% @private
-spec get_effective_space_group_details_endpoint(od_space:id()) -> rest_api_request_sample:record().
get_effective_space_group_details_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"Get effective space group details">>,
        description = <<"Returns the details about a specific effective group in the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/effective_groups/$GROUP_ID", [SpaceId]),
        placeholders = #{
            <<"$GROUP_ID">> => <<"ID of the group.">>
        },
        swagger_operation_id = <<"get_effective_space_group">>
    }.


%% @private
-spec list_direct_group_space_privileges_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_direct_group_space_privileges_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List group's direct space privileges">>,
        description = <<"Returns the list of group's direct privileges in the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/groups/$GROUP_ID/privileges", [SpaceId]),
        placeholders = #{
            <<"$GROUP_ID">> => <<"ID of the group.">>
        },
        swagger_operation_id = <<"list_group_space_privileges">>
    }.


%% @private
-spec list_effective_group_space_privileges_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_effective_group_space_privileges_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List group's effective space privileges">>,
        description = <<"Returns the list of group's effective privileges in the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/effective_groups/$GROUP_ID/privileges", [SpaceId]),
        placeholders = #{
            <<"$GROUP_ID">> => <<"ID of the group.">>
        },
        swagger_operation_id = <<"list_effective_group_space_privileges">>
    }.


%% @private
-spec update_group_space_privileges_endpoint(od_space:id()) -> rest_api_request_sample:record().
update_group_space_privileges_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"Update group's space privileges">>,
        description = <<"Updates group's privileges in the space.">>,
        method = 'PATCH',
        path = str_utils:format_bin("/spaces/~s/groups/$GROUP_ID/privileges", [SpaceId]),
        data = <<"{\"grant\": $PRIVS_TO_GRANT_LIST, \"revoke\": $PRIVS_TO_REVOKE_LIST}">>,
        headers = #{
            <<"content-type">> => <<"application/json">>
        },
        placeholders = #{
            <<"$GROUP_ID">> => <<"ID of the group.">>,
            <<"$PRIVS_TO_GRANT_LIST">> => <<"Names of privileges to grant (a JSON array of strings).">>,
            <<"$PRIVS_TO_REVOKE_LIST">> => <<"Names of privileges to revoke (a JSON array of strings).">>
        },
        swagger_operation_id = <<"update_group_space_privileges">>
    }.


%% @private
-spec list_space_shares_endpoint(od_space:id()) -> rest_api_request_sample:record().
list_space_shares_endpoint(SpaceId) ->
    #rest_api_request_sample{
        name = <<"List space shares">>,
        description = <<"Returns the list of shares from the space.">>,
        method = 'GET',
        path = str_utils:format_bin("/spaces/~s/shares", [SpaceId]),
        swagger_operation_id = <<"list_space_shares">>
    }.
