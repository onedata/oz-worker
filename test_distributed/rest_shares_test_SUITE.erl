%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of shares REST module in onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_shares_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


-export([all/0, init_per_suite/1, end_per_suite/1, end_per_testcase/2]).
-export([
    create_share_test/1,
    view_shares_test/1,
    modify_share_test/1,
    remove_share_test/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    ?ALL([
        create_share_test,
        view_shares_test,
        modify_share_test,
        remove_share_test
    ]).

%%%===================================================================
%%% Functions used to validate REST calls
%%%===================================================================
% Below functions are used in the tests for concise test code. They check if
% given REST operation ended with expected results.

% Tries to create a share for a user in given space and asserts if returned code
% matches the expected.
check_create_share(Config, Code, Issuer, SpaceId, ShareId, Parameters) ->
    ReqPath = [<<"/spaces/">>, SpaceId, <<"/shares/">>, ShareId],
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => put,
            path => ReqPath,
            body => Parameters,
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).

% Tries to retrieve share data for a user and asserts if returned code and
% body matches the expected.
check_get_share(Config, Code, Issuer, ShareId, ExpectedBody) ->
    ReqPath = [<<"/shares/">>, ShareId],
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => ReqPath,
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).

% Tries to retrieve share data for a user and asserts if returned code and
% body matches the expected.
check_get_shares_of_space(Config, Code, Issuer, SpaceId, ExpectedShareList) ->
    ReqPath = [<<"/spaces/">>, SpaceId, <<"/shares/">>],
    ExpectedBody = case ExpectedShareList of
        undefined -> undefined;
        _ -> #{<<"shares">> => ExpectedShareList}
    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => ReqPath,
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).

% Tries to update share data for a user by renaming the share and asserts if
% returned code and body matches the expected.
check_rename_share(Config, Code, Issuer, ShareId, Params) ->
    ReqPath = [<<"/shares/">>, ShareId],
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => patch,
            path => ReqPath,
            auth => Issuer,
            body => Params
        },
        expect => #{
            code => Code
        }
    }).

% Tries to remove a share for a user and asserts if
% returned code and body matches the expected.
check_remove_share(Config, Code, Issuer, ShareId) ->
    ReqPath = [<<"/shares/">>, ShareId],
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => delete,
            path => ReqPath,
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).


%%%===================================================================
%%% Test functions
%%%===================================================================
create_share_test(Config) ->
    % Create a user and a space
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space} = oz_test_utils:create_space(
        Config, ?USER(User), <<"sp">>
    ),
    % User should be able to create a share, as he has space_manages_shares
    % privilege by default in his space.
    ?assert(check_create_share(Config, 204, {user, User}, Space, <<"someShareId">>, #{
        <<"name">> => <<"whatever">>,
        <<"rootFileId">> => <<"whatever">>
    })),
    % Shares cannot be overwritten, so the same request should now fail
    ?assert(check_create_share(Config, 400, {user, User}, Space, <<"someShareId">>, #{
        <<"name">> => <<"whatever">>,
        <<"rootFileId">> => <<"whatever">>
    })),
    % Take the space_manages_shares privilege from user and try to create
    % another share, it should fail.
    oz_test_utils:space_set_user_privileges(
        Config, Space, User, set, [?SPACE_VIEW]
    ),
    ?assert(check_create_share(Config, 403, {user, User}, Space, <<"anotherShareId">>, #{
        <<"name">> => <<"whatever">>,
        <<"rootFileId">> => <<"whatever">>
    })),
    % User should be able to create shares again if we add him to a group that
    % has space_manages_shares privilege and belongs to the space.
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User), <<"gr">>),
    oz_test_utils:add_group_to_space(Config, Space, Group),
    oz_test_utils:space_set_group_privileges(
        Config, Space, Group, set, [?SPACE_MANAGE_SHARES]
    ),
    % Now the user should be able to create a share
    ?assert(check_create_share(Config, 204, {user, User}, Space, <<"anotherShareId">>, #{
        <<"name">> => <<"whatever">>,
        <<"rootFileId">> => <<"whatever">>
    })),
    ok.


view_shares_test(Config) ->
    % Create a user and a space
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space} = oz_test_utils:create_space(
        Config, ?USER(User), <<"sp">>
    ),
    % Create a share
    {Share1Id, Share1Name, Share1File} = {<<"s1id">>, <<"s1nm">>, <<"s1rf">>},
    ?assert(check_create_share(Config, 204, {user, User}, Space, Share1Id, #{
        <<"name">> => Share1Name,
        <<"rootFileId">> => Share1File
    })),
    % Retrieve it and validate the obtained data
    Share1ExpectedData = #{
        <<"shareId">> => Share1Id,
        <<"name">> => Share1Name,
        <<"spaceId">> => Space,
        <<"rootFileId">> => Share1File,
        % Public share URL is computed by OZ, so it is not provided in create,
        % but should be included in GET response
        <<"publicUrl">> => get_public_share_url(Config, Share1Id)
    },
    ?assert(check_get_share(Config, 200, {user, User}, Share1Id, Share1ExpectedData)),
    % Create some other shares
    {Share2Id, Share2Name, Share2File} = {<<"s2id">>, <<"s2nm">>, <<"s2rf">>},
    {Share3Id, Share3Name, Share3File} = {<<"s3id">>, <<"s3nm">>, <<"s3rf">>},
    ?assert(check_create_share(Config, 204, {user, User}, Space, Share2Id, #{
        <<"name">> => Share2Name,
        <<"rootFileId">> => Share2File
    })),
    ?assert(check_create_share(Config, 204, {user, User}, Space, Share3Id, #{
        <<"name">> => Share3Name,
        <<"rootFileId">> => Share3File
    })),
    % Retrieve them and validate the obtained data
    Share2ExpectedData = #{
        <<"shareId">> => Share2Id,
        <<"name">> => Share2Name,
        <<"spaceId">> => Space,
        <<"rootFileId">> => Share2File,
        % Public share URL is computed by OZ, so it is not provided in create,
        % but should be included in GET response
        <<"publicUrl">> => get_public_share_url(Config, Share2Id)
    },
    ?assert(check_get_share(Config, 200, {user, User}, Share2Id, Share2ExpectedData)),
    Share3ExpectedData = #{
        <<"shareId">> => Share3Id,
        <<"name">> => Share3Name,
        <<"spaceId">> => Space,
        <<"rootFileId">> => Share3File,
        % Public share URL is computed by OZ, so it is not provided in create,
        % but should be included in GET response
        <<"publicUrl">> => get_public_share_url(Config, Share3Id)
    },
    ?assert(check_get_share(Config, 200, {user, User}, Share3Id, Share3ExpectedData)),
    % Retrieve all shares of Space and check if they all all there
    ?assert(check_get_shares_of_space(Config, 200, {user, User}, Space, [
        Share1Id, Share2Id, Share3Id
    ])),
    % Remove the user from Space, he should no longer be able to view the shares
    % of the space nor each of the shares.
    oz_test_utils:user_leave_space(Config, User, Space),
    ?assert(check_get_share(Config, 403, {user, User}, Share1Id, undefined)),
    ?assert(check_get_share(Config, 403, {user, User}, Share2Id, undefined)),
    ?assert(check_get_share(Config, 403, {user, User}, Share3Id, undefined)),
    ?assert(check_get_shares_of_space(Config, 403, {user, User}, Space, undefined)),
    % However, when we add him to a group and the group to the Space, it should
    % be possible again.

    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User), <<"gr">>),
    oz_test_utils:add_group_to_space(Config, Space, Group),
    ?assert(check_get_share(Config, 200, {user, User}, Share1Id, Share1ExpectedData)),
    ?assert(check_get_share(Config, 200, {user, User}, Share2Id, Share2ExpectedData)),
    ?assert(check_get_share(Config, 200, {user, User}, Share3Id, Share3ExpectedData)),
    ?assert(check_get_shares_of_space(Config, 200, {user, User}, Space, [
        Share1Id, Share2Id, Share3Id
    ])),
    ok.


modify_share_test(Config) ->
    % Create a user and a space
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space} = oz_test_utils:create_space(
        Config, ?USER(User), <<"sp">>
    ),
    % Create a share
    {ShareId, ShareName, ShareFile} = {<<"s1id">>, <<"s1nm">>, <<"s1rf">>},
    ?assert(check_create_share(Config, 204, {user, User}, Space, ShareId, #{
        <<"name">> => ShareName,
        <<"rootFileId">> => ShareFile
    })),
    % Make sure share data is correct
    ShareExpectedData = #{
        <<"shareId">> => ShareId,
        <<"name">> => ShareName,
        <<"spaceId">> => Space,
        <<"rootFileId">> => ShareFile,
        % Public share URL is computed by OZ, so it is not provided in create,
        % but should be included in GET response
        <<"publicUrl">> => get_public_share_url(Config, ShareId)
    },
    ?assert(check_get_share(Config, 200, {user, User}, ShareId, ShareExpectedData)),
    % Try to modify share data (currently only rename is supported)
    NewName = <<"new name">>,
    % First, try wrong parameters
    ?assert(check_rename_share(
        Config, 400, {user, User}, ShareId, #{<<"wrong">> => <<"params">>}
    )),
    % Now correct ones
    ?assert(check_rename_share(
        Config, 204, {user, User}, ShareId, #{<<"name">> => NewName}
    )),
    % Retrieve share data and check if the name was changed
    ?assert(check_get_share(Config,
        200, {user, User}, ShareId, ShareExpectedData#{<<"name">> => NewName}
    )),
    % Take the space_manage_shares privilege from user and makes sure he no
    % longer can modify shares.
    oz_test_utils:space_set_user_privileges(
        Config, Space, User, set, [?SPACE_VIEW]
    ),
    EvenMoreNewName = <<"newest new name">>,
    ?assert(check_rename_share(
        Config, 403, {user, User}, ShareId, #{<<"name">> => EvenMoreNewName}
    )),
    % User should be able to rename shares again if we add him to a group that
    % has space_manages_shares privilege and belongs to the space.

    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User), <<"gr">>),
    oz_test_utils:add_group_to_space(Config, Space, Group),
    oz_test_utils:space_set_group_privileges(
        Config, Space, Group, set, [?SPACE_MANAGE_SHARES]
    ),
    % Now the user should be able to rename the share
    ?assert(check_rename_share(
        Config, 204, {user, User}, ShareId, #{<<"name">> => EvenMoreNewName}
    )),
    % Check if the data was updated
    ?assert(check_get_share(Config,
        200, {user, User}, ShareId, ShareExpectedData#{<<"name">> => EvenMoreNewName}
    )),
    ok.


remove_share_test(Config) ->
    % Create a user and a space
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space} = oz_test_utils:create_space(
        Config, ?USER(User), <<"sp">>
    ),
    % Create two shares
    {Share1Id, Share1Name, Share1File} = {<<"s1id">>, <<"s1nm">>, <<"s1rf">>},
    {Share2Id, Share2Name, Share2File} = {<<"s2id">>, <<"s2nm">>, <<"s2rf">>},
    ?assert(check_create_share(Config, 204, {user, User}, Space, Share1Id, #{
        <<"name">> => Share1Name,
        <<"rootFileId">> => Share1File
    })),
    ?assert(check_create_share(Config, 204, {user, User}, Space, Share2Id, #{
        <<"name">> => Share2Name,
        <<"rootFileId">> => Share2File
    })),
    % Try to remove a share
    ?assert(check_remove_share(Config, 202, {user, User}, Share1Id)),
    % Make sure the share 1 not longer exists (403 because the user is not
    % authorized to ask about non-existent share)
    ?assert(check_get_share(Config, 403, {user, User}, Share1Id, undefined)),
    ?assert(check_get_shares_of_space(Config, 200, {user, User}, Space, [Share2Id])),
    % Take the space_manage_shares privilege from user and makes sure he no
    % longer can remove shares.
    oz_test_utils:space_set_user_privileges(
        Config, Space, User, set, [?SPACE_VIEW]
    ),
    ?assert(check_remove_share(Config, 403, {user, User}, Share2Id)),
    % User should be able to remove shares again if we add him to a group that
    % has space_manages_shares privilege and belongs to the space.

    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User), <<"gr">>),
    oz_test_utils:add_group_to_space(Config, Space, Group),
    oz_test_utils:space_set_group_privileges(
        Config, Space, Group, set, [?SPACE_MANAGE_SHARES]
    ),
    % Now the user should be able to remove the share
    ?assert(check_remove_share(Config, 202, {user, User}, Share2Id)),
    % Make sure the share 2 not longer exists
    ?assert(check_get_share(Config, 403, {user, User}, Share2Id, undefined)),
    ?assert(check_get_shares_of_space(Config, 200, {user, User}, Space, [])),
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================
get_public_share_url(Config, ShareId) ->
    oz_test_utils:call_oz(
        Config, n_share_logic, share_id_to_public_url, [ShareId]
    ).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(etls).

end_per_testcase(_, Config) ->
    % Remove everything that was created during a testcase
    ok = oz_test_utils:delete_all_entities(Config).

