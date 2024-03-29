%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests related to space marketplace.
%%% NOTE: API tests are located in @see space_misc_api_test_SUITE.erl
%%% @end
%%%-------------------------------------------------------------------
-module(space_marketplace_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    submit_request_test/1,
    grant_test/1,
    reject_test/1,

    submit_error_limit_reached_test/1,
    submit_error_already_a_member_test/1,
    resolve_error_not_found_test/1,
    resolve_error_mailer_malfunction_test/1,
    get_membership_requester_info_forged_ids_test/1,

    filter_advertised_test/1,

    % sequential_tests - manipulate time or modify the advertised spaces commonly used by all the spaces
    submit_reminder_test/1,
    submit_reminder_error_too_soon_test/1,
    submit_reminder_error_marketplace_disabled_test/1,

    submit_another_request_test/1,
    submit_another_error_recently_rejected_test/1,
    submit_another_error_marketplace_disabled_test/1,
    submit_error_mailer_malfunction_test/1,
    submit_error_marketplace_disabled_test/1,

    get_membership_requester_info_space_deleted_test/1,
    get_membership_requester_info_space_no_longer_advertised_test/1,
    get_membership_requester_info_user_already_a_member_test/1,
    get_membership_requester_info_error_marketplace_disabled_test/1,

    resolve_no_longer_valid_test/1,
    resolve_error_marketplace_disabled_test/1,

    prune_pending_requests_test/1,
    prune_rejected_history_test/1
]).

groups() -> [
    {parallel_tests, [parallel], [
        submit_request_test,
        grant_test,
        reject_test,

        submit_error_limit_reached_test,
        submit_error_already_a_member_test,
        resolve_error_not_found_test,
        resolve_error_mailer_malfunction_test,
        get_membership_requester_info_forged_ids_test,

        filter_advertised_test
    ]},
    {sequential_tests, [sequential], [
        submit_reminder_test,
        submit_reminder_error_too_soon_test,
        submit_reminder_error_marketplace_disabled_test,

        submit_another_request_test,
        submit_another_error_recently_rejected_test,
        submit_another_error_marketplace_disabled_test,
        submit_error_mailer_malfunction_test,
        submit_error_marketplace_disabled_test,

        get_membership_requester_info_space_deleted_test,
        get_membership_requester_info_space_no_longer_advertised_test,
        get_membership_requester_info_user_already_a_member_test,
        get_membership_requester_info_error_marketplace_disabled_test,

        resolve_no_longer_valid_test,
        resolve_error_marketplace_disabled_test,

        prune_pending_requests_test,
        prune_rejected_history_test
    ]}
].

all() -> [
    {group, parallel_tests},
    {group, sequential_tests}
].

% a couple over the pending + rejected history limits to properly test them
-define(MARKETPLACE_SIZE, 1100).

-define(FROZEN_TIME(), ozt_mocks:get_frozen_time_seconds()).
-define(RAND_ADVERTISED_SPACE(), ?RAND_ELEMENT(ozt_spaces:list_marketplace())).
-define(RAND_REASON(), case ?RAND_BOOL() of
    true -> ?RAND_UNICODE_STR(200);
    false -> <<"">>
end).

-define(PENDING_REQUEST_PRUNING_INTERVAL, ozt:get_env(space_marketplace_pending_request_pruning_interval_seconds)).
-define(MIN_BACKOFF_BETWEEN_REMINDERS_SECONDS, ozt:get_env(space_marketplace_min_backoff_between_reminders_seconds)).
-define(MIN_BACKOFF_AFTER_REJECTION_SECONDS, ozt:get_env(space_marketplace_min_backoff_after_rejection_seconds)).

-define(ATTEMPTS, 30).


%%%===================================================================
%%% Tests
%%%===================================================================

submit_request_test(_Config) ->
    {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    OperatorEmail = get_marketplace_contact_email(SpaceId),

    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmail),
    ?assert(has_registered_space_membership_request(
        pending, RequesterId, SpaceId, RequestId, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assert(has_received_membership_request_email(
        OperatorEmail, ?FROZEN_TIME(), SpaceId, first, RequestId, RequesterId, RequesterEmail
    )).


grant_test(_Config) ->
    {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},

    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmail),
    ozt_spaces:resolve_membership_request(SpaceId, RequestId, grant),
    ?assertNot(has_registered_space_membership_request(
        any, RequesterId, SpaceId, RequestId, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assert(ozt_spaces:has_eff_user(SpaceId, RequesterId)),
    ?assert(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), SpaceId, {resolved, grant})).


reject_test(_Config) ->
    {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},

    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmail),
    ozt_spaces:resolve_membership_request(SpaceId, RequestId, {reject, Reason = ?RAND_REASON()}),
    ?assert(has_registered_space_membership_request(
        rejected, RequesterId, SpaceId, RequestId, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assertNot(ozt_spaces:has_eff_user(SpaceId, RequesterId)),
    ?assert(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), SpaceId, {resolved, {reject, Reason}})).


submit_error_limit_reached_test(_Config) ->
    RequesterId = ozt_users:create(),
    AdvertisedSpaces = ?RAND_SUBLIST(ozt_spaces:list_marketplace(), 1001),
    Results = ozt:pmap(fun(SpaceId) ->
        RequesterEmail = ?RAND_EMAIL_ADDRESS(),
        Result = ozt_spaces:try_submit_membership_request(SpaceId, RequesterId, RequesterEmail),
        case Result of
            {ok, _} ->
                ok;
            {error, _} ->
                ?assertNot(has_received_membership_request_email(
                    get_marketplace_contact_email(SpaceId), ?FROZEN_TIME(), SpaceId,
                    first, <<>>, RequesterId, RequesterEmail
                ))
        end,
        Result
    end, AdvertisedSpaces),

    {OkResults, ErrorResults} = lists:partition(fun
        ({ok, _}) -> true;
        ({error, _}) -> false
    end, Results),

    ?assertEqual(1000, length(OkResults)),
    ?assertMatch([?ERROR_LIMIT_REACHED(1000, <<"pending space membership requests">>)], ErrorResults).


submit_error_already_a_member_test(_Config) ->
    {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    ozt_spaces:add_user(SpaceId, RequesterId),
    ?assertMatch(
        ?ERROR_RELATION_ALREADY_EXISTS(od_user, RequesterId, od_space, SpaceId),
        ozt_spaces:try_submit_membership_request(SpaceId, RequesterId, RequesterEmail)
    ),
    ?assertNot(has_received_membership_request_email(
        get_marketplace_contact_email(SpaceId), ?FROZEN_TIME(), SpaceId,
        first, <<>>, RequesterId, RequesterEmail
    )).


resolve_error_not_found_test(_Config) ->
    {RequesterId, RequesterEmail, ExistingSpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    ExistingRequestId = ozt_spaces:submit_membership_request(ExistingSpaceId, RequesterId, RequesterEmail),
    ForgedSpaceId = datastore_key:new(),
    ForgedRequestId = <<RequesterId/binary, "-7832648714">>,
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ExistingSpaceId, ForgedRequestId, grant)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ExistingSpaceId, ForgedRequestId, {reject, ?RAND_REASON()})),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ForgedSpaceId, ExistingRequestId, grant)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ForgedSpaceId, ExistingRequestId, {reject, ?RAND_REASON()})),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ForgedSpaceId, ForgedRequestId, grant)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ForgedSpaceId, ForgedRequestId, {reject, ?RAND_REASON()})).


resolve_error_mailer_malfunction_test(_Config) ->
    lists:foreach(fun(Decision) ->
        {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
        RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmail),
        ozt_mailer:toggle_error_simulation(RequesterEmail, true),
        % an error in mailer should not block request resolving
        ?assertMatch(ok, ozt_spaces:try_resolve_membership_request(SpaceId, RequestId, Decision)),
        ?assertNot(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), SpaceId, {resolved, Decision}))
    end, [grant, {reject, ?RAND_REASON()}]).


get_membership_requester_info_forged_ids_test(_Config) ->
    {RequesterId, ExistingSpaceId} = {ozt_users:create(), ?RAND_ADVERTISED_SPACE()},
    ExistingRequestId = ozt_spaces:submit_membership_request(ExistingSpaceId, RequesterId),
    ForgedSpaceId = datastore_key:new(),
    ForgedRequestId = <<RequesterId/binary, "-iuysdtgfuye">>,
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(ExistingSpaceId, ForgedRequestId)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(ExistingSpaceId, ForgedRequestId)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(ForgedSpaceId, ExistingRequestId)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(ForgedSpaceId, ExistingRequestId)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(ForgedSpaceId, ForgedRequestId)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(ForgedSpaceId, ForgedRequestId)).


filter_advertised_test(_Config) ->
    AdvertisedSpaceIds = ?RAND_SUBLIST(ozt_spaces:list_marketplace()),
    NonAdvertisedSpaceIds = lists_utils:generate(fun() -> ?RAND_STR() end, 500),
    ?assertEqual(
        lists:sort(AdvertisedSpaceIds),
        lists:sort(ozt:rpc(space_marketplace, filter_advertised, [?SHUFFLED(NonAdvertisedSpaceIds ++ AdvertisedSpaceIds)]))
    ).

% ----------------
% sequential_tests

submit_reminder_test(_Config) ->
    {RequesterId, FirstRequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    OperatorEmail = get_marketplace_contact_email(SpaceId),

    FirstRequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, FirstRequesterEmail),

    ReminderEmails = lists_utils:generate(fun() ->
        ReminderRequesterEmail = ?RAND_EMAIL_ADDRESS(),
        ozt_mocks:simulate_seconds_passing(?MIN_BACKOFF_BETWEEN_REMINDERS_SECONDS),
        ReminderRequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, ReminderRequesterEmail),
        ?assertEqual(FirstRequestId, ReminderRequestId),
        ?assert(has_registered_space_membership_request(
            pending, RequesterId, SpaceId, ReminderRequestId, ReminderRequesterEmail, ?FROZEN_TIME())
        ),
        ?assert(has_received_membership_request_email(
            OperatorEmail, ?FROZEN_TIME(), SpaceId,
            reminder, ReminderRequestId, RequesterId, ReminderRequesterEmail
        )),
        ReminderRequesterEmail
    end, 10),

    ozt_spaces:resolve_membership_request(SpaceId, FirstRequestId, grant),
    ?assertNot(has_registered_space_membership_request(
        any, RequesterId, SpaceId, FirstRequestId, lists:last(ReminderEmails), ?FROZEN_TIME())
    ),
    ?assert(ozt_spaces:has_eff_user(SpaceId, RequesterId)),
    ?assert(has_received_notification_email(lists:last(ReminderEmails), ?FROZEN_TIME(), SpaceId, {resolved, grant})).


submit_reminder_error_too_soon_test(_Config) ->
    {RequesterId, SpaceId} = {ozt_users:create(), ?RAND_ADVERTISED_SPACE()},

    ozt_spaces:submit_membership_request(SpaceId, RequesterId),
    ozt_mocks:simulate_seconds_passing(?MIN_BACKOFF_BETWEEN_REMINDERS_SECONDS - 1),
    ReminderAllowedDate = time:seconds_to_iso8601(?FROZEN_TIME() + 1),
    ExpError = ?ERROR_FORBIDDEN(<<
        "A membership request to this space has been submitted recently. "
        "A reminder can be generated not sooner than at ", ReminderAllowedDate/binary
    >>),
    ?assertEqual(ExpError, ozt_spaces:try_submit_membership_request(SpaceId, RequesterId)).


submit_reminder_error_marketplace_disabled_test(_Config) ->
    {RequesterId, FirstRequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    OperatorEmail = get_marketplace_contact_email(SpaceId),

    ozt_spaces:submit_membership_request(SpaceId, RequesterId, FirstRequesterEmail),
    ReminderRequesterEmail = ?RAND_EMAIL_ADDRESS(),
    ozt_mocks:simulate_seconds_passing(?MIN_BACKOFF_BETWEEN_REMINDERS_SECONDS),
    ozt:set_env(space_marketplace_enabled, false),
    ?assertEqual(
        ?ERROR_SPACE_MARKETPLACE_DISABLED,
        ozt_spaces:try_submit_membership_request(SpaceId, RequesterId, ReminderRequesterEmail)
    ),
    ?assertNot(has_registered_space_membership_request(
        pending, RequesterId, SpaceId, <<>>, ReminderRequesterEmail, ?FROZEN_TIME())
    ),
    ?assertNot(has_received_membership_request_email(
        OperatorEmail, ?FROZEN_TIME(), SpaceId,
        reminder, <<>>, RequesterId, ReminderRequesterEmail
    )).


submit_another_request_test(_Config) ->
    {RequesterId, SpaceId} = {ozt_users:create(), ?RAND_ADVERTISED_SPACE()},
    {RequesterEmailAlpha, RequesterEmailBeta} = {?RAND_EMAIL_ADDRESS(), ?RAND_EMAIL_ADDRESS()},
    OperatorEmail = get_marketplace_contact_email(SpaceId),

    FirstRequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmailAlpha),
    ozt_mocks:simulate_seconds_passing(?RAND_INT(1, 989234)),
    ozt_spaces:resolve_membership_request(SpaceId, FirstRequestId, {reject, Reason = ?RAND_REASON()}),
    ?assert(has_received_notification_email(RequesterEmailAlpha, ?FROZEN_TIME(), SpaceId, {resolved, {reject, Reason}})),
    ozt_mocks:simulate_seconds_passing(?MIN_BACKOFF_AFTER_REJECTION_SECONDS),
    AnotherRequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmailBeta),
    ?assertNotEqual(FirstRequestId, AnotherRequestId),

    ?assert(has_registered_space_membership_request(
        pending, RequesterId, SpaceId, AnotherRequestId, RequesterEmailBeta, ?FROZEN_TIME())
    ),
    ?assert(has_received_membership_request_email(
        OperatorEmail, ?FROZEN_TIME(), SpaceId, first, AnotherRequestId, RequesterId, RequesterEmailBeta
    )),

    ozt_spaces:resolve_membership_request(SpaceId, AnotherRequestId, grant),
    ?assertNot(has_registered_space_membership_request(
        any, RequesterId, SpaceId, AnotherRequestId, RequesterEmailBeta, ?FROZEN_TIME())
    ),
    ?assert(ozt_spaces:has_eff_user(SpaceId, RequesterId)),
    ?assert(has_received_notification_email(RequesterEmailBeta, ?FROZEN_TIME(), SpaceId, {resolved, grant})).


submit_another_error_recently_rejected_test(_Config) ->
    {RequesterId, SpaceId} = {ozt_users:create(), ?RAND_ADVERTISED_SPACE()},

    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
    ozt_mocks:simulate_seconds_passing(?RAND_INT(1, 989234)),
    ozt_spaces:resolve_membership_request(SpaceId, RequestId, {reject, ?RAND_REASON()}),

    ozt_mocks:simulate_seconds_passing(?MIN_BACKOFF_AFTER_REJECTION_SECONDS - 1),
    ReminderAllowedDate = time:seconds_to_iso8601(?FROZEN_TIME() + 1),
    ExpError = ?ERROR_FORBIDDEN(<<
        "A membership request to this space has been recently rejected. "
        "Another request can be made not sooner than at ", ReminderAllowedDate/binary
    >>),
    ?assertEqual(ExpError, ozt_spaces:try_submit_membership_request(SpaceId, RequesterId)).


submit_another_error_marketplace_disabled_test(_Config) ->
    {RequesterId, SpaceId} = {ozt_users:create(), ?RAND_ADVERTISED_SPACE()},
    {RequesterEmailAlpha, RequesterEmailBeta} = {?RAND_EMAIL_ADDRESS(), ?RAND_EMAIL_ADDRESS()},
    OperatorEmail = get_marketplace_contact_email(SpaceId),

    FirstRequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmailAlpha),
    ozt_mocks:simulate_seconds_passing(?RAND_INT(1, 989234)),
    Reason = ?RAND_REASON(),
    ozt_spaces:resolve_membership_request(SpaceId, FirstRequestId, {reject, Reason}),
    ?assert(has_received_notification_email(RequesterEmailAlpha, ?FROZEN_TIME(), SpaceId, {resolved, {reject, Reason}})),
    ozt_mocks:simulate_seconds_passing(?MIN_BACKOFF_AFTER_REJECTION_SECONDS),
    ozt:set_env(space_marketplace_enabled, false),
    ?assertEqual(
        ?ERROR_SPACE_MARKETPLACE_DISABLED,
        ozt_spaces:try_submit_membership_request(SpaceId, RequesterId, RequesterEmailBeta)
    ),
    ?assertNot(has_received_membership_request_email(
        OperatorEmail, ?FROZEN_TIME(), SpaceId, first, <<>>, RequesterId, RequesterEmailBeta
    )).


submit_error_mailer_malfunction_test(_Config) ->
    {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    OperatorEmailAddress = get_marketplace_contact_email(SpaceId),
    ozt_mailer:toggle_error_simulation(OperatorEmailAddress, true),
    ?assertMatch(
        ?ERROR_INTERNAL_SERVER_ERROR(_),
        ozt_spaces:try_submit_membership_request(SpaceId, RequesterId, RequesterEmail)
    ),
    ?assertNot(has_received_membership_request_email(
        OperatorEmailAddress, ?FROZEN_TIME(), SpaceId,
        first, <<>>, RequesterId, RequesterEmail
    )).


submit_error_marketplace_disabled_test(_Config) ->
    {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    OperatorEmail = get_marketplace_contact_email(SpaceId),
    ozt:set_env(space_marketplace_enabled, false),
    ?assertEqual(
        ?ERROR_SPACE_MARKETPLACE_DISABLED,
        ozt_spaces:try_submit_membership_request(SpaceId, RequesterId, RequesterEmail)
    ),
    ?assertNot(has_registered_space_membership_request(
        pending, RequesterId, SpaceId, <<>>, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assertNot(has_received_membership_request_email(
        OperatorEmail, ?FROZEN_TIME(), SpaceId, first, <<>>, RequesterId, RequesterEmail
    )).


get_membership_requester_info_space_deleted_test(_Config) ->
    {RequesterId, SpaceId} = {ozt_users:create(), ozt_spaces:create_advertised()},
    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
    ozt_spaces:delete(SpaceId),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(SpaceId, RequestId)).


get_membership_requester_info_space_no_longer_advertised_test(_Config) ->
    {RequesterId, SpaceId} = {ozt_users:create(), ozt_spaces:create_advertised()},
    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
    ozt_spaces:update(SpaceId, #{<<"advertisedInMarketplace">> => false}),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_get_membership_info(SpaceId, RequestId)).


get_membership_requester_info_user_already_a_member_test(_Config) ->
    {RequesterId, SpaceId} = {ozt_users:create(), ozt_spaces:create_advertised()},
    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
    ozt_spaces:add_user(SpaceId, RequesterId),
    ?assertMatch(
        ?ERROR_RELATION_ALREADY_EXISTS(od_user, RequesterId, od_space, SpaceId),
        ozt_spaces:try_get_membership_info(SpaceId, RequestId)
    ).


get_membership_requester_info_error_marketplace_disabled_test(_Config) ->
    {RequesterId, ExistingSpaceId} = {ozt_users:create(), ?RAND_ADVERTISED_SPACE()},
    ExistingRequestId = ozt_spaces:submit_membership_request(ExistingSpaceId, RequesterId),
    ozt:set_env(space_marketplace_enabled, false),
    ?assertEqual(
        ?ERROR_SPACE_MARKETPLACE_DISABLED,
        ozt_spaces:try_get_membership_info(ExistingSpaceId, ExistingRequestId)
    ).


resolve_no_longer_valid_test(_Config) ->
    {RequesterId, RequesterEmail} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS()},
    % a no longer valid request should be treated as not found,
    % plus it should trigger pruning (regardless of interval) and a notification
    % first reason - the user is already a member
    FirstSpaceId = ozt_spaces:create_advertised(),
    FirstRequestId = ozt_spaces:submit_membership_request(FirstSpaceId, RequesterId, RequesterEmail),
    ozt_spaces:add_user(FirstSpaceId, RequesterId),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(FirstSpaceId, FirstRequestId, grant)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(FirstSpaceId, FirstRequestId, {reject, ?RAND_REASON()})),
    ?assertNot(has_registered_space_membership_request(
        any, RequesterId, FirstSpaceId, FirstRequestId, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assert(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), FirstSpaceId, already_granted), ?ATTEMPTS),
    % second reason - the space is no longer advertised
    SecondSpaceId = ozt_spaces:create_advertised(),
    SecondRequestId = ozt_spaces:submit_membership_request(SecondSpaceId, RequesterId, RequesterEmail),
    ozt_spaces:update(SecondSpaceId, #{<<"advertisedInMarketplace">> => false}),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(SecondSpaceId, SecondRequestId, grant)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(SecondSpaceId, SecondRequestId, {reject, ?RAND_REASON()})),
    ?assertNot(has_registered_space_membership_request(
        any, RequesterId, SecondSpaceId, SecondRequestId, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assert(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), SecondSpaceId, cancelled), ?ATTEMPTS),
    % third reason - the space is deleted
    % here, pruning cannot be immediately scheduled; as the space no longer exists,
    % all marketplace operations on it fail on existence check
    ThirdSpaceId = ozt_spaces:create_advertised(),
    ThirdRequestId = ozt_spaces:submit_membership_request(ThirdSpaceId, RequesterId, RequesterEmail),
    ozt_spaces:delete(ThirdSpaceId),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ThirdSpaceId, ThirdRequestId, grant)),
    ?assertMatch(?ERROR_NOT_FOUND, ozt_spaces:try_resolve_membership_request(ThirdSpaceId, ThirdRequestId, {reject, ?RAND_REASON()})),
    ?assert(has_registered_space_membership_request(
        pending, RequesterId, ThirdSpaceId, ThirdRequestId, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assertNot(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), ThirdSpaceId, cancelled), ?ATTEMPTS),
    % however, it should be pruned when the interval passes
    ozt_mocks:simulate_seconds_passing(?PENDING_REQUEST_PRUNING_INTERVAL),
    % some operation must be performed to trigger the pruning
    ozt_spaces:submit_membership_request(ozt_spaces:create_advertised(), RequesterId, ?RAND_EMAIL_ADDRESS()),
    ?assertNot(has_registered_space_membership_request(
        any, RequesterId, ThirdSpaceId, ThirdRequestId, RequesterEmail, ?FROZEN_TIME())
    ),
    ?assert(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), ThirdSpaceId, cancelled), ?ATTEMPTS).


resolve_error_marketplace_disabled_test(_Config) ->
    {RequesterId, RequesterEmail, SpaceId} = {ozt_users:create(), ?RAND_EMAIL_ADDRESS(), ?RAND_ADVERTISED_SPACE()},
    RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId, RequesterEmail),
    ozt:set_env(space_marketplace_enabled, false),
    ?assertEqual(
        ?ERROR_SPACE_MARKETPLACE_DISABLED,
        ozt_spaces:try_resolve_membership_request(SpaceId, RequestId, grant)
    ),
    ?assertNot(has_received_notification_email(RequesterEmail, ?FROZEN_TIME(), SpaceId, {resolved, grant})).


prune_pending_requests_test(_Config) ->
    RequesterId = ozt_users:create(),
    AdvertisedSpaces = ?RAND_SUBLIST(ozt_spaces:list_marketplace(), 1052),
    AdvertisedSpacesToBeAlreadyGranted = lists:sublist(AdvertisedSpaces, 1003, 50),
    AdvertisedSpacesToBeDeleted = ozt:pmap(fun(_) -> ozt_spaces:create_advertised() end, lists:seq(1, 50)),
    AdvertisedSpacesToBeWithdrawn = ozt:pmap(fun(_) -> ozt_spaces:create_advertised() end, lists:seq(1, 50)),

    FirstRequestedSpace = hd(AdvertisedSpaces),
    FirstContactEmail = <<FirstRequestedSpace/binary, "@example.com">>,
    FirstRequestId = ozt_spaces:submit_membership_request(FirstRequestedSpace, RequesterId, FirstContactEmail),
    % pruning is done automatically with every request submission/resolving,
    % but with an interval between consecutive attempts
    FirstPruningTime = ?FROZEN_TIME(),

    ozt:pforeach(fun(SpaceId) ->
        ozt_spaces:submit_membership_request(SpaceId, RequesterId, <<SpaceId/binary, "@example.com">>),
        ozt_mocks:simulate_seconds_passing(1)
    end, ?SHUFFLED(lists:flatten([
        lists:sublist(AdvertisedSpaces, 2, 849),
        AdvertisedSpacesToBeAlreadyGranted,
        AdvertisedSpacesToBeDeleted,
        AdvertisedSpacesToBeWithdrawn
    ]))),
    % with 1000 requests made, the limit should be reached
    ?assertMatch(?ERROR_LIMIT_REACHED(_, _), ozt_spaces:try_submit_membership_request(
        lists:nth(851, AdvertisedSpaces), RequesterId
    )),
    % apply modifications that will make some spaces no longer applicable for a request
    ozt:pforeach(fun(SpaceId) ->
        ozt_spaces:add_user(SpaceId, RequesterId)
    end, AdvertisedSpacesToBeAlreadyGranted),
    ozt:pforeach(fun(SpaceId) ->
        ozt_spaces:delete(SpaceId)
    end, AdvertisedSpacesToBeDeleted),
    ozt:pforeach(fun(SpaceId) ->
        ozt_spaces:update(SpaceId, #{<<"advertisedInMarketplace">> => false})
    end, AdvertisedSpacesToBeWithdrawn),
    % the spaces should be pruned upon the next request submission/resolving,
    % given that the interval has passed
    ozt_mocks:simulate_seconds_passing(?PENDING_REQUEST_PRUNING_INTERVAL - (?FROZEN_TIME() - FirstPruningTime) - 1),
    ?assertMatch(?ERROR_LIMIT_REACHED(_, _), ozt_spaces:try_submit_membership_request(
        lists:nth(851, AdvertisedSpaces), RequesterId
    )),
    ozt_mocks:simulate_seconds_passing(1),
    % simulate errors for some of the notifications - they should not cause the pruning to crash
    SpacesWithNotificationErrors = lists:filter(fun(SpaceId) ->
        SimulateErrors = ?RAND_BOOL(),
        ozt_mailer:toggle_error_simulation(<<SpaceId/binary, "@example.com">>, SimulateErrors),
        SimulateErrors
    end, AdvertisedSpacesToBeAlreadyGranted ++ AdvertisedSpacesToBeDeleted ++ AdvertisedSpacesToBeWithdrawn),
    % both request submission and resolving of a request should trigger pruning
    TestedMethod = ?RAND_ELEMENT([submit_new_request, resolve_existing_request]),
    case TestedMethod of
        submit_new_request ->
            ?assertMatch({ok, _}, ozt_spaces:try_submit_membership_request(
                lists:nth(851, AdvertisedSpaces), RequesterId
            ));
        resolve_existing_request ->
            ozt_spaces:resolve_membership_request(FirstRequestedSpace, FirstRequestId, grant)
    end,
    lists:foreach(fun(SpaceId) ->
        ?assertEqual(not lists:member(SpaceId, SpacesWithNotificationErrors), has_received_notification_email(
            <<SpaceId/binary, "@example.com">>, ?FROZEN_TIME(), SpaceId, already_granted
        ), ?ATTEMPTS)
    end, AdvertisedSpacesToBeAlreadyGranted),
    lists:foreach(fun(SpaceId) ->
        ?assertEqual(not lists:member(SpaceId, SpacesWithNotificationErrors), has_received_notification_email(
            <<SpaceId/binary, "@example.com">>, ?FROZEN_TIME(), SpaceId, cancelled
        ), ?ATTEMPTS)
    end, AdvertisedSpacesToBeDeleted ++ AdvertisedSpacesToBeWithdrawn),
    % depending on the testing method, there may be 851 or 849 pending requests - even it out
    case TestedMethod of
        submit_new_request ->
            ok;
        resolve_existing_request ->
            ozt_spaces:submit_membership_request(lists:nth(851, AdvertisedSpaces), RequesterId),
            ozt_spaces:submit_membership_request(lists:nth(852, AdvertisedSpaces), RequesterId)
    end,
    % since the requests have been pruned, it should be possible to make another 149 requests
    lists:foreach(fun(SpaceId) ->
        ?assertMatch({ok, _}, ozt_spaces:try_submit_membership_request(SpaceId, RequesterId))
    end, lists:sublist(AdvertisedSpaces, 853, 149)),
    % but not more than that
    ?assertMatch(?ERROR_LIMIT_REACHED(_, _), ozt_spaces:try_submit_membership_request(
        lists:nth(1002, AdvertisedSpaces), RequesterId
    )),
    % until the next pruning
    ozt_spaces:add_user(lists:nth(17, AdvertisedSpaces), RequesterId),
    ozt_mocks:simulate_seconds_passing(?PENDING_REQUEST_PRUNING_INTERVAL),
    ?assertMatch({ok, _}, ozt_spaces:try_submit_membership_request(
        lists:nth(1002, AdvertisedSpaces), RequesterId
    )).


prune_rejected_history_test(_Config) ->
    RequesterId = ozt_users:create(),
    AdvertisedSpaces = ?RAND_SUBLIST(ozt_spaces:list_marketplace(), 1100),
    {SpacesWithinLimit, Rest} = lists:split(1000, AdvertisedSpaces),
    {OutdatedRejectedSpaces, ExpectedPrematurelyPrunedSpaces} = lists:split(50, Rest),

    % OutdatedRejectedSpaces are expected to be pruned because
    %     MIN_BACKOFF_AFTER_REJECTION_SECONDS has passed
    % ExpectedPrematurelyPrunedSpaces are expected to be pruned because of history overflow,
    %   despite the fact that the backoff has not passed yet

    ozt:pforeach(fun(SpaceId) ->
        RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
        ozt_spaces:resolve_membership_request(SpaceId, RequestId, {reject, ?RAND_REASON()}),
        ozt_mocks:simulate_seconds_passing(?RAND_INT(1, 1000))
    end, OutdatedRejectedSpaces),
    ozt_mocks:simulate_seconds_passing(?MIN_BACKOFF_AFTER_REJECTION_SECONDS),
    % after the backoff has passed, all 50 above rejections should be treated as outdated and
    % pruned when needed
    lists:foreach(fun(SpaceId) ->
        RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
        ozt_spaces:resolve_membership_request(SpaceId, RequestId, {reject, ?RAND_REASON()}),
        ozt_mocks:simulate_seconds_passing(1)
    end, ExpectedPrematurelyPrunedSpaces),
    % all 50 above spaces should be appended to the rejection history, the first one with the
    % oldest timestamp
    ozt:pforeach(fun(SpaceId) ->
        RequestId = ozt_spaces:submit_membership_request(SpaceId, RequesterId),
        ozt_spaces:resolve_membership_request(SpaceId, RequestId, {reject, ?RAND_REASON()}),
        ozt_mocks:simulate_seconds_passing(1)
    end, lists:sublist(SpacesWithinLimit, 950)),
    % adding 950 spaces to the history should cause the OutdatedRejectedSpaces to be
    % pruned, which means that it should now be possible to submit subsequent requests for them
    lists:foreach(fun(SpaceId) ->
        ?assertMatch({ok, _}, ozt_spaces:try_submit_membership_request(SpaceId, RequesterId))
    end, ?RAND_SUBLIST(OutdatedRejectedSpaces, 5)),
    % however, ExpectedPrematurelyPrunedSpaces should still be in the history and hence
    % it should not be possible to request membership in them
    lists:foreach(fun(SpaceId) ->
        ?assertMatch(?ERROR_FORBIDDEN(_), ozt_spaces:try_submit_membership_request(SpaceId, RequesterId))
    end, ?RAND_SUBLIST(ExpectedPrematurelyPrunedSpaces, 5)),
    % with every new rejection, the history should become overflown, and the oldest of
    % ExpectedPrematurelyPrunedSpaces should be pruned, despite that the backoff has not
    % passed yet
    lists:foreach(fun(Ordinal) ->
        ToRejectSpaceId = lists:nth(950 + Ordinal, SpacesWithinLimit),
        RequestId = ozt_spaces:submit_membership_request(ToRejectSpaceId, RequesterId),
        ozt_spaces:resolve_membership_request(ToRejectSpaceId, RequestId, {reject, ?RAND_REASON()}),
        JustPrunedSpaceId = lists:nth(Ordinal, ExpectedPrematurelyPrunedSpaces),
        ?assertMatch({ok, _}, ozt_spaces:try_submit_membership_request(JustPrunedSpaceId, RequesterId)),
        case Ordinal of
            50 ->
                ok;
            _ ->
                NotYetPrunedSpaceId = lists:nth(Ordinal + 1, ExpectedPrematurelyPrunedSpaces),
                ?assertMatch(?ERROR_FORBIDDEN(_), ozt_spaces:try_submit_membership_request(NotYetPrunedSpaceId, RequesterId))
        end
    end, lists:seq(1, 50)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
-spec get_marketplace_contact_email(od_space:id()) -> od_user:email().
get_marketplace_contact_email(SpaceId) ->
    #od_space{marketplace_contact_email = MarketplaceContactEmail} = ozt_spaces:get(SpaceId),
    MarketplaceContactEmail.


%% @private
-spec get_space_membership_requests_json(od_user:id()) -> json_utils:json_map().
get_space_membership_requests_json(UserId) ->
    #od_user{space_membership_requests = SpaceMembershipRequests} = ozt_users:get(UserId),
    jsonable_record:to_json(SpaceMembershipRequests).


%% @private
-spec has_registered_space_membership_request(
    pending | rejected | any,
    od_user:id(),
    od_space:id(),
    space_membership_requests:request_id(),
    od_user:email(),
    time:seconds()
) ->
    boolean().
has_registered_space_membership_request(Type, RequesterId, SpaceId, RequestId, RequesterEmail, TimestampSeconds) ->
    #{<<"pending">> := Pending, <<"rejected">> := Rejected} = get_space_membership_requests_json(RequesterId),
    RegistryToCheck = case Type of
        pending -> Pending;
        rejected -> Rejected;
        any -> maps:merge(Pending, Rejected)
    end,
    try
        true = maps:is_key(SpaceId, RegistryToCheck),
        #{
            <<"requestId">> := RequestId,
            <<"contactEmail">> := RequesterEmail,
            <<"lastActivity">> := TimestampSeconds
        } = maps:get(SpaceId, RegistryToCheck),
        true
    catch
        _:_ ->
            false
    end.


%% @private
-spec has_received_membership_request_email(
    od_user:email(),
    time:seconds(),
    od_space:id(),
    first | reminder,
    space_membership_requests:request_id(),
    od_user:id(),
    od_user:email()
) ->
    boolean().
has_received_membership_request_email(
    OperatorEmail,
    TimestampSeconds,
    SpaceId,
    Classification,
    RequestId,
    RequesterId,
    RequesterEmail
) ->
    #od_space{name = SpaceName} = ozt_spaces:get(SpaceId),
    #od_user{full_name = RequesterFullName, username = RequesterUsername} = ozt_users:get(RequesterId),
    ExpectedUsername = case RequesterUsername of undefined -> <<"none">>; _ -> RequesterUsername end,
    SubjectClassificationKeyword = case Classification of first -> "New"; reminder -> "REMINDER:" end,
    BodyClassificationKeyword = case Classification of first -> "new"; reminder -> "kind reminder" end,

    MatchingEmails = ozt_mailer:match_received_emails(OperatorEmail, TimestampSeconds, [
        SpaceName,
        SubjectClassificationKeyword
    ], [
        "Dear Onedata user",
        BodyClassificationKeyword, SpaceId, SpaceName,
        RequesterId, RequesterFullName, ExpectedUsername, RequesterEmail,
        "confirmJoinSpaceRequest", RequestId,
        "This is an automated message", ozt:get_domain()
    ]),
    % in this test suite, we do not expect any duplicated emails - double-check (length should be 0 or 1)
    ?assert(length(MatchingEmails) < 2),

    1 == length(MatchingEmails).


%% @private
-spec has_received_notification_email(
    od_user:email(),
    time:seconds(),
    od_space:id(),
    {resolved, space_membership_requests:decision()} | already_granted | cancelled
) ->
    boolean().
has_received_notification_email(
    RequesterEmail,
    TimestampSeconds,
    SpaceId,
    Type
) ->
    SpaceName = case ozt_spaces:exists(SpaceId) of
        true -> (ozt_spaces:get(SpaceId))#od_space.name;
        false -> <<"Unknown">>
    end,
    SubjectTypeKeyword = case Type of
        {resolved, grant} -> "GRANTED";
        {resolved, {reject, _}} -> "REJECTED";
        already_granted -> "already GRANTED";
        cancelled -> "CANCELLED"
    end,
    BodyTypeKeywords = case Type of
        {resolved, grant} -> ["GRANTED", "by the space maintainer", "in Web GUI", "/#/onedata/spaces/"];
        {resolved, {reject, Reason}} ->
            case Reason of
                <<"">> ->
                    ["REJECTED", "by the space maintainer",
                    "No reason for rejection was provided."];
                Reason ->
                    ["REJECTED", "by the space maintainer", str_utils:format(
                    "Reason: \"~ts\"~n",
                    [Reason]
                )]
            end;
        already_granted -> ["has been withdrawn", "has been GRANTED", "independently of space marketplace"];
        cancelled -> ["has been CANCELLED", "has been deleted", "no longer advertised"]
    end,
    1 == length(ozt_mailer:match_received_emails(RequesterEmail, TimestampSeconds, [
        "Membership",
        SpaceName,
        SubjectTypeKeyword
    ], [
        "Dear Onedata user",
        "Your membership request for space",
        SpaceId, SpaceName,
        "This is an automated message", ozt:get_domain()
    ] ++ BodyTypeKeywords)).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        CurrentMarketplaceSize = length(ozt_spaces:list_marketplace()),
        lists_utils:pforeach(fun(_) ->
            ozt_spaces:create_advertised()
        end, lists:seq(1, max(0, ?MARKETPLACE_SIZE - CurrentMarketplaceSize)))
    end).

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    ozt_mailer:mock(),
    ozt_mocks:freeze_time(),
    Config.

end_per_group(_, Config) ->
    ozt_mailer:unmock(),
    ozt_mocks:unfreeze_time(),
    Config.


init_per_testcase(_, Config) ->
    % spaces are reused between testcases and some tests may simulate errors
    % for marketplace contact email, so they must be reset for no interference
    ozt:set_env(space_marketplace_enabled, true),
    ozt:pforeach(fun(SpaceId) ->
        ozt_mailer:toggle_error_simulation(get_marketplace_contact_email(SpaceId), false)
    end, ozt_spaces:list_marketplace()),
    Config.

end_per_testcase(_, Config) ->
    Config.
