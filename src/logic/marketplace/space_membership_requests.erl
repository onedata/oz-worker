%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module represents a jsonable record that tracks space membership
%%% requests (in the context of space marketplace).
%%% @end
%%%-------------------------------------------------------------------
-module(space_membership_requests).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([empty/0]).
-export([submit/5]).
-export([lookup_pending_request_id/2, lookup_email_for_pending/3]).
-export([resolve/4]).
-export([infer_requester_id/1]).
-export([prune_pending_requests/2]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


% expresses the decision of space maintainer; whether to grant or reject a membership request
-type decision() :: grant | {reject, binary()}.
-type request_id() :: binary().
-record(request, {
    id :: request_id(),
    contact_email :: od_user:email(),
    % the time of request submission (or last reminder) in case of pending requests
    % or the time of rejection in case of rejected requests
    last_activity :: time:seconds()
}).
-type request() :: #request{}.

-record(space_membership_requests, {
    pending = #{} :: #{od_space:id() => request()},
    rejected = #{} :: #{od_space:id() => request()},
    last_pending_request_pruning_time = 0 :: time:seconds()
}).
-type record() :: #space_membership_requests{}.
-export_type([decision/0, request_id/0, record/0]).


-define(NOW_SECONDS(), global_clock:timestamp_seconds()).
-define(PENDING_REQUEST_LIMIT, 1000).
-define(REJECTED_REQUEST_HISTORY_LENGTH, 1000).

-define(PENDING_REQUEST_PRUNING_INTERVAL, oz_worker:get_env(
    space_marketplace_pending_request_pruning_interval_seconds, 86400  % a day
)).
-define(MIN_BACKOFF_BETWEEN_REMINDERS_SECONDS, oz_worker:get_env(
    space_marketplace_min_backoff_between_reminders_seconds, 604800  % a week
)).
-define(MIN_BACKOFF_AFTER_REJECTION_SECONDS, oz_worker:get_env(
    space_marketplace_min_backoff_after_rejection_seconds, 2592000  % a month
)).

-define(REQUEST_ID_RANDOM_PART_BYTES, 4).  % 4 bytes = 8 hex chars
-define(REQUEST_ID_SEPARATOR, "-").


%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> record().
empty() ->
    #space_membership_requests{}.


-spec submit(od_space:id(), od_user:id(), od_user:email(), binary(), record()) ->
    {done, record()} | pruning_needed | no_return().
submit(SpaceId, UserId, ContactEmail, Message, InitialRecord) ->
    case pruning_interval_passed(InitialRecord) of
        true ->
            pruning_needed;
        false ->
            {done, submit_internal(SpaceId, UserId, ContactEmail, Message, InitialRecord)}
    end.


-spec lookup_pending_request_id(od_space:id(), record()) ->
    request_id() | no_return().
lookup_pending_request_id(SpaceId, #space_membership_requests{pending = Pending}) ->
    case maps:find(SpaceId, Pending) of
        {ok, #request{id = RequestId}} -> RequestId;
        _ -> throw(?ERROR_NOT_FOUND)
    end.


-spec lookup_email_for_pending(od_space:id(), request_id(), record()) -> od_user:email() | no_return().
lookup_email_for_pending(SpaceId, RequestId, Record) ->
    od_space:is_advertised_in_marketplace(SpaceId) orelse throw(?ERROR_NOT_FOUND),
    Request = lookup_pending(SpaceId, RequestId, Record),  % throws in case of nonexistent pending request
    assert_not_a_member(infer_requester_id(RequestId), SpaceId),
    Request#request.contact_email.


-spec resolve(od_space:id(), request_id(), decision(), record()) ->
    {done, record()} | pruning_needed | no_return().
resolve(SpaceId, RequestId, Decision, InitialRecord) ->
    lookup_pending(SpaceId, RequestId, InitialRecord),  % throws in case of nonexistent pending request

    RequesterUserId = infer_requester_id(RequestId),
    % in case the request for this specific space is no longer valid, trigger immediate
    % pruning, regardless of the interval - this will prune the request
    ShouldTriggerPruning = user_logic:has_eff_space(RequesterUserId, SpaceId) orelse
        not od_space:is_advertised_in_marketplace(SpaceId),

    UpdatedRecord = case ShouldTriggerPruning of
        true -> InitialRecord#space_membership_requests{last_pending_request_pruning_time = 0};
        false -> InitialRecord
    end,

    case pruning_interval_passed(UpdatedRecord) of
        true ->
            pruning_needed;
        false ->
            {done, resolve_sanitized(SpaceId, RequesterUserId, Decision, UpdatedRecord)}
    end.


-spec infer_requester_id(request_id()) -> od_user:id() | no_return().
infer_requester_id(RequestId) ->
    try
        [UserId, _] = binary:split(RequestId, <<?REQUEST_ID_SEPARATOR>>, [global]),
        UserId
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_IDENTIFIER(<<"requestId">>))
    end.


%% @private
-spec gen_request_id(od_user:id()) -> request_id().
gen_request_id(UserId) ->
    RandomPart = str_utils:rand_hex(?REQUEST_ID_RANDOM_PART_BYTES),
    <<UserId/binary, ?REQUEST_ID_SEPARATOR, RandomPart/binary>>.


%% @doc Prunes no longer valid requests and sends corresponding notifications.
-spec prune_pending_requests(od_user:id(), record()) -> record().
prune_pending_requests(UserId, Record = #space_membership_requests{pending = Pending}) ->
    PendingSpaceIds = maps:keys(Pending),
    % since the last update of the record, the user may have gained access to some of the spaces
    {ok, UserSpaceIds} = user_logic:get_eff_spaces(?ROOT, UserId),
    AlreadyGrantedSpaceIds = lists_utils:intersect(PendingSpaceIds, UserSpaceIds),
    async_send_notifications_for_spaces(already_granted, AlreadyGrantedSpaceIds, Record),
    SpaceIdsWithoutAlreadyGranted = lists_utils:subtract(PendingSpaceIds, AlreadyGrantedSpaceIds),

    % since the last update of the record, some spaces may have been removed from the marketplace
    StillRelevantSpaceIds = space_marketplace:filter_advertised(SpaceIdsWithoutAlreadyGranted),
    CancelledSpaceIds = lists_utils:subtract(SpaceIdsWithoutAlreadyGranted, StillRelevantSpaceIds),
    async_send_notifications_for_spaces(cancelled, CancelledSpaceIds, Record),

    Record#space_membership_requests{
        pending = maps:with(StillRelevantSpaceIds, Pending),
        last_pending_request_pruning_time = ?NOW_SECONDS()
    }.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode(json, Record).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode(json, RecordJson).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    encode(db, Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    decode(db, RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec lookup_pending(od_space:id(), request_id(), record()) -> request() | no_return().
lookup_pending(SpaceId, RequestId, #space_membership_requests{pending = Pending}) ->
    case maps:find(SpaceId, Pending) of
        {ok, #request{id = RequestId} = Request} -> Request;
        _ -> throw(?ERROR_NOT_FOUND)
    end.


%% @private
-spec submit_internal(od_space:id(), od_user:id(), od_user:email(), binary(), record()) ->
    record() | no_return().
submit_internal(SpaceId, UserId, ContactEmail, Message, InitialRecord) ->
    {Approach, UpdatedRecord} = qualify(SpaceId, UserId, InitialRecord),
    {RequestClassification, Request} = case Approach of
        {reuse, RequestToReuse} ->
            % reminder reuses the same request id, which makes the previous decision
            % URL still valid (the space maintainer can use any of the emails to decide)
            {reminder, RequestToReuse#request{
                contact_email = ContactEmail,
                last_activity = ?NOW_SECONDS()
            }};
        create_new ->
            {first, #request{
                id = gen_request_id(UserId),
                contact_email = ContactEmail,
                last_activity = ?NOW_SECONDS()
            }}
    end,
    ?check(space_marketplace_mailer:check_send_membership_request(
        SpaceId, UserId, Request#request.id, RequestClassification, ContactEmail, Message
    )),
    UpdatedRecord#space_membership_requests{
        pending = maps:put(SpaceId, Request, UpdatedRecord#space_membership_requests.pending)
    }.


%% @private
-spec resolve_sanitized(od_space:id(), od_user:id(), decision(), record()) ->
    record() | no_return().
resolve_sanitized(SpaceId, RequesterUserId, Decision, Record = #space_membership_requests{
    pending = Pending,
    rejected = Rejected
}) ->
    {Request = #request{contact_email = ContactEmail}, NewPending} = maps:take(SpaceId, Pending),
    RecordWithUpdatedPending = Record#space_membership_requests{pending = NewPending},
    case Decision of
        grant ->
            ?check(space_logic:add_user(?ROOT, SpaceId, RequesterUserId)),
            space_marketplace_mailer:best_effort_notify_request_resolved(SpaceId, ContactEmail, Decision),
            RecordWithUpdatedPending;
        {reject, _} ->
            space_marketplace_mailer:best_effort_notify_request_resolved(SpaceId, ContactEmail, Decision),
            trim_overflowing_rejected_history(RecordWithUpdatedPending#space_membership_requests{
                rejected = Rejected#{SpaceId => Request#request{last_activity = ?NOW_SECONDS()}}
            })
    end.


%% @private
-spec qualify(od_space:id(), od_user:id(), record()) ->
    {create_new | {reuse, request()}, record()} | no_return().
qualify(_SpaceId, _UserId, Record) when map_size(Record#space_membership_requests.pending) >= ?PENDING_REQUEST_LIMIT ->
    throw(?ERROR_LIMIT_REACHED(?PENDING_REQUEST_LIMIT, <<"pending space membership requests">>));
qualify(SpaceId, UserId, Record) ->
    assert_not_a_member(UserId, SpaceId),
    NewRecord = check_if_not_recently_rejected(SpaceId, Record),
    case maps:find(SpaceId, NewRecord#space_membership_requests.pending) of
        error ->
            {create_new, NewRecord};
        {ok, #request{last_activity = LastActivity} = Request} ->
            NextRequestAllowedAt = LastActivity + ?MIN_BACKOFF_BETWEEN_REMINDERS_SECONDS,
            case ?NOW_SECONDS() >= NextRequestAllowedAt of
                true ->
                    {{reuse, Request}, NewRecord};
                false ->
                    throw(?ERROR_FORBIDDEN(str_utils:format_bin(
                        "A membership request to this space has been submitted recently. "
                        "A reminder can be generated not sooner than at ~ts",
                        [time:seconds_to_iso8601(NextRequestAllowedAt)]
                    )))
            end
    end.


%% @private
-spec assert_not_a_member(od_user:id(), od_space:id()) -> ok | no_return().
assert_not_a_member(UserId, SpaceId) ->
    case user_logic:has_eff_space(UserId, SpaceId) of
        false -> ok;
        true -> throw(?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_space, SpaceId))
    end.


%% @private
-spec check_if_not_recently_rejected(od_space:id(), record()) -> record() | no_return().
check_if_not_recently_rejected(SpaceId, Record) ->
    case maps:is_key(SpaceId, Record#space_membership_requests.rejected) of
        true ->
            NewRecord = forget_outdated_rejected_history(Record),
            case maps:find(SpaceId, NewRecord#space_membership_requests.rejected) of
                {ok, #request{last_activity = LastActivity}} ->
                    throw(?ERROR_FORBIDDEN(str_utils:format_bin(
                        "A membership request to this space has been recently rejected. "
                        "Another request can be made not sooner than at ~ts",
                        [time:seconds_to_iso8601(LastActivity + ?MIN_BACKOFF_AFTER_REJECTION_SECONDS)]
                    )));
                error ->
                    NewRecord
            end;
        false ->
            Record
    end.


-spec pruning_interval_passed(record()) -> boolean().
pruning_interval_passed(#space_membership_requests{last_pending_request_pruning_time = LastPendingRequestPruningTime}) ->
    ?NOW_SECONDS() - LastPendingRequestPruningTime >= ?PENDING_REQUEST_PRUNING_INTERVAL.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used to send notifications for multiple spaces
%% (which may be long-lasting) in an asynchronous manner.
%% @end
%%--------------------------------------------------------------------
-spec async_send_notifications_for_spaces(already_granted | cancelled, [od_space:id()], record()) -> pid().
async_send_notifications_for_spaces(_Type, [], _) ->
    ok;
async_send_notifications_for_spaces(Type, SpaceIds, #space_membership_requests{pending = Pending}) ->
    spawn(fun() ->
        % We accept the risk that some of the emails won't be sent, the notifications
        % are treated as best-effort (they serve as an extra information for the user,
        % but not critical for the proper operation of marketplace). Still, an
        % error will be logged in case of failures.
        ?catch_exceptions(lists_utils:pforeach(fun(SpaceId) ->
            #request{contact_email = Email} = maps:get(SpaceId, Pending),
            case Type of
                already_granted ->
                    space_marketplace_mailer:best_effort_notify_membership_already_granted(SpaceId, Email);
                cancelled ->
                    space_marketplace_mailer:best_effort_notify_request_cancelled(SpaceId, Email)
            end
        end, SpaceIds))
    end).


%% @private
-spec trim_overflowing_rejected_history(record()) -> record().
trim_overflowing_rejected_history(Record) ->
    case is_rejected_request_history_overflown(Record) of
        false ->
            Record;
        true ->
            #space_membership_requests{
                rejected = Rejected
            } = RecordWithOutdatedForgotten = forget_outdated_rejected_history(Record),
            case is_rejected_request_history_overflown(RecordWithOutdatedForgotten) of
                false ->
                    RecordWithOutdatedForgotten;
                true ->
                    % in case it's not possible to stay within history limit by forgetting
                    % only outdated requests, the oldest one is removed
                    RejectedSpacesSortedByLastActivity = lists:sort(fun(A, B) ->
                        (maps:get(A, Rejected))#request.last_activity < (maps:get(B, Rejected))#request.last_activity
                    end, maps:keys(Rejected)),
                    RecordWithOutdatedForgotten#space_membership_requests{
                        rejected = maps:remove(hd(RejectedSpacesSortedByLastActivity), Rejected)
                    }
            end
    end.


%% @private
-spec forget_outdated_rejected_history(record()) -> record().
forget_outdated_rejected_history(Record = #space_membership_requests{rejected = Rejected}) ->
    Now = ?NOW_SECONDS(),
    Record#space_membership_requests{rejected = maps:filter(fun(_, #request{last_activity = LastActivity}) ->
        Now < LastActivity + ?MIN_BACKOFF_AFTER_REJECTION_SECONDS
    end, Rejected)}.


%% @private
is_rejected_request_history_overflown(#space_membership_requests{rejected = Rejected}) ->
    maps:size(Rejected) > ?REJECTED_REQUEST_HISTORY_LENGTH.


%% @private
-spec encode(json | db, record()) -> json_utils:json_term().
encode(json, #space_membership_requests{pending = Pending, rejected = Rejected}) ->
    #{
        <<"pending">> => maps:map(fun(_SpaceId, R) -> request_to_json(R) end, Pending),
        <<"rejected">> => maps:map(fun(_SpaceId, R) -> request_to_json(R) end, Rejected)
    };
encode(db, #space_membership_requests{last_pending_request_pruning_time = LPT} = Record) ->
    maps:merge(encode(json, Record), #{<<"lastPendingRequestPruningTime">> => LPT}).


%% @private
-spec decode(json | db, json_utils:json_term()) -> record().
decode(json, #{<<"pending">> := PendingJson, <<"rejected">> := RejectedJson}) ->
    #space_membership_requests{
        pending = maps:map(fun(_SpaceId, R) -> request_from_json(R) end, PendingJson),
        rejected = maps:map(fun(_SpaceId, R) -> request_from_json(R) end, RejectedJson)
    };
decode(db, #{<<"lastPendingRequestPruningTime">> := LPT} = JsonRecord) ->
    Record = decode(json, JsonRecord),
    Record#space_membership_requests{last_pending_request_pruning_time = LPT}.


%% @private
-spec request_to_json(request()) -> json_utils:json_term().
request_to_json(#request{
    id = Id,
    contact_email = ContactEmail,
    last_activity = LastActivity
}) ->
    #{
        <<"requestId">> => Id,
        <<"contactEmail">> => ContactEmail,
        <<"lastActivity">> => LastActivity
    }.


%% @private
-spec request_from_json(json_utils:json_term()) -> request().
request_from_json(#{
    <<"requestId">> := Id,
    <<"contactEmail">> := ContactEmail,
    <<"lastActivity">> := LastActivity
}) ->
    #request{
        id = Id,
        contact_email = ContactEmail,
        last_activity = LastActivity
    }.
