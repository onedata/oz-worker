%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module responsible for sending emails related to space marketplace
%%% using the onezone_mailer.
%%% @end
%%%-------------------------------------------------------------------
-module(space_marketplace_mailer).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([check_send_membership_request/6]).
-export([best_effort_notify_request_resolved/3]).
-export([best_effort_notify_membership_already_granted/2]).
-export([best_effort_notify_request_cancelled/2]).


-define(GREETING, <<"Dear Onedata user,">>).
-define(FOOTER, <<
    "This is an automated message - please do not reply directly to this email.\n"
    "\n"
    "Onezone website: ", (oz_worker:get_url())/binary
>>).


%%%===================================================================
%%% API
%%%===================================================================

-spec check_send_membership_request(
    od_space:id(),
    od_user:id(),
    space_membership_requests:request_id(),
    first | reminder,
    od_user:email(),
    binary()
) ->
    ok | ?ERROR_INTERNAL_SERVER_ERROR(_).
check_send_membership_request(SpaceId, RequesterUserId, RequestId, RequestClassification, RequesterEmail, Message) ->
    {SpaceName, MarketplaceContactEmail} = get_space_info(SpaceId),
    {RequesterFullName, RequesterUsername} = get_user_info(RequesterUserId),
    DecisionUri = build_gui_decision_uri(SpaceId, RequestId),
    OpeningSentence = str_utils:format_bin(case RequestClassification of
        first ->
            "A new membership request for space '~ts' (id: ~s) has been posted by:";
        reminder ->
            "This is a kind reminder about the membership request for space '~ts' (id: ~s) that has been posted by:"
    end, [SpaceName, SpaceId]),
    Subject = str_utils:format_bin(
        "~s membership request for space '~ts'",
        [
            case RequestClassification of
                first -> "New";
                reminder -> "REMINDER:"
            end,
            SpaceName
        ]
    ),
    Body = str_utils:format_bin(
        "~s~n"
        "~n"
        "~ts~n"
        "~n"
        "Full name: ~ts~n"
        "Username: ~ts~n"
        "E-mail address: ~ts~n"
        "User ID: ~s~n"
        "~ts"
        "~n"
        "Please decide upon acceptance or rejection of the request without undue delay. Visit the link below:~n"
        "~s~n"
        "~n"
        "Membership requests can be made by any user since this space is advertised in the space marketplace. "
        "If you wish to disable the space advertisement, use the space configuration menu in Web GUI.~n"
        "~n"
        "~s",
        [
            ?GREETING,
            OpeningSentence,
            RequesterFullName,
            RequesterUsername,
            RequesterEmail,
            RequesterUserId,
            case Message of <<"">> -> ""; _ -> str_utils:format("Message: ~ts~n", [Message]) end,
            DecisionUri,
            ?FOOTER
        ]
    ),
    check_send(MarketplaceContactEmail, Subject, Body).


-spec best_effort_notify_request_resolved(
    od_space:id(),
    od_user:email(),
    space_membership_requests:decision()
) ->
    ok | ?ERROR_INTERNAL_SERVER_ERROR(_).
best_effort_notify_request_resolved(SpaceId, UserContactEmail, Decision) ->
    {SpaceName, _} = get_space_info(SpaceId),
    DecisionStr = case Decision of
        grant -> "GRANTED";
        {reject, _} -> "REJECTED"
    end,
    ExtraInfo = case Decision of
        {reject, Reason} ->
            case Reason of
                <<"">> ->
                    "No reason for rejection was provided.";
                _ ->
                    str_utils:format(
                    "Reason: ~n"
                    "~ts~n"
                    "~n",
                    [Reason]
                )
            end;

        grant ->
            str_utils:format(
                "You may start using the space. View it in Web GUI by clicking the link below:~n"
                "~s~n"
                "~n",
                [build_gui_space_view_uri(SpaceId)]
            )
    end,
    Subject = str_utils:format_bin("Membership request ~s - space '~ts'", [DecisionStr, SpaceName]),
    Body = str_utils:format_bin(
        "~s~n"
        "~n"
        "Your membership request for space '~ts' (id: ~s) has been ~s by the space maintainer.~n"
        "~n"
        "~ts"
        "~s",
        [
            ?GREETING,
            SpaceName, SpaceId, DecisionStr,
            ExtraInfo,
            ?FOOTER
        ]
    ),
    best_effort_send(UserContactEmail, Subject, Body).


-spec best_effort_notify_membership_already_granted(
    od_space:id(),
    od_user:email()
) ->
    ok | ?ERROR_INTERNAL_SERVER_ERROR(_).
best_effort_notify_membership_already_granted(SpaceId, UserContactEmail) ->
    {SpaceName, _} = get_space_info(SpaceId),
    Subject = str_utils:format_bin("Membership already GRANTED - space '~ts'", [SpaceName]),
    Body = str_utils:format_bin(
        "~s~n"
        "~n"
        "Your membership request for space '~ts' (id: ~s) has been withdrawn "
        "since access to the space has been GRANTED to you independently of space marketplace.~n"
        "~n"
        "~s",
        [
            ?GREETING,
            SpaceName, SpaceId,
            ?FOOTER
        ]
    ),
    best_effort_send(UserContactEmail, Subject, Body).


-spec best_effort_notify_request_cancelled(
    od_space:id(),
    od_user:email()
) ->
    ok | ?ERROR_INTERNAL_SERVER_ERROR(_).
best_effort_notify_request_cancelled(SpaceId, UserContactEmail) ->
    SpaceName = try
        {Name, _} = get_space_info(SpaceId),
        Name
    catch _:_ ->
        <<"Unknown">>
    end,
    Subject = str_utils:format_bin("Membership request CANCELLED - space '~ts'", [SpaceName]),
    Body = str_utils:format_bin(
        "~s~n"
        "~n"
        "Your membership request for space '~ts' (id: ~s) has been CANCELLED.~n"
        "Possible reasons: the space has been deleted or is no longer advertised in the marketplace.~n"
        "~n"
        "~s",
        [
            ?GREETING,
            SpaceName, SpaceId,
            ?FOOTER
        ]
    ),
    best_effort_send(UserContactEmail, Subject, Body).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% URI of the page in GUI where the space maintainer makes a decision
%% whether to grant/reject given membership request.
%% @end
%%--------------------------------------------------------------------
-spec build_gui_decision_uri(od_space:id(), space_membership_requests:request_id()) -> http_client:url().
build_gui_decision_uri(SpaceId, RequestId) ->
    oz_worker:get_uri(str_utils:format_bin(
        "/#/onedata?action_name=confirmJoinSpaceRequest&action_spaceId=~s&action_requestId=~s",
        [SpaceId, RequestId]
    )).

%% @private
-spec build_gui_space_view_uri(od_space:id()) -> http_client:url().
build_gui_space_view_uri(SpaceId) ->
    oz_worker:get_uri(str_utils:format_bin("/#/onedata/spaces/~s", [SpaceId])).


%% @private
-spec get_space_info(od_space:id()) -> {od_space:name(), od_space:marketplace_contact_email()}.
get_space_info(SpaceId) ->
    {ok, #document{value = #od_space{
        name = Name,
        marketplace_contact_email = MarketplaceContactEmail
    }}} = od_space:get(SpaceId),
    {Name, MarketplaceContactEmail}.


%% @private
-spec get_user_info(od_user:id()) -> {od_user:full_name(), od_user:username()}.
get_user_info(UserId) ->
    {ok, #document{value = #od_user{
        full_name = FullName,
        username = Username
    }}} = od_user:get(UserId),
    {FullName, utils:ensure_defined(Username, <<"none">>)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used for emails that are critical; a failure to deliver should raise an exception.
%% @end
%%--------------------------------------------------------------------
-spec check_send(od_user:email(), binary(), binary()) -> ok | no_return().
check_send(Recipient, Subject, Body) ->
    ?check(onezone_mailer:send([Recipient], Subject, Body)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used in case where a failure to deliver an email should not cause failure
%% of the overall procedure.
%% Errors will be logged internally by onezone_mailer.
%% @end
%%--------------------------------------------------------------------
-spec best_effort_send(od_user:email(), binary(), binary()) -> ok.
best_effort_send(Recipient, Subject, Body) ->
    onezone_mailer:send([Recipient], Subject, Body),
    ok.