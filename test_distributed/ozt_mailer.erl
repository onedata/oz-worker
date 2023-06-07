%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tools for mocking the onezone mailer service in CT tests and verification
%%% of sent mail expectations.
%%%
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_mailer).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([mock/0, unmock/0]).
-export([toggle_error_simulation/2]).
-export([match_received_emails/4]).

-record(received_mail, {
    subject :: binary(),
    body :: binary(),
    timestamp = global_clock:timestamp_seconds() :: time:seconds()
}).

-define(SIMULATE_ERRORS_KEY(EmailAddress), {simulate_errors, EmailAddress}).
-define(MAIL_HISTORY_KEY(RecipientAddress), {mail_history, RecipientAddress}).


%%%===================================================================
%%% API
%%%===================================================================

-spec mock() -> ok.
mock() ->
    ozt_mocks:mock_new(onezone_mailer),
    ozt_mocks:mock_expect(onezone_mailer, send, fun oz_node_mailer_send_mock/3).


-spec unmock() -> ok.
unmock() ->
    ozt_mocks:mock_unload([onezone_mailer]).


%%--------------------------------------------------------------------
%% @doc
%% If enabled, all attempts to sent and email to this address will fail with an error.
%% @end
%%--------------------------------------------------------------------
-spec toggle_error_simulation(od_user:email(), boolean()) -> ok.
toggle_error_simulation(EmailAddress, Flag) ->
    testmaster_save_on_oz_nodes(?SIMULATE_ERRORS_KEY(EmailAddress), Flag).


-spec match_received_emails(smtp_client:email_address(), time:seconds(), [string() | binary()], [string() | binary()]) ->
    boolean().
match_received_emails(Address, Timestamp, SubjectKeywords, BodyKeywords) ->
    FullHistory = testmaster_get_email_history(Address),
    MatchingByTimestamp = lists:filter(fun(#received_mail{timestamp = T}) ->
        T == Timestamp
    end, FullHistory),
    MatchingBySubject = lists:filter(fun(#received_mail{subject = Subject}) ->
        lists:all(fun(Keyword) ->
            nomatch /= string:find(Subject, str_utils:unicode_list_to_binary(Keyword))
        end, SubjectKeywords)
    end, MatchingByTimestamp),
    lists:filter(fun(#received_mail{body = Body}) ->
        lists:all(fun(Keyword) ->
            nomatch /= string:find(Body, str_utils:unicode_list_to_binary(Keyword))
        end, BodyKeywords)
    end, MatchingBySubject).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
    -spec oz_node_mailer_send_mock([smtp_client:email_address()], binary(), binary()) ->
    ok | ?ERROR_INTERNAL_SERVER_ERROR(_).
oz_node_mailer_send_mock(RecipientAddresses, Subject, Body) ->
    case lists:any(fun(Addr) -> oz_node_get_saved(?SIMULATE_ERRORS_KEY(Addr), false) end, RecipientAddresses) of
        true ->
            ?ERROR_INTERNAL_SERVER_ERROR(str_utils:rand_hex(6));
        false ->
            lists:foreach(fun(RecipientAddress) ->
                oz_node_collect_email(RecipientAddress, Subject, Body)
            end, RecipientAddresses)
    end.


%% @private
-spec oz_node_collect_email(smtp_client:email_address(), binary(), binary()) -> ok.
oz_node_collect_email(RecipientAddress, Subject, Body) ->
    oz_node_update_on_nodes(?MAIL_HISTORY_KEY(RecipientAddress), fun(PreviousMailHistory) ->
        {ok, PreviousMailHistory ++ [#received_mail{subject = Subject, body = Body}], infinity}
    end, []).


%% @private
-spec testmaster_save_on_oz_nodes(atom(), term()) -> ok.
testmaster_save_on_oz_nodes(Key, Value) ->
    ozt:rpc_multicall(node_cache, put, [Key, Value]).


%% @private
-spec testmaster_get_saved(atom(), term()) -> term().
testmaster_get_saved(Key, Default) ->
    ozt:rpc(node_cache, get, [Key, Default]).


%% @private
-spec testmaster_get_email_history(smtp_client:email_address()) -> [#received_mail{}].
testmaster_get_email_history(RecipientAddress) ->
    testmaster_get_saved(?MAIL_HISTORY_KEY(RecipientAddress), []).


%% @private
-spec oz_node_update_on_nodes(atom(), node_cache:update_callback(), term()) -> ok.
oz_node_update_on_nodes(Key, UpdateCallback, Default) ->
    utils:rpc_multicall(consistent_hashing:get_all_nodes(), node_cache, update, [Key, UpdateCallback, Default]),
    ok.


%% @private
-spec oz_node_get_saved(atom(), term()) -> term().
oz_node_get_saved(Key, Default) ->
    node_cache:get(Key, Default).