%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is used to handle test login flow, which can be used to test
%%% OIDC/SAML config without interrupting the Onezone service. The test login
%%% logic is realized using the process dictionary to minimize the interference
%%% with the production login code.
%%% @end
%%%-------------------------------------------------------------------
-module(idp_auth_test_mode).
-author("Lukasz Opiola").

%% API
-export([
    process_enable_test_mode/0, process_disable_test_mode/0,
    process_is_test_mode_enabled/0,
    store_user_data/1, get_user_data/0,
    gather_log/3, gather_log/4,
    get_logs/0
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Enables test mode for the current process. If enabled, IdPs config will be
%% acquired from test auth config file and the login process will be treated as
%% a dry run, presenting collected attributes rather than logging the user in.
%% @end
%%--------------------------------------------------------------------
-spec process_enable_test_mode() -> ok.
process_enable_test_mode() ->
    put(auth_test_mode_enabled, true),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Disables test mode for the current process.
%% @end
%%--------------------------------------------------------------------
-spec process_disable_test_mode() -> ok.
process_disable_test_mode() ->
    put(auth_test_mode_enabled, false),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if current process has the test mode enabled.
%% @end
%%--------------------------------------------------------------------
-spec process_is_test_mode_enabled() -> boolean().
process_is_test_mode_enabled() ->
    get(auth_test_mode_enabled) =:= true.


%%--------------------------------------------------------------------
%% @doc
%% Stores user data (in JSON compatible format) in process memory.
%% @end
%%--------------------------------------------------------------------
-spec store_user_data(json_utils:json_term()) -> ok.
store_user_data(UserData) ->
    put(auth_test_mode_user_data, UserData),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Return user data (in JSON compatible format) stored in process memory.
%% @end
%%--------------------------------------------------------------------
-spec get_user_data() -> json_utils:json_term().
get_user_data() ->
    get(auth_test_mode_user_data).


%%--------------------------------------------------------------------
%% @doc
%% @equiv gather_log(Loglevel, Format, Args, undefined)
%% @end
%%--------------------------------------------------------------------
-spec gather_log(Loglevel :: atom(), Format :: string(), Args :: [term()]) ->
    ok.
gather_log(Loglevel, Format, Args) ->
    gather_log(Loglevel, Format, Args, undefined).


%%--------------------------------------------------------------------
%% @doc
%% Computes a log message based on given format and args, includes stacktrace
%% (if specified) and appends it to the logs stored in process memory.
%% @end
%%--------------------------------------------------------------------
-spec gather_log(Loglevel :: atom(), Format :: string(), Args :: [term()],
    Stacktrace :: undefined | list()) -> ok.
gather_log(Loglevel, Format, Args, Stacktrace) ->
    case process_is_test_mode_enabled() of
        false ->
            ok;
        true ->
            append_to_log(str_utils:format("~s ~ts~s", [
                format_loglevel(Loglevel),
                str_utils:format(Format, Args),
                format_stacktrace(Stacktrace)
            ]))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the logs accumulated in process memory.
%% @end
%%--------------------------------------------------------------------
-spec get_logs() -> string().
get_logs() ->
    case get(auth_test_mode_log) of
        undefined -> "";
        Log -> Log
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec append_to_log(string()) -> ok.
append_to_log(Str) ->
    NewLog = case get_logs() of
        "" -> Str;
        Log -> Log ++ "\n\n" ++ Str
    end,
    put(auth_test_mode_log, NewLog),
    ok.


%% @private
-spec format_loglevel(atom()) -> string().
format_loglevel(debug) -> "[DEBUG]";
format_loglevel(warning) -> "[WARNG]";
format_loglevel(error) -> "[ERROR]".


%% @private
-spec format_stacktrace(undefined | list()) -> string().
format_stacktrace(undefined) ->
    "";
format_stacktrace(Stacktrace) ->
    str_utils:format("~nStacktrace: ~s", [
        iolist_to_binary(lager:pr_stacktrace(Stacktrace))
    ]).
