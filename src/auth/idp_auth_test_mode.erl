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
    set_up_for_current_pid/1,
    is_test_mode_enabled_for_current_pid/0,
    store_user_data/1, get_user_data/0,
    store_state_token/1, get_state_token/0,
    gather_log/3,
    get_logs/0
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Sets up the memory of the current process, enabling or disabling the test mode.
%% If enabled, IdPs config will be acquired from test auth config file and the
%% login process will be treated as a dry run, presenting collected attributes
%% rather than logging the user in.
%%
%% NOTE: the setup must be run every time an IdP login-related process starts to
%% make sure there are no remnants of previous executions in the process memory.
%% @end
%%--------------------------------------------------------------------
-spec set_up_for_current_pid(boolean()) -> ok.
set_up_for_current_pid(Flag) ->
    put(auth_test_mode_enabled, Flag),
    store_user_data(#{}),
    store_state_token(<<"undefined">>),
    clear_logs().


-spec is_test_mode_enabled_for_current_pid() -> boolean().
is_test_mode_enabled_for_current_pid() ->
    get(auth_test_mode_enabled) =:= true.


-spec store_user_data(json_utils:json_term()) -> ok.
store_user_data(UserData) ->
    put(auth_test_mode_user_data, UserData),
    ok.


-spec get_user_data() -> json_utils:json_term().
get_user_data() ->
    get(auth_test_mode_user_data).


-spec store_state_token(state_token:state_token()) -> ok.
store_state_token(UserData) ->
    put(auth_test_mode_state_token, UserData),
    ok.


-spec get_state_token() -> state_token:state_token().
get_state_token() ->
    get(auth_test_mode_state_token).


-spec gather_log(Loglevel :: atom(), Format :: string(), Args :: [term()]) -> ok.
gather_log(Loglevel, Format, Args) ->
    case is_test_mode_enabled_for_current_pid() of
        false ->
            ok;
        true ->
            append_to_log(str_utils:format("~ts ~ts", [
                format_loglevel(Loglevel),
                str_utils:format(Format, Args)
            ]))
    end.


-spec get_logs() -> string().
get_logs() ->
    case get(auth_test_mode_log) of
        undefined -> "";
        Log -> Log
    end.


-spec clear_logs() -> ok.
clear_logs() ->
    put(auth_test_mode_log, undefined),
    ok.


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
