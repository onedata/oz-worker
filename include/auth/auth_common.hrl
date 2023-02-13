%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common macros for modules related to authentication and
%%% authorization.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AUTH_COMMON_HRL).
-define(AUTH_COMMON_HRL, 1).

-include_lib("ctool/include/logging.hrl").

-define(CURRENT_CONFIG_VERSION, 3).

-define(XRDS_CACHE_TTL, oz_worker:get_env(openid_xrds_cache_ttl_seconds, 3600)). % 1 hour

-define(bin(Term), auth_config:ensure_bin(Term)).
-define(str(Term), auth_config:ensure_str(Term)).


% Login macros to use during login process handling, if auth test mode is
% enabled, the logs will be gathered and displayed on the login results page.
-define(auth_debug(Format, Args), begin
    idp_auth_test_mode:gather_log(debug, Format, Args),
    ?debug(Format, Args)
end).
-define(auth_debug_exception(Format, Args, Class, Reason, Stacktrace), begin
    idp_auth_test_mode:gather_log(
        debug,
        Format ++ "~n> Caught: ~s:~p~n> Stacktrace:~s",
        Args ++ [Class, Reason, lager:pr_stacktrace(Stacktrace)]
    ),
    ?debug_exception(Format, Class, Reason, Args, Stacktrace)
end).

-define(auth_warning(Format, Args), begin
    idp_auth_test_mode:gather_log(warning, Format, Args),
    ?warning(Format, Args)
end).

-define(auth_error(Format, Args), begin
    idp_auth_test_mode:gather_log(error, Format, Args),
    ?error(Format, Args)
end).


-endif.
