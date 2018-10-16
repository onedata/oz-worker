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

-define(CURRENT_CONFIG_VERSION, 2).

-define(XRDS_CACHE_TTL, oz_worker:get_env(openid_xrds_cache_ttl, timer:hours(1))).

-define(bin(Term), auth_config:ensure_bin(Term)).
-define(str(Term), auth_config:ensure_str(Term)).

-endif.

