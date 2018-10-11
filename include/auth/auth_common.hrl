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

-define(bin(__Term), case __Term of
    undefined -> undefined;
    __List when is_list(__List) -> str_utils:unicode_list_to_binary(__List);
    __Binary when is_binary(__Binary) -> __Binary
end).

-define(str(__Term), case __Term of
    undefined -> undefined;
    __Atom when is_atom(__Atom) -> atom_to_list(__Atom);
    __Binary when is_binary(__Binary) -> str_utils:binary_to_unicode_list(__Binary);
    __Str when is_list(__Str) -> __Str
end).

-endif.

