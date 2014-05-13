%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% dao_tokens header
%%% @end
%%% Created : 12. May 2014 12:54 PM
%%%-------------------------------------------------------------------

-ifndef(DAO_TOKENS_HRL).
-define(DAO_TOKENS_HRL, 1).

%% This record defines a token that can be used by user to do something
-record(token, {type = undefined, expires = []}).

-endif.