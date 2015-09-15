%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Dao definitions for token records
%%% @end
%%%-------------------------------------------------------------------

-ifndef(DAO_TOKENS_HRL).
-define(DAO_TOKENS_HRL, 1).

%% This record defines a token that can be used by user to do something
-record(token, {
    secret :: binary(),
    resource :: atom(),
    resource_id :: binary()
}).

-endif.
