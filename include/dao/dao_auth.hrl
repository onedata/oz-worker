%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Dao definitions for authorization records
%%% @end
%%%-------------------------------------------------------------------

-ifndef(DAO_AUTH_HRL).
-define(DAO_AUTH_HRL, 1).

%% Records of this type store a macaroons secret
-record(auth, {
    secret :: binary(),
    user_id :: binary()
}).

-endif.
