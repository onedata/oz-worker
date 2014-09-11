%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Dao definitions for token records
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-ifndef(DAO_TOKENS_HRL).
-define(DAO_TOKENS_HRL, 1).

%% This record defines a token that can be used by user to do something
-record(token, {
    value    :: binary(),
    type     :: token_logic:token_type(),
    expires  :: non_neg_integer(),
    resource :: {Type :: token_logic:resource_type(), Resource :: binary()}
}).

-endif.
