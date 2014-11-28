%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Provider channel common definitions.
%% @end
%% ===================================================================
-author("Krzysztof Trzepla").

-ifndef(OP_CHANNEL_HRL).
-define(OP_CHANNEL_HRL, 1).

-define(op_channel_endpoint, <<"/oneprovider">>).
-define(op_channel_listener, op_channel).

-record(state, {providers = maps:new(), connections = maps:new()}).

-endif.