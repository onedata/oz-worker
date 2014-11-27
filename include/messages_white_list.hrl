%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains list of communicates that can be processed
%% by Global Registry.
%% @end
%% ===================================================================

-ifndef(MESSAGES_WHITE_LIST_HRL).
-define(MESSAGES_WHITE_LIST_HRL, 1).

%% white lists of messages that can be processed by Global Registry
-define(MessagesWhiteList, [
  message, spacemodified, spaceremoved, usermodified, userremoved, groupmodified, groupremoved
]).

%% list of messages decoders that can be used
-define(DecodersList, [
  gr_communication_protocol, gr_messages
]).

-endif.
