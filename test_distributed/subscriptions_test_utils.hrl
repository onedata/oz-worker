%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Definitions of macros and records used in subscriptions tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Michal Zmuda").
-author("Jakub Kudzia").

-define(MESSAGES_WAIT_TIMEOUT, timer:seconds(2)).
-define(MESSAGES_RECEIVE_ATTEMPTS, 60).


%% appends function name to id (atom) and yields binary accepted by the db
-define(ID(Id), list_to_binary(atom_to_list(Id) ++ " # " ++ atom_to_list(
    element(2, element(2, process_info(self(), current_function)))))
).

%% helper record for maintaining subscription progress between message receives
-record(subs_ctx, {
    node :: node(),
    provider :: binary(),
    users :: [binary()],
    resume_at :: subscriptions:seq(),
    missing :: [subscriptions:seq()]
}).