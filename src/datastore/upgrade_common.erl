%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common functions related to upgrading onezone datastore models.
%%% @end
%%%-------------------------------------------------------------------
-module(upgrade_common).
-author("Lukasz Opiola").

-include_lib("ctool/include/aai/aai.hrl").

% Obsolete #client{} record, replaced by #subject{} since 19.02.0-rc1
-record(client, {
    type = nobody :: user | provider | root | nobody,
    id = <<"">> :: binary()
}).

-type obsolete_subject_record() :: {subject, aai:subject_type(), aai:subject_id()}.

-export([client_to_subject/1]).
-export([upgrade_subject_record/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts the obsolete #client{} record to #subject{}.
%% @end
%%--------------------------------------------------------------------
-spec client_to_subject(undefined | #client{}) ->
    undefined | obsolete_subject_record().
client_to_subject(undefined) -> undefined;
client_to_subject(#client{type = root}) -> {subject, root, undefined};
client_to_subject(#client{type = nobody}) -> {subject, nobody, undefined};
client_to_subject(#client{type = user, id = <<"">>}) -> {subject, nobody, undefined};
client_to_subject(#client{type = user, id = Id}) -> {subject, user, Id};
client_to_subject(#client{type = provider, id = <<"">>}) -> {subject, nobody, undefined};
client_to_subject(#client{type = provider, id = Id}) -> {subject, oneprovider, Id}.


%%--------------------------------------------------------------------
%% @doc
%% Converts the obsolete #subject{} record to a newer one (3 fields instead of 2).
%% @end
%%--------------------------------------------------------------------
-spec upgrade_subject_record(undefined | obsolete_subject_record()) ->
    undefined | aai:subject().
upgrade_subject_record(undefined) -> undefined;
upgrade_subject_record({subject, root, undefined}) -> ?SUB(root);
upgrade_subject_record({subject, nobody, undefined}) -> ?SUB(nobody);
upgrade_subject_record({subject, user, Id}) -> ?SUB(user, Id);
upgrade_subject_record({subject, oneprovider, Id}) -> ?SUB(?ONEPROVIDER, Id).
