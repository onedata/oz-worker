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

-export([client_to_subject/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts the obsolete #client{} record to #subject{}.
%% @end
%%--------------------------------------------------------------------
-spec client_to_subject(undefined | #client{}) -> undefined | aai:subject().
client_to_subject(undefined) -> undefined;
client_to_subject(#client{type = root}) -> ?SUB(root);
client_to_subject(#client{type = nobody}) -> ?SUB(nobody);
client_to_subject(#client{type = user, id = <<"">>}) -> ?SUB(nobody);
client_to_subject(#client{type = user, id = Id}) -> ?SUB(user, Id);
client_to_subject(#client{type = provider, id = <<"">>}) -> ?SUB(nobody);
client_to_subject(#client{type = provider, id = Id}) -> ?SUB(?ONEPROVIDER, Id).
