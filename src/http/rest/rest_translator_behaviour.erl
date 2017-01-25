%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour should be implemented by modules that translate
%%% entity logic results into REST HTTP replies.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_translator_behaviour).

-include("rest.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-callback response(Operation :: n_entity_logic:operation(),
    EntityId :: n_entity_logic:entity_id(), Resource :: n_entity_logic:resource(),
    Result :: n_entity_logic:result()) -> #rest_resp{}.

