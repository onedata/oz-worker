%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
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
%% Translates given entity logic CREATE result into REST response
%% expressed by #rest_resp{} record. GRI holds the #gri{} od the request,
%% new GRI holds the #gri{} of new aspect that was created.
%% @end
%%--------------------------------------------------------------------
-callback create_response(entity_logic:gri(), entity_logic:auth_hint(),
Result :: {data, term()} | {fetched, entity_logic:gri(), term()} |
{not_fetched, entity_logic:gri()} |
{not_fetched, entity_logic:gri(), entity_logic:auth_hint()}) -> #rest_resp{}.


%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic GET result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-callback get_response(entity_logic:gri(), Result :: term()) ->
    #rest_resp{}.
