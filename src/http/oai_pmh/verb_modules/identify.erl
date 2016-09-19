%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(identify).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").
-include("registered_names.hrl").

-behaviour(oai_verb_behaviour).

%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_response/2]).


%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback required_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec required_arguments() -> [binary()].
required_arguments() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_arguments() -> [binary()].
optional_arguments() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback exclusive_arguments/0
%%% @end
%%%--------------------------------------------------------------------
-spec exclusive_arguments() -> [binary()].
exclusive_arguments() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback required_response_elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec required_response_elements() -> [binary()].
required_response_elements() -> [
    <<"repositoryName">>, <<"baseURL">>,
    <<"protocolVersion">>, <<"earliestDatestamp">>,
    <<"deletedRecord">>, <<"granularity">>,
    <<"adminEmail">>
].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_response_elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_response_elements() -> [binary()].
optional_response_elements() ->
    [<<"compression">>, <<"description">>].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback get_response/2
%%% @end
%%%-------------------------------------------------------------------
-spec get_response(binary(), proplist()) -> oai_response().
get_response(<<"repositoryName">>, _Args) ->
    <<"REPOSITORY NAME">>; % TODO what should be the repository name
get_response(<<"baseURL">>, _Args) ->
    {ok, Domain} = application:get_env(?APP_Name, http_domain),
    {ok, OAI_PREFIX} = application:get_env(?APP_Name, oai_pmh_api_prefix),
    list_to_binary(Domain ++ OAI_PREFIX);
get_response(<<"protocolVersion">>, _Args) ->
    ?PROTOCOL_VERSION;
get_response(<<"earliestDatestamp">>, _Args) ->
    <<"1970-01-01T00:00:00Z">>; % TODO how to get it
get_response(<<"deletedRecord">>, _Args) ->
    <<"no">>;
get_response(<<"granularity">>, _Args) ->
    <<"YYYY-MM-DDThh:mm:ss:Z">>;
get_response(<<"adminEmail">>, _Args) ->
    [<<"a@mail.com">>, <<"b@mail.com">>]; % TODO how to get them
get_response(<<"compression">>, _Args) ->
    <<"">>; %TODO
get_response(<<"description">>, _Args) -> [].
%% TODO do we need description ???


