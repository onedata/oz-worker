%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Module responsible for handling "Identify" OAI-PMH request.
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#Identify
%%% @end
%%%-------------------------------------------------------------------
-module(identify).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").

-behaviour(oai_verb_behaviour).

%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_response/2]).


%%%===================================================================
%%% API
%%%===================================================================

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
    <<"protocolVersion">>, <<"adminEmail">>,
    <<"earliestDatestamp">>, <<"deletedRecord">>,
    <<"granularity">>
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
-spec get_response(binary(), [proplists:property()]) -> oai_response().
get_response(<<"repositoryName">>, _Args) ->
    oz_worker:get_name();
get_response(<<"baseURL">>, _Args) ->
    Domain = oz_worker:get_domain(),
    {ok, OAIPrefix} = oz_worker:get_env(oai_pmh_api_prefix),
    str_utils:format_bin("~s~s", [Domain, OAIPrefix]);
get_response(<<"protocolVersion">>, _Args) ->
    ?PROTOCOL_VERSION;
get_response(<<"earliestDatestamp">>, _Args) ->
    case get_earliest_datestamp() of
        none -> <<"Repository is empty">>;
        Datestamp -> oai_utils:datetime_to_oai_datestamp(Datestamp)
    end;
get_response(<<"deletedRecord">>, _Args) ->
    <<"no">>;
get_response(<<"granularity">>, _Args) ->
    <<"YYYY-MM-DDThh:mm:ssZ">>;
get_response(<<"adminEmail">>, _Args) ->
    {ok, AdminEmails} = oz_worker:get_env(admin_emails),
    lists:map(fun(AdminEmail) ->
        list_to_binary(AdminEmail)
    end, AdminEmails);
get_response(<<"compression">>, _Args) -> <<"">>;
%%% TODO by default 'Identity' compressions is supported.
%%% TODO optionally we can add support for other types of compression
%%% TODO compressions other than 'Identity' must be enumerated in Identify request
get_response(<<"description">>, _Args) -> [].
%%% TODO add repository description



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Returns earliest metadata datestamp.
%%% @end
%%%-------------------------------------------------------------------
-spec get_earliest_datestamp() -> erlang:datetime().
get_earliest_datestamp() ->
    Ids = oai_utils:list_handles(),
    Datestamps = lists:map(fun(Id) ->
        #od_handle{timestamp = Timestamp} = oai_utils:get_handle(Id),
        Timestamp
    end, Ids),
    case Datestamps of
        [] -> none;
        _ -> hd(lists:sort(fun oai_utils:is_earlier_or_equal/2, Datestamps))
    end.
