%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements onezone_plugin_behaviour and harvester_plugin_behaviour 
%%% and is called by harvester_logic_plugin to handle operations on Elasticsearch.
%%% This plugin conforms to Elasticsearch 6.x
%%% @end
%%%-------------------------------------------------------------------
-module(elasticsearch_plugin).
-author("Michal Stanisz").

-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/logging.hrl").

-export([type/0]).
-export([
    get_name/0,
    ping/1,
    create_index/3, delete_index/2,
    submit_batch/4,
    query_index/3,
    query_validator/0
]).

-behaviour(onezone_plugin_behaviour).
-behaviour(harvester_plugin_behaviour).

-define(ENTRY_PATH(Path), <<"/_doc/", Path/binary>>).


%%--------------------------------------------------------------------
%% @doc
%% {@link onezone_plugin_behaviour} callback type/0.
%% @end
%%--------------------------------------------------------------------
type() ->
    harvester_plugin.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback get_name/0
%% @end
%%--------------------------------------------------------------------
-spec get_name() -> binary().
get_name() ->
    <<"Elasticsearch plugin">>.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback ping/1
%% @end
%%--------------------------------------------------------------------
-spec ping(od_harvester:endpoint()) -> {ok | {error, term()}}.
ping(Endpoint) ->
    case do_request(get, Endpoint, <<>>, <<>>, <<>>, [200]) of
        {ok, _,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback create_index/4.
%% @end
%%--------------------------------------------------------------------
-spec create_index(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:schema()) ->
    {ok | {error, term()}}.
create_index(Endpoint, IndexId, Schema) ->
    NewSchema = case Schema of
        undefined -> <<"{}">>;
        Schema -> Schema
    end,
    case do_request(put, Endpoint, IndexId, <<>>, NewSchema, [{200,300}]) of
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback delete_index/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_index(od_harvester:endpoint(), od_harvester:index_id()) -> {ok | {error, term()}}.
delete_index(Endpoint, IndexId) ->
    case do_request(delete, Endpoint, IndexId, <<>>, <<>>, [{200,300}, 404]) of
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback submit_batch/4.
%% @end
%%--------------------------------------------------------------------
-spec submit_batch(od_harvester:endpoint(), od_harvester:id(), od_harvester:indices(), od_harvester:batch()) -> 
    {ok, [{od_harvester:index_id(), {SuccessfulSeq :: integer() | undefined, 
        {FailedSeq :: integer(), Error :: binary()} | undefined}}]}.
submit_batch(Endpoint, HarvesterId, Indices, Batch) ->
    PreparedBatch = prepare_elasticsearch_batch(Batch),
    FirstSeq = maps:get(<<"seq">>, lists:nth(1, Batch)),
    {ok, utils:pmap(fun(IndexId) ->
        case do_request(post, Endpoint, IndexId, ?ENTRY_PATH(<<"_bulk">>), PreparedBatch,
            #{<<"content-type">> => <<"application/x-ndjson">>}, [{recv_timeout, 15000}], [{200,300}]) of
            {ok,_,_,Body} ->
                Res = json_utils:decode(Body),
                case maps:get(<<"errors">>, Res) of
                    false -> {IndexId, ok};
                    true -> {IndexId, parse_batch_result(Res, Batch, HarvesterId, IndexId)}
                end;
            {error, _} = Error -> 
                ?debug("Error when updating index ~p in harvester ~p: ~p", [IndexId, HarvesterId, Error]),
                ErrorMsg = case error_rest_translator:translate(Error) of
                    {_, {MessageFormat, FormatArgs}} ->
                        str_utils:format_bin(
                            str_utils:to_list(MessageFormat), FormatArgs
                        );
                    {_, MessageBinary} -> MessageBinary
                end,
                {IndexId, {undefined, {FirstSeq, ErrorMsg}}}
        end
    end, Indices)}.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback query/3.
%% @end
%%--------------------------------------------------------------------
-spec query_index(od_harvester:endpoint(), od_harvester:index_id(), Data :: #{}) ->
    {ok, map()} | {error, term()}.
query_index(Endpoint, IndexId, Data) ->
    #{
        <<"method">> := Method,
        <<"path">> := Path
    } = Data,
    Body = maps:get(<<"body">>, Data, <<>>),
    case do_request(Method, Endpoint, IndexId, ?ENTRY_PATH(Path), Body) of
        {ok, Code, Headers, ResponseBody} ->
            {ok, #{
                <<"code">> => Code,
                <<"headers">> => Headers,
                <<"body">> => ResponseBody
            }};
        {error, _} = Error -> 
            Error
    end.
    

%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback query_validator/0.
%% @end
%%--------------------------------------------------------------------
-spec query_validator() -> map().
query_validator() -> #{
    required => #{
        <<"method">> => {atom, [post, get]},
        <<"path">> => {binary, non_empty}
    },
    optional => #{
        <<"body">> => {binary, non_empty}
    }
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @equiv do_request(Method, Endpoint, HarvesterId, IndexId, Path, Data, undefined).
%% @end
%%--------------------------------------------------------------------
-spec do_request(http_client:method(), od_harvester:endpoint(),
    od_harvester:index_id(), Path :: binary(), Data :: binary()) -> ok.
do_request(Method, Endpoint, IndexId, Path, Data) ->
    do_request(Method, Endpoint, IndexId, Path, Data, undefined).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% @equiv do_request(Method, Endpoint, HarvesterId, IndexId, Path, Data, 
%%          #{<<"content-type">> => <<"application/json">>}, [], ExpectedCodes).
%% @end
%%--------------------------------------------------------------------
-spec do_request(http_client:method(), od_harvester:endpoint(),
    od_harvester:index_id(), Path :: binary(), Data :: binary(),
    ExpectedCodes :: [integer() | {integer(), integer()}] | undefined) -> ok.
do_request(Method, Endpoint, IndexId, Path, Data, ExpectedCodes) ->
    do_request(Method, Endpoint, IndexId, Path, Data,
        #{<<"content-type">> => <<"application/json">>}, [], ExpectedCodes).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes request to elasticsearch.
%% When ExpectedCodes lists is provided returns error if response has wrong status code. 
%% ExpectedCodes list must conform to specification in is_code_expected/2.
%% @end
%%--------------------------------------------------------------------
-spec do_request(http_client:method(), od_harvester:endpoint(), od_harvester:index_id(),
    Path :: binary(), Data :: binary(), http_client:headers(), http_client:opts(),
    ExpectedCodes :: [integer() | {integer(), integer()}] | undefined) -> ok.
do_request(Method, Endpoint, IndexId, Path, Data, Headers, Opts, ExpectedCodes) ->
    Url = <<Endpoint/binary, "/", IndexId/binary, Path/binary>>,
    case http_client:request(Method, Url, Headers, Data, Opts) of
        {ok, Code, RespHeaders, Body} = Response when is_list(ExpectedCodes) ->
            case is_code_expected(Code, ExpectedCodes) of
                true -> 
                    Response;
                _ ->
                    ?debug("~p ~p returned unexpected response ~p:~n ~p~n~p",
                        [Method, Url, Code, RespHeaders, json_utils:decode(Body)]),
                    ?ERROR_BAD_DATA(<<"payload">>)
            end;
        {ok,_,_,_} = Response ->
            Response;
        {error, _} = Error ->
            ?error("~p ~p was unsuccessful due to ~w",
                [Method, Url, Error]),
            ?ERROR_TEMPORARY_FAILURE
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate saying whether given Code is expected.
%% Each element of ExpectedCodes list can be in form of:
%% * ExpectedCode - represents that given status code is expected;
%% * {Begin, End} - represents range from Begin(inclusive) to End(exclusive) 
%%                  of expected status codes.
%% @end
%%--------------------------------------------------------------------
-spec is_code_expected(Code :: integer(), ExpectedCodes :: [integer() | {integer(), integer()}]) -> 
    boolean().
is_code_expected(Code, ExpectedCodes) ->
    lists:any(fun({B, E}) -> (Code >= B) and (Code < E);
                 (ECode) -> Code =:= ECode
    end, ExpectedCodes).


%% @private
-spec prepare_elasticsearch_batch(od_harvester:batch()) -> binary().
prepare_elasticsearch_batch(Batch) ->
    Requests = lists:map(fun(BatchEntry) ->
        #{
            <<"operation">> := Operation,
            <<"fileId">> := EntryId
        } = BatchEntry,
        ESOperation = case Operation of
            <<"submit">> -> <<"index">>;
            <<"delete">> -> <<"delete">>
        end,
        Payload = maps:get(<<"payload">>, BatchEntry, #{}),
        % submit only JSON, ignore other metadata, delete entry when there is no JSON
        {FinalOperation, Data} = case maps:find(<<"json">>, Payload) of
            {ok, JSON} -> {ESOperation, JSON};
            _ -> {<<"delete">>, undefined}
        end,
        Req = json_utils:encode(#{FinalOperation => #{<<"_id">> => EntryId}}),
        case FinalOperation of
            <<"index">> -> str_utils:join_binary([Req, Data], <<"\n">>);
            _ -> Req
        end
    end, Batch),
    <<(str_utils:join_binary(Requests, <<"\n">>))/binary, "\n">>.


%% @private
-spec parse_batch_result(Res :: map(), od_harvester:batch(), od_harvester:id(), od_harvester:index_id()) -> 
    {SuccessfulSeq :: integer() | undefined, {FailedSeq :: integer(), Error :: binary()} | undefined}.
parse_batch_result(Res, Batch, HarvesterId, IndexId) ->
    lists:foldl(
        fun({EntryResponse, BatchEntry}, {PrevSeq, undefined}) -> 
            Seq = maps:get(<<"seq">>, BatchEntry),
            case get_entry_response_error(EntryResponse) of
                undefined -> 
                    {Seq, undefined};
                {ErrorType, Error} ->
                    case is_es_error_ignored(ErrorType) of
                        true ->
                            ?debug("Entry submit in harvester ~p in index ~p dropped because of not matching schema: ~p",
                                [HarvesterId, IndexId, Error]),
                            {Seq, undefined};
                        false ->
                            ?debug("Unexpected error in batch response in harvester ~p in index ~p: ~p", 
                                [HarvesterId, IndexId, Error]),
                            {PrevSeq, {Seq, ErrorType}}
                    end
            end;
           (_, Acc) -> 
               % ignore rest of response when first error is found 
               Acc 
        end, {undefined, undefined}, lists:zip(maps:get(<<"items">>, Res), Batch)).


%% @private
-spec get_entry_response_error(EntryResponse :: map()) ->
    {ErrorType :: binary(), ErrorMap :: map()} | undefined.
get_entry_response_error(EntryResponse) ->
    InnerMap = case maps:find(<<"index">>, EntryResponse) of
        {ok, M} -> M;
        _ -> maps:get(<<"delete">>, EntryResponse)
    end,
    case maps:get(<<"error">>, InnerMap, undefined) of
        undefined -> undefined;
        ErrorMap -> {maps:get(<<"type">>, ErrorMap, <<"unexpected_error">>), ErrorMap}
    end.


%% @private
-spec is_es_error_ignored(Error :: binary()) -> boolean().
is_es_error_ignored(<<"mapper_parsing_exception">>) -> true;
is_es_error_ignored(<<"strict_dynamic_mapping_exception">>) -> true;
is_es_error_ignored(_) -> false.
