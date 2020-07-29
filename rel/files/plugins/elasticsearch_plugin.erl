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
%%% This plugin is compatible with Elasticsearch 6.x and 7.x (up to 7.1).
%%% @end
%%%-------------------------------------------------------------------
-module(elasticsearch_plugin).
-author("Michal Stanisz").

-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/headers.hrl").

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

-type converted_xattrs() :: #{binary() => converted_xattrs() | binary()}.
-type es_batch() :: binary(). %% @TODO VFS-6546 Add example 
-type rejected_fields() :: all | [binary()]. %% @TODO VFS-6546 Add example 
-type index_submit_response() :: ok | {error, SuccessfulSeq :: pos_integer() | undefined,
        FailedSeq :: pos_integer(), Error :: binary()}.

-define(ENTRY_PATH(Path), <<"/_doc/", Path/binary>>).
-define(REQUEST_TIMEOUT, oz_worker:get_env(elasticsearch_plugin_request_timeout, timer:minutes(2))).

-define(REQUEST_RETURN_OK(Response),
    case Response of
        {ok, _,_,_} -> ok;
        {error, _} = Error -> Error
    end
).

-define(INTERNAL_METADATA_KEY, <<"__onedata">>).
-define(REJECTED_METADATA_KEY, <<"__rejected">>).
-define(REJECTION_REASON_METADATA_KEY, <<"__rejection_reason">>).

-define(MAX_SUBMIT_RETRIES, 3).


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
    ?REQUEST_RETURN_OK(do_request(get, Endpoint, <<>>, <<>>, <<>>, [200])).


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
    ?REQUEST_RETURN_OK(do_request(put, Endpoint, IndexId, <<>>, NewSchema, [{200,300}])).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback delete_index/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_index(od_harvester:endpoint(), od_harvester:index_id()) -> {ok | {error, term()}}.
delete_index(Endpoint, IndexId) ->
    ?REQUEST_RETURN_OK(do_request(delete, Endpoint, IndexId, <<>>, <<>>, [{200,300}, 404])).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback submit_batch/4.
%% @end
%%--------------------------------------------------------------------
-spec submit_batch(od_harvester:endpoint(), od_harvester:id(), od_harvester:indices(), od_harvester:batch()) -> 
    {ok, [{od_harvester:index_id(), ok | {error, SuccessfulSeq :: pos_integer() | undefined,
        FailedSeq :: pos_integer(), Error :: binary()}}]}.
submit_batch(Endpoint, _HarvesterId, Indices, Batch) ->
    submit_to_all_indices(Endpoint, Indices, Batch).


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
    is_path_allowed(Method, Path) orelse 
        throw(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"path">>, allowed_paths(Method))),
    case do_request(Method, Endpoint, IndexId, <<"/", Path/binary>>, Body) of
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
    od_harvester:index_id(), Path :: binary(), Data :: binary()) -> http_client:response().
do_request(Method, Endpoint, IndexId, Path, Data) ->
    do_request(Method, Endpoint, IndexId, Path, Data, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @equiv do_request(Method, Endpoint, HarvesterId, IndexId, Path, Data, 
%%          #{?HDR_CONTENT_TYPE => <<"application/json">>}, ExpectedCodes).
%% @end
%%--------------------------------------------------------------------
-spec do_request(http_client:method(), od_harvester:endpoint(),
    od_harvester:index_id(), Path :: binary(), Data :: binary(),
    ExpectedCodes :: [integer() | {integer(), integer()}] | undefined) -> http_client:response().
do_request(Method, Endpoint, IndexId, Path, Data, ExpectedCodes) ->
    do_request(Method, Endpoint, IndexId, Path, Data,
        #{?HDR_CONTENT_TYPE => <<"application/json">>}, ExpectedCodes).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes request to elasticsearch.
%% When ExpectedCodes lists is provided returns error if response has wrong status code. 
%% ExpectedCodes list must conform to specification in is_code_expected/2.
%% @end
%%--------------------------------------------------------------------
-spec do_request(http_client:method(), od_harvester:endpoint(), od_harvester:index_id(),
    Path :: binary(), Data :: binary(), http_client:headers(),
    ExpectedCodes :: [integer() | {integer(), integer()}] | undefined) -> http_client:response().
do_request(Method, Endpoint, IndexId, Path, Data, Headers, ExpectedCodes) ->
    Url = <<Endpoint/binary, "/", IndexId/binary, Path/binary>>,
    case http_client:request(Method, Url, Headers, Data, [{recv_timeout, ?REQUEST_TIMEOUT}]) of
        {ok, Code, RespHeaders, Body} = Response when is_list(ExpectedCodes) ->
            case is_code_expected(Code, ExpectedCodes) of
                true -> 
                    Response;
                _ ->
                    ?debug("Elasticsearch plugin: ~p ~p returned unexpected response ~p:~n ~p~n~p",
                        [Method, Url, Code, RespHeaders, json_utils:decode(Body)]),
                    ?ERROR_BAD_DATA(<<"payload">>)
            end;
        {ok,_,_,_} = Response ->
            Response;
        {error, _} = Error ->
            ?error("Elasticsearch plugin: ~p ~p was unsuccessful due to ~w",
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepares elasticsearch bulk request in format:
%% { "index" : { "_id" : FileId1 } }
%% { "field1" : "value1" }
%% { "delete" : {"_id" : FileId2 } }
%% { "index" : { "_id" : FileId1 } }
%% { "field1" : "value1" }
%% Each line ends with literal '\n'
%% @end
%%--------------------------------------------------------------------
-spec prepare_elasticsearch_batch(od_harvester:batch(), {rejected_fields(), binary()}) -> es_batch().
prepare_elasticsearch_batch(Batch, Rejected) ->
    Requests = lists:map(fun(BatchEntry) ->
        #{
            <<"operation">> := Operation,
            <<"fileId">> := EntryId
        } = BatchEntry,
        ESOperation = case Operation of
            <<"submit">> -> <<"index">>;
            <<"delete">> -> <<"delete">>
        end,
        Req = json_utils:encode(#{ESOperation => #{<<"_id">> => EntryId}}),
        case ESOperation of
            <<"index">> ->
                Data = json_utils:encode(prepare_data(BatchEntry, Rejected)),
                str_utils:join_binary([Req, Data], <<"\n">>);
            _ -> Req
        end
    end, Batch),
    <<(str_utils:join_binary(Requests, <<"\n">>))/binary, "\n">>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepares data to be sent to Elasticsearch based on entry sent by provider.
%% SpaceId, FileName and Xattrs are stored under special key ?INTERNAL_METADATA_KEY,
%% which is added to json metadata.
%% @end
%%--------------------------------------------------------------------
-spec prepare_data(od_harvester:batch_entry(), {rejected_fields(), binary()}) -> map().
prepare_data(BatchEntry, {RejectedFields, RejectionReason}) ->
    Payload = maps:get(<<"payload">>, BatchEntry, #{}),
    InternalParams = maps:with([<<"spaceId">>, <<"fileName">>], BatchEntry),
    InternalParams1 = case maps:find(<<"xattrs">>, Payload) of
        {ok, Xattrs} -> InternalParams#{<<"xattrs">> => prepare_xattrs(Xattrs, RejectedFields)};
        _ -> InternalParams
    end,
    EncodedJson = maps:get(<<"json">>, Payload, <<"{}">>),
    DecodedJson = json_utils:decode(EncodedJson),
    InternalParams2 = InternalParams1#{
        <<"json_metadata_exists">> => maps:is_key(<<"json">>, Payload),
        <<"xattr_metadata_exists">> => maps:is_key(<<"xattrs">>, Payload)
    },
    InternalParams3 = case RejectedFields of
        [] -> InternalParams2;
        [_|_] -> InternalParams2#{
            ?REJECTED_METADATA_KEY => RejectedFields,
            ?REJECTION_REASON_METADATA_KEY => RejectionReason
        };
        all -> InternalParams2#{?REJECTION_REASON_METADATA_KEY => RejectionReason}
    end, 
    ResultJson = prepare_json(DecodedJson, RejectedFields),
    ResultJson#{?INTERNAL_METADATA_KEY => InternalParams3}.


-spec prepare_xattrs(#{binary() => binary()}, rejected_fields()) -> converted_xattrs().
prepare_xattrs(Xattrs, all) ->
    #{?REJECTED_METADATA_KEY => json_utils:encode(Xattrs)};
prepare_xattrs(Xattrs, Fields) ->
    ConvertedXattrs = convert_xattrs(Xattrs),
    Re = <<(?INTERNAL_METADATA_KEY)/binary, "\\.xattrs\\.(.*)">>,
    lists:foldl(fun(RejectedField, Acc) ->
        case re:run(RejectedField, Re, [{capture, all_but_first, binary}]) of
            {match, [Field | _]} -> remove_field(split_on_dot(Field), Acc);
            _ -> Acc
        end
    end, ConvertedXattrs, Fields).


prepare_json(Json, all) when map_size(Json) == 0->
    #{};
prepare_json(Json, all) ->
    #{<<(?INTERNAL_METADATA_KEY)/binary, ".", (?REJECTED_METADATA_KEY)/binary>> => json_utils:encode(Json)};
prepare_json(Json, Fields) ->
    lists:foldl(fun(RejectedField, Acc) ->
        remove_field(split_on_dot(RejectedField), Acc)
    end, Json, Fields).


remove_field([H], Map) when is_map(Map) ->
    maps:without([H], Map);
remove_field([H | Tail], Map) when is_map(Map) ->
    case maps:get(H, Map, undefined) of
        undefined -> Map;
        InnerMapOrList -> Map#{H => remove_field(Tail, InnerMapOrList)}
    end;
remove_field(Key, List) when is_list(List) ->
    lists:map(fun(InnerMapOrList) ->
        remove_field(Key, InnerMapOrList)
    end, List).


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
        ErrorMap -> 
            ErrorType = maps:get(<<"type">>, ErrorMap, <<"unexpected_error">>),
            ErrorReason = maps:get(<<"reason">>, ErrorMap, ErrorType), 
            {ErrorType, ErrorReason, ErrorMap}
    end.


%% @private
-spec is_path_allowed(http_client:method(), binary()) -> boolean().
is_path_allowed(Method, Path) ->
    AllowedPaths = allowed_paths(Method),
    lists:any(fun(AllowedPathRe) ->
        match == re:run(Path, AllowedPathRe, [{capture, none}])
    end, AllowedPaths).


%% @private
-spec is_es_schema_error(Error :: binary()) -> boolean().
is_es_schema_error(<<"mapper_parsing_exception">>) -> true;
is_es_schema_error(<<"strict_dynamic_mapping_exception">>) -> true;
is_es_schema_error(<<"illegal_argument_exception">>) -> true;
is_es_schema_error(_) -> false.


%% @private
-spec allowed_paths(http_client:method()) -> [binary()].
allowed_paths(post) -> [<<"^_search.*$">>];
allowed_paths(get) -> [<<"^_mapping$">>, <<"^_search.*$">>].


%% @private
-spec submit_to_all_indices(od_harvester:endpoint(), od_harvester:indices(), od_harvester:batch()) ->
    {ok, [{od_harvester:index_id(), index_submit_response()}]}.
submit_to_all_indices(Endpoint, Indices, Batch) ->
    PreparedBatch = prepare_elasticsearch_batch(Batch, {[], <<>>}),
    FirstSeq = maps:get(<<"seq">>, lists:nth(1, Batch)),
    {ok, lists_utils:pmap(fun(IndexId) ->
        {IndexId, submit_to_index(Endpoint, IndexId, Batch, PreparedBatch, [], FirstSeq)}
    end, Indices)}.


%% @private
-spec submit_to_index(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:batch(), 
    es_batch(), rejected_fields(), FirstSeq :: pos_integer()) -> index_submit_response().
submit_to_index(Endpoint, IndexId, Batch, PreparedBatch, Fields, FirstSeq) ->
    case do_submit_request(Endpoint, IndexId, PreparedBatch) of
        {ok, Res} -> check_result(Endpoint, IndexId, Batch, Fields, Res);
        {error, ErrorMsg} -> {error, undefined, FirstSeq, ErrorMsg}
    end.


%% @private
-spec check_result(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:batch(), 
    rejected_fields(), map()) -> index_submit_response().
check_result(Endpoint, IndexId, Batch, RejectedFields, Result) ->
    case maps:get(<<"errors">>, Result) of
        false -> ok;
        true ->
            case parse_batch_result(Result, Batch) of
                {rejected, Field, Reason} ->
                    case is_binary(Field) andalso length(RejectedFields) + 1  < ?MAX_SUBMIT_RETRIES of
                        true -> resubmit(Endpoint, IndexId, Batch, [Field | RejectedFields], Reason);
                        false -> resubmit(Endpoint, IndexId, Batch, all, Reason)
                    end;
                Other -> Other
            end
    end.


%% @private
-spec resubmit(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:batch(), 
    rejected_fields(), binary()) -> index_submit_response().
resubmit(Endpoint, IndexId, Batch, Fields, Reason) ->
    PreparedBatch = prepare_elasticsearch_batch(Batch, {Fields, Reason}),
    FirstSeq = maps:get(<<"seq">>, lists:nth(1, Batch)),
    submit_to_index(Endpoint, IndexId, Batch, PreparedBatch, Fields, FirstSeq).


%% @private
do_submit_request(Endpoint, IndexId, PreparedBatch) ->
    case do_request(post, Endpoint, IndexId, ?ENTRY_PATH(<<"_bulk">>), PreparedBatch,
        #{?HDR_CONTENT_TYPE => <<"application/x-ndjson">>}, [{200,300}]) of
        {ok,_,_,Body} -> {ok, json_utils:decode(Body)};
        {error, _} = Error ->
            ErrorMsg = case Error of
                ?ERROR_TEMPORARY_FAILURE ->
                    <<"Temporary failure: Elasticsearch service is currently unavailable">>;
                ?ERROR_BAD_DATA(<<"payload">>) ->
                    <<"Provided payload cannot be understood by the server">>
            end,
            {error, ErrorMsg}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses elasticsearch bulk result in format:
%%  "items": [
%%    {
%%      "index": {
%%        ...
%%      }
%%    },
%%    {
%%      "delete": {
%%        ...
%%      }
%%    },
%%    {
%%      "index": {
%%        ...
%%        "error": {
%%          ...
%%          "type": "mapper_parsing_exception",
%%          ...
%%        }
%%      }
%%    }
%%  ]
%% @end
%%--------------------------------------------------------------------
-spec parse_batch_result(Res :: map(), od_harvester:batch()) -> 
    index_submit_response() | {rejected, binary(), binary()}.
parse_batch_result(Result, Batch) ->
    ParsedResult = lists:foldl(
        fun({EntryResponse, BatchEntry}, {PrevSeq, undefined}) ->
            Seq = maps:get(<<"seq">>, BatchEntry),
            case get_entry_response_error(EntryResponse) of
                undefined -> {Seq, undefined};
                {ErrorType, ErrorReason, _Error} ->
                    case is_es_schema_error(ErrorType) of
                        true -> {rejected, retrieve_rejected_field(ErrorReason), ErrorReason};
                        false -> {PrevSeq, {Seq, <<ErrorType/binary, ": ", ErrorReason/binary>>}}
                    end
            end;
            (_, Acc) -> Acc  % ignore rest of response when first error is found 
        end, {undefined, undefined}, lists:zip(maps:get(<<"items">>, Result), Batch)
    ),
    case ParsedResult of
        {_, undefined} -> ok;
        {SuccessfulSeq, {FailedSeq, Error}} -> {error, SuccessfulSeq, FailedSeq, Error};
        Other -> Other
    end.


%% @private
-spec retrieve_rejected_field(binary()) -> binary() | all.
retrieve_rejected_field(ErrorReason) ->
    ?debug("[Elasticsearch plugin]: ~p", [ErrorReason]),
    ReToRun = [
        <<"Existing mapping for \\[(.*)\\] must">>,
        <<"failed to parse field \\[(.*)\\] of type">>,
        <<"object mapping for \\[(.*)\\] tried to parse">>,
        <<"mapper \\[(.*)\\] of different type">>
    ],
    Res = lists:filtermap(fun(Re) -> 
        case re:run(ErrorReason, Re, [{capture, all_but_first, binary}]) of
            {match, [Field | _]} -> {true, Field};
            _ -> false
        end
    end, ReToRun),
    case Res of
        [] -> all;
        [Field | _] -> Field
    end.


%% @TODO VFS-6546 preserve longest conflicting xattr
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts each xattr with '.' in key to nested maps, e.g:
%% #{<<"aaa.bbb.ccc.ddd">> => <<"123">>} will be converted to 
%% #{<<"aaa">> => #{<<"bbb">> => #{<<"ccc">> => #{<<"ddd">> => <<"123">>}}}} 
%%
%% In case of conflicting keys (e.g <<"aaa">>, <<"aaa.bbb">>) fixme
%% Rejected keys are stored under ?REJECTED_XAATRS_KEY key.
%% @end
%%--------------------------------------------------------------------
-spec convert_xattrs(#{binary() => binary()}) -> converted_xattrs().
convert_xattrs(Xattrs) ->
    maps:fold(fun convert_xattr/3, #{}, Xattrs).


%% @private
-spec convert_xattr(binary(), binary(), converted_xattrs()) -> converted_xattrs().
convert_xattr(Key, Value, Map) ->
    KeyList = split_on_dot(Key),
    
    try put_xattr_value(KeyList ++ [<<"__value">>], Value, Map) catch
        throw:rejected ->
            RejectedList = maps:get(?REJECTED_METADATA_KEY, Map, []),
            Map#{?REJECTED_METADATA_KEY => [Key | RejectedList]}
    end.


%% @private
-spec put_xattr_value([binary()], binary(), converted_xattrs()) -> converted_xattrs().
put_xattr_value([], Value, _Map) ->
    Value;
put_xattr_value([H | Tail], Value, Map) ->
    Nested = maps:get(H, Map, #{}),
    case is_map(Nested) of
        true -> Map#{H => put_xattr_value(Tail, Value, Nested)};
        false -> throw(rejected)
    end.


%% @private
-spec split_on_dot(binary()) -> [binary()].
split_on_dot(Binary) ->
    lists:filter(fun(A) ->
        byte_size(A) > 0
    end, binary:split(Binary, <<".">>, [global])).
