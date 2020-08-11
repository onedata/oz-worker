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

-include("datastore/oz_datastore_models.hrl").
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

-ifdef(TEST).
-compile(export_all).
-endif.

-behaviour(onezone_plugin_behaviour).
-behaviour(harvester_plugin_behaviour).

-type converted_xattrs() :: #{binary() => converted_xattrs() | binary()}.

%% ES batch is a binary in a following format:
%% { "index" : { "_id" : FileId1 } }
%% { "field1" : "value1" }
%% { "delete" : {"_id" : FileId2 } }
%% { "index" : { "_id" : FileId1 } }
%% { "field1" : "value1" }
%% Each line ends with literal '\n'
-type es_batch() :: binary().

-type rejected_fields() :: all | [binary()].
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
submit_batch(Endpoint, HarvesterId, Indices, Batch) ->
    try
        {ok, lists_utils:pmap(fun({IndexId, IndexInfo}) ->
            {IndexId, submit_to_index(Endpoint, IndexId, IndexInfo, Batch)}
        end, maps:to_list(Indices))}
    catch error:{parallel_call_failed, {failed_processes, Errors}} ->
        ?error("[Elasticsearch plugin] Submit batch in harvester ~p failed due to: ~p", 
            [HarvesterId, Errors]),
        throw(?ERROR_TEMPORARY_FAILURE)
    end.


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
%%% Functions regarding communication with Elasticsearch
%%%===================================================================

%% @private
%% @doc
%% @equiv submit_to_index(Endpoint, IndexId, IndexInfo, Batch, {[], <<>>}).
%% @end
submit_to_index(Endpoint, IndexId, IndexInfo, Batch) ->
    submit_to_index(Endpoint, IndexId, IndexInfo, Batch, {[], <<>>}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to submit given PreparedBatch to given Index in Elasticsearch. 
%% If batch was rejected because of not matching index schema, invalid 
%% field(provided in ES response) is removed and batch is resubmitted. 
%% This process is repeated up to ?MAX_SUBMIT_RETRIES times after which 
%% whole data is sent as string under ?REJECTED_METADATA_KEY key and 
%% rejection reason is submitted under ?REJECTION_REASON_METADATA_KEY.
%% @end
%%--------------------------------------------------------------------
-spec submit_to_index(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:index(), 
    od_harvester:batch(), {rejected_fields(), binary()}) -> index_submit_response().
submit_to_index(Endpoint, IndexId, IndexInfo, Batch, {RejectedFields, _Reason} = Rejection) ->
    PreparedBatch = prepare_elasticsearch_batch(Batch, IndexInfo, Rejection),
    ?notice("~p", [PreparedBatch]),
    FirstSeq = maps:get(<<"seq">>, lists:nth(1, Batch)),
    case do_submit_request(Endpoint, IndexId, PreparedBatch) of
        {ok, Res} -> check_result(Endpoint, IndexId, IndexInfo, Batch, RejectedFields, Res);
        {error, ErrorMsg} -> {error, undefined, FirstSeq, ErrorMsg}
    end.


%% @private
-spec check_result(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:index(), 
    od_harvester:batch(), rejected_fields(), map()) -> index_submit_response().
check_result(Endpoint, IndexId, IndexInfo, Batch, RejectedFields, Result) ->
    case maps:get(<<"errors">>, Result, false) of
        false -> ok;
        true ->
            case parse_batch_result(Result, Batch, IndexInfo#harvester_index.retry_on_rejection) of
                {rejected, Field, Reason} ->
                    case is_binary(Field) andalso length(RejectedFields) + 1  < ?MAX_SUBMIT_RETRIES of
                        true -> 
                            submit_to_index(Endpoint, IndexId, IndexInfo, Batch, 
                                {[Field | RejectedFields], Reason});
                        false -> 
                            submit_to_index(Endpoint, IndexId, IndexInfo, Batch, {all, Reason})
                    end;
                Other -> Other
            end
    end.


%% @private
% fixme spec
do_submit_request(_Endpoint, _IndexId, empty) ->
    {ok, #{}};
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
                    ?error("Elasticsearch plugin: ~p ~p returned unexpected response ~p:~n ~p~n~p",
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


%%%===================================================================
%%% Functions regarding creating payload to send to Elasticsearch
%%%===================================================================

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
-spec prepare_elasticsearch_batch(od_harvester:batch(), od_harvester:index(), 
    {rejected_fields(), binary()}) -> es_batch() | empty.
prepare_elasticsearch_batch(Batch, IndexInfo, Rejected) ->
    Requests = lists:filtermap(fun(BatchEntry) ->
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
                Data = prepare_data(BatchEntry, IndexInfo, Rejected),
                case maps:size(Data) == 0 of
                    true -> 
                        false;
                    false ->
                        EncodedData = json_utils:encode(Data),
                        {true, str_utils:join_binary([Req, EncodedData], <<"\n">>)}
                end;
            _ -> {true, Req}
        end
    end, Batch),
    case Requests of
        [] -> empty;
        _ -> <<(str_utils:join_binary(Requests, <<"\n">>))/binary, "\n">>
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepares data to be sent to Elasticsearch based on entry sent by provider.
%% SpaceId, FileName and Xattrs are stored under special key ?INTERNAL_METADATA_KEY,
%% which is added to json metadata.
%% @end
%%--------------------------------------------------------------------
-spec prepare_data(od_harvester:batch_entry(), od_harvester:index(), 
    {rejected_fields(), binary()}) -> map().
prepare_data(BatchEntry, IndexInfo, {RejectedFields, RejectionReason}) ->
    MetadataTypesToInclude = IndexInfo#harvester_index.include_metadata,
    FileDetailsToInclude = IndexInfo#harvester_index.include_file_details,
    
    InternalParams = maps:with(FileDetailsToInclude, BatchEntry),
    Payload = maps:with(MetadataTypesToInclude, maps:get(<<"payload">>, BatchEntry, #{})),
    EncodedJson = maps:get(<<"json">>, Payload, <<"{}">>),
    DecodedJson = json_utils:decode(EncodedJson),
    InternalParams1 = lists:foldl(fun(MetadataType, Acc) ->
        add_to_internal_params(
            MetadataType, 
            lists:member(<<"metadataExistenceFlags">>, FileDetailsToInclude), 
            Payload, RejectedFields, Acc)
    end, InternalParams, MetadataTypesToInclude),
    InternalParams2 = maybe_add_rejection(InternalParams1, RejectedFields, RejectionReason, 
        IndexInfo#harvester_index.include_rejection_reason),
    ResultJson = prepare_json(DecodedJson, RejectedFields),
    case maps:size(InternalParams2) == 0 of
        true -> ResultJson;
        false -> ResultJson#{?INTERNAL_METADATA_KEY => InternalParams2}
    end.


% fixme specs docs and refactor
add_to_internal_params(<<"json">> = MetadataType, MetadataExistenceFlag, Payload, _RejectedFields, InternalParams) ->
    maybe_add_existence_flag(MetadataType, maps:is_key(<<"json">>, Payload), MetadataExistenceFlag, InternalParams);
add_to_internal_params(MetadataType, MetadataExistenceFlag, Payload, RejectedFields, InternalParams) ->
    case maps:find(MetadataType, Payload) of
        {ok, Metadata} -> 
            maybe_add_existence_flag(MetadataType, true, MetadataExistenceFlag, 
                InternalParams#{MetadataType => prepare_metadata(MetadataType, Metadata, RejectedFields)});
        _ -> 
            maybe_add_existence_flag(MetadataType, false, MetadataExistenceFlag, InternalParams)
    end.


maybe_add_existence_flag(_MetadataType, _Exists, false, InternalParams) ->
    InternalParams;
maybe_add_existence_flag(MetadataType, Exists, true, InternalParams) ->
    InternalParams#{<<MetadataType/binary, "_metadata_exists">> => Exists}.


prepare_metadata(<<"xattrs">>, Xattrs, RejectedFields) ->
    prepare_xattrs(Xattrs, RejectedFields);
prepare_metadata(<<"rdf">>, Rdf, _RejectedFields) ->
    Rdf.


maybe_add_rejection(InternalParams, [], _RejectionReason, _) ->
    InternalParams;
maybe_add_rejection(InternalParams, RejectedFields, _RejectionReason, false) ->
    InternalParams#{?REJECTED_METADATA_KEY => RejectedFields};
maybe_add_rejection(InternalParams, RejectedFields, RejectionReason, true) ->
    InternalParams#{
        ?REJECTED_METADATA_KEY => RejectedFields,
        ?REJECTION_REASON_METADATA_KEY => RejectionReason
    }.


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


-spec prepare_json(json_utils:json_map(), rejected_fields()) -> json_utils:json_map().
prepare_json(Json, all) when map_size(Json) == 0->
    #{};
prepare_json(Json, all) ->
    #{<<(?INTERNAL_METADATA_KEY)/binary, ".", (?REJECTED_METADATA_KEY)/binary>> => json_utils:encode(Json)};
prepare_json(Json, Fields) ->
    lists:foldl(fun(RejectedField, Acc) ->
        remove_field(split_on_dot(RejectedField), Acc)
    end, Json, Fields).


-spec remove_field([binary()], json_utils:json_term()) -> json_utils:json_term().
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts each xattr with '.' in key to nested maps and stores value 
%% under special key: <<"__value">>.
%% For example #{<<"aaa.bbb.ccc">> => <<"123">>} will be converted to 
%% #{<<"aaa">> => #{<<"bbb">> => #{<<"ccc">> => #{<<"__value">> => <<"123">>]}}} 
%%
%% In case of conflicting keys (e.g <<"aaa">>, <<"aaa.__value">>) only 
%% the most nested one will be preserved, rest will be rejected.
%% Rejected keys are stored under ?REJECTED_XAATRS_KEY key.
%% @end
%%--------------------------------------------------------------------
-spec convert_xattrs(#{binary() => binary()}) -> converted_xattrs().
convert_xattrs(Xattrs) ->
    AsList = lists:map(fun({K, V}) -> {split_on_dot(K), K, V} end, maps:to_list(Xattrs)),
    Sorted = lists:sort(fun({K1, _, _}, {K2, _, _}) -> length(K1) > length(K2) end, AsList),
    lists:foldl(fun({KeyList, OriginalKey, Value}, Map) ->
        ExtendedKeyList = KeyList ++ [<<"__value">>],
        
        try put_xattr_value(ExtendedKeyList, Value, Map) catch
            throw:rejected ->
                RejectedList = maps:get(?REJECTED_METADATA_KEY, Map, []),
                Map#{?REJECTED_METADATA_KEY => [OriginalKey | RejectedList]}
        end
    end, #{}, Sorted).


%% @private
-spec put_xattr_value([binary()], binary(), converted_xattrs()) -> converted_xattrs().
put_xattr_value([Key], Value, Map) ->
    maps:is_key(Key, Map) andalso throw(rejected),
    Map#{Key => Value};
put_xattr_value([H | Tail], Value, Map) ->
    Nested = maps:get(H, Map, #{}),
    Map#{H => put_xattr_value(Tail, Value, Nested)}.


%% @private
-spec split_on_dot(binary()) -> [binary()].
split_on_dot(Binary) ->
    lists:filter(fun(A) ->
        byte_size(A) > 0
    end, binary:split(Binary, <<".">>, [global])).


%%%===================================================================
%%% Functions regarding parsing reply from Elasticsearch
%%%===================================================================

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
%%          "reason": "mapper [a.b] of different type"
%%          ...
%%        }
%%      }
%%    }
%%  ]
%% @end
%%--------------------------------------------------------------------
-spec parse_batch_result(Res :: map(), od_harvester:batch(), boolean()) ->
    index_submit_response() | {rejected, binary(), binary()}.
parse_batch_result(Result, Batch, RetryOnRejection) ->
    ParsedResult = lists:foldl(
        fun({EntryResponse, BatchEntry}, {PrevSeq, undefined}) ->
            Seq = maps:get(<<"seq">>, BatchEntry),
            case get_entry_response_error(EntryResponse) of
                undefined -> {Seq, undefined};
                {ErrorType, ErrorReason} ->
                    case {is_es_schema_error(ErrorType), RetryOnRejection} of
                        {true, true} -> {rejected, retrieve_rejected_field(ErrorReason), ErrorReason};
                        {true, false} -> 
                            ?debug("[Elasticsearch plugin] Entry submit dropped due to not matching schema: ~p", [EntryResponse]),
                            {Seq, undefined};
                        {false, _} -> {PrevSeq, {Seq, <<ErrorType/binary, ": ", ErrorReason/binary>>}}
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
            {ErrorType, ErrorReason}
    end.


%% @private
-spec is_es_schema_error(Error :: binary()) -> boolean().
is_es_schema_error(<<"mapper_parsing_exception">>) -> true;
is_es_schema_error(<<"strict_dynamic_mapping_exception">>) -> true;
is_es_schema_error(<<"illegal_argument_exception">>) -> true;
is_es_schema_error(_) -> false.


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
        [] -> all; % reject all fields when error is not recognized
        [Field | _] -> Field
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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
-spec is_path_allowed(http_client:method(), binary()) -> boolean().
is_path_allowed(Method, Path) ->
    AllowedPaths = allowed_paths(Method),
    lists:any(fun(AllowedPathRe) ->
        match == re:run(Path, AllowedPathRe, [{capture, none}])
    end, AllowedPaths).


%% @private
-spec allowed_paths(http_client:method()) -> [binary()].
allowed_paths(post) -> [<<"^_search.*$">>];
allowed_paths(get) -> [<<"^_mapping$">>, <<"^_search.*$">>].
