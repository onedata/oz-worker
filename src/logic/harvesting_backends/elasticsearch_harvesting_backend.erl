%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements harvesting_backend_behaviour and is called 
%%% by harvester_logic_plugin to handle operations on Elasticsearch.
%%% This plugin is compatible with Elasticsearch 6.x and 7.x (up to 7.1).
%%% @end
%%%-------------------------------------------------------------------
-module(elasticsearch_harvesting_backend).
-author("Michal Stanisz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/headers.hrl").

-export([
    get_name/0,
    ping/1,
    create_index/4, delete_index/2,
    submit_batch/4,
    query_index/3,
    query_validator/0
]).

% for tests
-export([do_submit_request/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-behaviour(harvesting_backend_behaviour).

-type converted_xattrs() :: #{binary() => converted_xattrs() | binary()}.

%% ES batch, if not empty, is a binary in a following format:
%% { "index" : { "_id" : FileId1 } }
%% { "field1" : "value1" }
%% { "delete" : {"_id" : FileId2 } }
%% { "index" : { "_id" : FileId1 } }
%% { "field1" : "value1" }
%% Each line ends with literal '\n'
-type es_batch() :: binary() | empty.

-type rejected_fields() :: all | [binary()].
-type rejection_info() :: {rejected_fields(), binary()}.

% Internal params is a map that contains subset of following keys (as binaries): 
%   - xattrs => converted_xattrs() 
%   - rdf => binary()
%   - {metadata_type} + ?METADATA_EXISTENCE_FLAG_SUFFIX (e.g json_metadata_exists) => boolean() 
%   - specified in `include_file_details` in IndexInfo (e.g spaceId, fileName) => binary()
%   - ?REJECTED_METADATA_KEY => rejected_fields() 
%   - ?REJECTION_REASON_METADATA_KEY => binary()
-type internal_params() :: #{binary() => binary() | rejected_fields() | boolean() | converted_xattrs()}.


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
-define(METADATA_EXISTENCE_FLAG_SUFFIX, <<"_metadata_exists">>).

-define(MAX_SUBMIT_RETRIES, 3).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvesting_backend_behaviour} callback get_name/0
%% @end
%%--------------------------------------------------------------------
-spec get_name() -> binary().
get_name() ->
    <<"Elasticsearch plugin">>.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvesting_backend_behaviour} callback ping/1
%% @end
%%--------------------------------------------------------------------
-spec ping(od_harvester:endpoint()) -> ok | {error, term()}.
ping(Endpoint) ->
    ?REQUEST_RETURN_OK(do_request(get, Endpoint, <<>>, <<>>, <<>>, [200])).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvesting_backend_behaviour} callback create_index/4.
%% @end
%%--------------------------------------------------------------------
-spec create_index(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:index(), 
    od_harvester:schema()) -> ok | {error, term()}.
create_index(Endpoint, IndexId, IndexInfo, Schema) ->
    NewSchema = case Schema of
        undefined -> <<"{}">>;
        Schema -> Schema
    end,
    ExtendedSchema = extend_schema(IndexInfo, json_utils:decode(NewSchema)),
    ?REQUEST_RETURN_OK(do_request(
        put, Endpoint, IndexId, <<>>, json_utils:encode(ExtendedSchema), [{200,300}])).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvesting_backend_behaviour} callback delete_index/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_index(od_harvester:endpoint(), od_harvester:index_id()) -> ok | {error, term()}.
delete_index(Endpoint, IndexId) ->
    ?REQUEST_RETURN_OK(do_request(delete, Endpoint, IndexId, <<>>, <<>>, [{200,300}, 404])).


%%--------------------------------------------------------------------
%% @doc
%% {@link harvesting_backend_behaviour} callback submit_batch/4.
%% @end
%%--------------------------------------------------------------------
-spec submit_batch(od_harvester:endpoint(), od_harvester:id(), od_harvester:indices(), od_harvester:batch()) ->
    {ok, [{od_harvester:index_id(), od_harvester:index_submit_response()}]}.
submit_batch(Endpoint, HarvesterId, Indices, Batch) ->
    try
        {ok, lists_utils:pmap(fun({IndexId, IndexInfo}) ->
            {IndexId, submit_to_index(Endpoint, IndexId, IndexInfo, Batch)}
        end, maps:to_list(Indices))}
    catch error:{parallel_call_failed, {failed_processes, Errors}} ->
        ?error_stacktrace("[Elasticsearch plugin] Submit batch in harvester ~p failed due to: ~p",
            [HarvesterId, Errors]),
        throw(?ERROR_TEMPORARY_FAILURE)
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvesting_backend_behaviour} callback query/3.
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
%% {@link harvesting_backend_behaviour} callback query_validator/0.
%% @end
%%--------------------------------------------------------------------
-spec query_validator() -> entity_logic:validity_verificator().
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
%% 
%% If batch was rejected because of not matching index schema and index 
%% have `retryOnRejection` set to true invalid field (retrieved from ES 
%% response) is removed and batch is resubmitted. 
%% This process is repeated up to ?MAX_SUBMIT_RETRIES times after which 
%% whole data is sent as string under ?REJECTED_METADATA_KEY key.
%% 
%% If any data was rejected and index have `includeRejectionReason` set 
%% to true rejection reason is submitted under ?REJECTION_REASON_METADATA_KEY.
%% @end
%%--------------------------------------------------------------------
-spec submit_to_index(od_harvester:endpoint(), od_harvester:index_id(), od_harvester:index(),
    od_harvester:batch(), rejection_info()) -> od_harvester:index_submit_response().
submit_to_index(Endpoint, IndexId, IndexInfo, Batch, {RejectedFields, _Reason} = RejectionInfo) ->
    {PreparedEsBatch, TrimmedBatch} = prepare_elasticsearch_batch(Batch, IndexInfo, RejectionInfo),
    % call using ?MODULE for mocking in tests
    case ?MODULE:do_submit_request(Endpoint, IndexId, PreparedEsBatch) of
        {ok, Res} ->
            IgnoreSchemaErrors = 
                (not IndexInfo#harvester_index.retry_on_rejection) or (RejectedFields == all),
            case check_result(Res, IgnoreSchemaErrors, RejectedFields) of
                {retry, NewRejectionInfo} ->
                    submit_to_index(Endpoint, IndexId, IndexInfo, TrimmedBatch, NewRejectionInfo);
                {error, SuccessfulEntryNum, FailedEntryNum, ErrorMsg} ->
                    {error, map_entry_num_to_seq(SuccessfulEntryNum, TrimmedBatch),
                        map_entry_num_to_seq(FailedEntryNum, TrimmedBatch), ErrorMsg};
                Other ->
                    Other
            end;
        {error, ErrorMsg} -> {error, undefined, map_entry_num_to_seq(1, TrimmedBatch), ErrorMsg}
    end.


%% @private
-spec map_entry_num_to_seq(pos_integer() | undefined, od_harvester:batch()) -> 
    pos_integer() | undefined.
map_entry_num_to_seq(undefined, _Batch) ->
    undefined;
map_entry_num_to_seq(EntryNum, Batch) ->
    maps:get(<<"seq">>, lists:nth(EntryNum, Batch)).


%% @private
-spec check_result(map(), boolean(), rejected_fields()) -> 
    od_harvester:index_submit_response() | {retry, rejection_info()}.
check_result(Result, IgnoreSchemaErrors, RejectedFields) ->
    case maps:get(<<"errors">>, Result, false) of
        false -> ok;
        true ->
            case parse_batch_result(Result, IgnoreSchemaErrors) of
                {rejected, Field, Reason} ->
                    case is_binary(Field) andalso length(RejectedFields) + 1  < ?MAX_SUBMIT_RETRIES of
                        true -> {retry, {[Field | RejectedFields], Reason}};
                        false -> {retry, {all, Reason}}
                    end;
                Other -> Other
            end
    end.


%% @private
-spec do_submit_request(od_harvester:endpoint(), od_harvester:index_id(), es_batch()) ->
    {ok, Result :: map()} | {error, ErrorMsg :: binary()}.
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
%% 
%% If any batch entry would be ignored because of options selected 
%% in IndexInfo it is removed from original batch. Processed batch 
%% is returned from this function.
%% @end
%%--------------------------------------------------------------------
-spec prepare_elasticsearch_batch(od_harvester:batch(), od_harvester:index(),
    rejection_info()) -> {es_batch(), od_harvester:batch()}.
prepare_elasticsearch_batch(Batch, IndexInfo, RejectionInfo) ->
    Res = lists:filtermap(fun(BatchEntry) ->
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
                Data = prepare_data(BatchEntry, IndexInfo, RejectionInfo),
                case maps:size(Data) == 0 of
                    true ->
                        false;
                    false ->
                        EncodedData = json_utils:encode(Data),
                        {true, {str_utils:join_binary([Req, EncodedData], <<"\n">>), BatchEntry}}
                end;
            _ -> {true, {Req, BatchEntry}}
        end
    end, Batch),
    case lists:unzip(Res) of
        {[], []} -> {empty, []};
        {Requests, Entries} -> {<<(str_utils:join_binary(Requests, <<"\n">>))/binary, "\n">>, Entries}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepares data to be sent to Elasticsearch based on entry sent by provider.
%%
%% Based on options set in IndexInfo, additional data (e.g spaceId, xattrs or RejectionInfo) 
%% will be stored under special key ?INTERNAL_METADATA_KEY, which is added to json metadata.
%% @end
%%--------------------------------------------------------------------
-spec prepare_data(od_harvester:batch_entry(), od_harvester:index(), rejection_info()) -> map().
prepare_data(BatchEntry, IndexInfo, {RejectedFields, RejectionReason}) ->
    MetadataTypesToInclude = IndexInfo#harvester_index.include_metadata,
    FileDetailsToInclude = IndexInfo#harvester_index.include_file_details,
    
    InternalParams = maps:with(FileDetailsToInclude, BatchEntry),
    Payload = maps:with(MetadataTypesToInclude, maps:get(<<"payload">>, BatchEntry, #{})),
    EncodedJson = maps:get(<<"json">>, Payload, <<"{}">>),
    DecodedJson = json_utils:decode(EncodedJson),
    ?notice("MetadataTypesToInclude: ~p", [MetadataTypesToInclude]),
    InternalParams1 = lists:foldl(fun(MetadataType, PartialInternalParams) ->
        add_to_internal_params(
            MetadataType,
            lists:member(<<"metadataExistenceFlags">>, FileDetailsToInclude),
            Payload, RejectedFields, PartialInternalParams)
    end, InternalParams, MetadataTypesToInclude),
    InternalParams2 = maybe_add_rejection_info(InternalParams1, RejectedFields, RejectionReason,
        IndexInfo#harvester_index.include_rejection_reason),
    ResultJson = prepare_json(DecodedJson, RejectedFields),
    case maps:size(InternalParams2) == 0 of
        true -> ResultJson;
        false -> ResultJson#{?INTERNAL_METADATA_KEY => InternalParams2}
    end.


%% @private
-spec add_to_internal_params(od_harvester:metadata_type(), AddExistenceFlags :: boolean(), 
    od_harvester:payload(), rejected_fields(), internal_params()) -> internal_params().
add_to_internal_params(<<"json">>, AddExistenceFlags, Payload, _RejectedFields, InternalParams) ->
    Exists = maps:is_key(<<"json">>, Payload),
    maybe_add_existence_flag(<<"json">>, Exists, AddExistenceFlags, InternalParams);
add_to_internal_params(MetadataType, MetadataExistenceFlag, Payload, RejectedFields, InternalParams) ->
    case maps:find(MetadataType, Payload) of
        {ok, Metadata} ->
            maybe_add_existence_flag(MetadataType, true, MetadataExistenceFlag,
                InternalParams#{
                    MetadataType => prepare_internal_metadata(MetadataType, Metadata, RejectedFields)
                });
        _ ->
            maybe_add_existence_flag(MetadataType, false, MetadataExistenceFlag, InternalParams)
    end.


%% @private
-spec maybe_add_existence_flag(od_harvester:metadata_type(), Exists :: boolean(), 
    AddExistenceFlags :: boolean(), internal_params()) -> internal_params().
maybe_add_existence_flag(_MetadataType, _Exists, false, InternalParams) ->
    InternalParams;
maybe_add_existence_flag(MetadataType, Exists, true, InternalParams) ->
    InternalParams#{<<MetadataType/binary, (?METADATA_EXISTENCE_FLAG_SUFFIX)/binary>> => Exists}.


%% @private
-spec prepare_internal_metadata(od_harvester:metadata_type(), binary() | json_utils:json_map(), 
    rejected_fields()) -> binary() | converted_xattrs().
prepare_internal_metadata(<<"xattrs">>, Xattrs, RejectedFields) ->
    prepare_xattrs(Xattrs, RejectedFields);
prepare_internal_metadata(<<"rdf">>, Rdf, _RejectedFields) ->
    json_utils:decode(Rdf).


%% @private
-spec maybe_add_rejection_info(internal_params(), rejected_fields(), RejectionReason :: binary(), 
    IncludeRejectionReason :: boolean()) -> internal_params().
maybe_add_rejection_info(InternalParams, [], _RejectionReason, _) ->
    InternalParams;
maybe_add_rejection_info(InternalParams, all, _RejectionReason, false) ->
    InternalParams;
maybe_add_rejection_info(InternalParams, RejectedFields, _RejectionReason, false) ->
    InternalParams#{?REJECTED_METADATA_KEY => RejectedFields};
maybe_add_rejection_info(InternalParams, all, RejectionReason, true) ->
    InternalParams#{
        ?REJECTION_REASON_METADATA_KEY => RejectionReason
    };
maybe_add_rejection_info(InternalParams, RejectedFields, RejectionReason, true) ->
    InternalParams#{
        ?REJECTED_METADATA_KEY => RejectedFields,
        ?REJECTION_REASON_METADATA_KEY => RejectionReason
    }.


%% @private
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


%% @private
-spec prepare_json(json_utils:json_map(), rejected_fields()) -> json_utils:json_map().
prepare_json(Json, all) when map_size(Json) == 0->
    #{};
prepare_json(Json, all) ->
    % for Elasticsearch this is equivalent to a following nested object 
    % ?INTERNAL_METADATA_KEY: {
    %   ?REJECTED_METADATA_KEY: Json encoded as string
    % }
    Key = <<(?INTERNAL_METADATA_KEY)/binary, ".", (?REJECTED_METADATA_KEY)/binary>>,
    #{Key => json_utils:encode(Json)};
prepare_json(Json, Fields) ->
    lists:foldl(fun(RejectedField, Acc) ->
        remove_field(split_on_dot(RejectedField), Acc)
    end, Json, Fields).


%% @private
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
-spec parse_batch_result(Result :: map(), boolean()) ->
    od_harvester:index_submit_response() | {rejected, binary(), binary()}.
parse_batch_result(Result, IgnoreSchemaErrors) ->
    Items = maps:get(<<"items">>, Result),
    ParsedResult = lists:foldl(
        fun({EntryResponse, Num}, {PrevNum, undefined}) ->
            case get_entry_response_error(EntryResponse) of
                undefined -> {Num, undefined};
                {ErrorType, ErrorReason} ->
                    case {is_es_schema_error(ErrorType), IgnoreSchemaErrors} of
                        {true, false} -> {rejected, retrieve_rejected_field(ErrorReason), ErrorReason};
                        {true, true} ->
                            ?debug("[Elasticsearch plugin] Entry submit dropped due to not matching schema: ~p",
                                [EntryResponse]),
                            {Num, undefined};
                        {false, _} -> {PrevNum, {Num, <<ErrorType/binary, ": ", ErrorReason/binary>>}}
                    end
            end;
            (_, Acc) -> Acc  % ignore rest of response when first error is found 
        end, {undefined, undefined}, lists:zip(Items, lists:seq(1, length(Items)))
    ),
    case ParsedResult of
        {_, undefined} -> ok;
        {SuccessfulEntryNum, {FailedEntryNum, Error}} -> {error, SuccessfulEntryNum, FailedEntryNum, Error};
        Other -> Other
    end.


%% @private
-spec get_entry_response_error(EntryResponse :: map()) ->
    {ErrorType :: binary(), ErrorReason :: binary()} | undefined.
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
%%% Functions regarding Elasticsearch schema
%%%===================================================================

-spec extend_schema(od_harvester:index(), map()) -> map().
extend_schema(IndexInfo, DecodedSchema) ->
    InternalFieldsSchema = prepare_internal_fields_schema(IndexInfo, #{}),
    kv_utils:put(
        [<<"mappings">>, <<"properties">>, ?INTERNAL_METADATA_KEY, <<"properties">>],
        InternalFieldsSchema,
        DecodedSchema
    ).


-spec prepare_internal_fields_schema(od_harvester:index(), map()) -> map().
prepare_internal_fields_schema(
    #harvester_index{include_file_details = [_ | _] = IncludeFileDetails} = IndexInfo, Map
) ->
    NewMap = case lists:member(<<"metadataExistenceFlags">>, IncludeFileDetails) of
        true ->
            IncludeMetadata = IndexInfo#harvester_index.include_metadata,
            ?notice("Include metadata: ~p", [IncludeMetadata]),
            lists:foldl(fun(MetadataType, Acc) ->
                kv_utils:put([<<MetadataType/binary, (?METADATA_EXISTENCE_FLAG_SUFFIX)/binary>>],
                    get_es_schema_type(boolean), Acc)
            end, Map, IncludeMetadata);
        false -> Map
    end,
    NewMap1 = lists:foldl(fun(FileDetail, Acc) ->
        kv_utils:put([FileDetail], get_es_schema_type(text), Acc)
    end, NewMap, lists:delete(<<"metadataExistenceFlags">>, IncludeFileDetails)),
    prepare_internal_fields_schema(IndexInfo#harvester_index{include_file_details = []}, NewMap1);
prepare_internal_fields_schema(
    #harvester_index{include_metadata = [_ | _] = IncludeMetadata} = IndexInfo, Map
) ->
    NewMap = case lists:member(<<"rdf">>, IncludeMetadata) of
        true -> kv_utils:put([<<"rdf">>], get_es_schema_type(text), Map);
        false -> Map
    end,
    prepare_internal_fields_schema(IndexInfo#harvester_index{include_metadata = []}, NewMap);
prepare_internal_fields_schema(#harvester_index{retry_on_rejection = true} = IndexInfo, Map) ->
    NewMap = kv_utils:put([?REJECTED_METADATA_KEY], get_es_schema_type(text), Map),
    prepare_internal_fields_schema(IndexInfo#harvester_index{retry_on_rejection = false}, NewMap);
prepare_internal_fields_schema(#harvester_index{include_rejection_reason = true} = IndexInfo, Map) ->
    NewMap = kv_utils:put([?REJECTION_REASON_METADATA_KEY], get_es_schema_type(text), Map),
    prepare_internal_fields_schema(IndexInfo#harvester_index{include_rejection_reason = false}, NewMap);
prepare_internal_fields_schema(_, Map) ->
    Map.


-spec get_es_schema_type(text | boolean) -> map().
get_es_schema_type(text) ->
    #{
        <<"type">> => <<"text">>,
        <<"fields">> => #{
            <<"keyword">> => #{
                <<"type">> => <<"keyword">>,
                <<"ignore_above">> => 256
            }
        }
    };
get_es_schema_type(boolean) ->
    #{
        <<"type">> => <<"boolean">>
    }.


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
