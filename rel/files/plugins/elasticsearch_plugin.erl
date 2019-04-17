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
-export([ping/1, create_index/4, delete_index/3, submit_entry/5, delete_entry/4, query_index/4, query_validator/0]).

-behaviour(onezone_plugin_behaviour).
-behaviour(harvester_plugin_behaviour).

-define(ES_INDEX_ID(HarvesterId, Index), 
    string:lowercase(base64url:encode(<<HarvesterId/binary, "#", Index/binary>>))).
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
-spec create_index(od_harvester:endpoint(), od_harvester:id(), od_harvester:index_id(), od_harvester:schema()) -> 
    {ok | {error, term()}}.
create_index(Endpoint, HarvesterId, IndexId, Schema) ->
    NewSchema = case Schema of
        undefined -> <<"{}">>;
        Schema -> Schema
    end,
    case do_request(put, Endpoint, HarvesterId, IndexId, <<>>, NewSchema, [{200,300}]) of
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback delete_index/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_index(od_harvester:endpoint(), od_harvester:id(), od_harvester:index_id()) -> 
    {ok | {error, term()}}.
delete_index(Endpoint, HarvesterId, IndexId) ->
    case do_request(delete, Endpoint, HarvesterId, IndexId, <<>>, <<>>, [{200,300}, 404]) of
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback submit_entry/4.
%% @end
%%--------------------------------------------------------------------
-spec submit_entry(od_harvester:endpoint(), od_harvester:id(),  od_harvester:index_id(),
    od_harvester:entry_id(), Data :: binary()) -> ok | {error, term()}.
submit_entry(Endpoint, HarvesterId, IndexId, Id, Data) ->
    case do_request(put, Endpoint, HarvesterId, IndexId, ?ENTRY_PATH(Id), Data, [{200,300}, 400]) of
        {ok, 400,_, Body} ->
            try
                #{<<"error">> := #{<<"type">> := ErrorType}} = json_utils:decode(Body),
                case ErrorType of
                    <<"mapper_parsing_exception">> -> 
                        ok;
                    <<"strict_dynamic_mapping_exception">> -> 
                        ok;
                    Error ->
                        ?debug("Unexpected error in harvester ~p when submiting entry for index ~p: ~p", 
                            [HarvesterId, IndexId, Error]),
                        ?ERROR_BAD_DATA(<<"payload">>)
                end
            catch
                _:_  ->
                    ?debug("Unrecognized resoponse from Elasticsearch in harvester ~p for index ~p: ~p", 
                        [HarvesterId, IndexId, Body]),
                    ?ERROR_BAD_DATA(<<"payload">>)
            end;
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback delete_entry/3.
%% @end
%%--------------------------------------------------------------------
-spec delete_entry(od_harvester:endpoint(), od_harvester:id(),  od_harvester:index_id(),
    od_harvester:entry_id()) -> ok | {error, term()}.
delete_entry(Endpoint, HarvesterId, IndexId, Id) ->
    case do_request(delete, Endpoint, HarvesterId, IndexId, ?ENTRY_PATH(Id), <<>>, [{200,300}, 404]) of
        {ok,_,_,_} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link harvester_plugin_behaviour} callback query/3.
%% @end
%%--------------------------------------------------------------------
-spec query_index(od_harvester:endpoint(), od_harvester:id(), od_harvester:index_id(), Data :: #{}) ->
    {ok, map()} | {error, term()}.
query_index(Endpoint, HarvesterId, IndexId, Data) ->
    #{
        <<"method">> := Method,
        <<"path">> := Path
    } = Data,
    Body = maps:get(<<"body">>, Data, <<>>),
    case do_request(Method, Endpoint, HarvesterId, IndexId, ?ENTRY_PATH(Path), Body) of
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
-spec query_validator() -> maps:map().
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
%% Makes request to elasticsearch.
%% Received status code will NOT be checked.
%% @end
%%--------------------------------------------------------------------
-spec do_request(http_client:method(), od_harvester:endpoint(), od_harvester:id(), 
    od_harvester:index_id(), Path :: binary(), Data :: binary()) -> ok.
do_request(Method, Endpoint, HarvesterId, IndexId, Path, Data) ->
    do_request(Method, Endpoint, HarvesterId, IndexId, Path, Data, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes request to elasticsearch.
%% When ExpectedCodes lists is provided returns error if response has wrong status code. 
%% ExpectedCodes list must conform to specification in is_code_expected/2.
%% @end
%%--------------------------------------------------------------------
-spec do_request(http_client:method(), od_harvester:endpoint(), od_harvester:id(), 
    od_harvester:index_id(), Path :: binary(), Data :: binary(), 
    ExpectedCodes :: [integer() | {integer(), integer()}] | undefined) -> ok.
do_request(Method, Endpoint, HarvesterId, IndexId, Path, Data, ExpectedCodes) ->
    Url = <<Endpoint/binary, "/", (?ES_INDEX_ID(HarvesterId, IndexId))/binary, Path/binary>>,
    case http_client:request(Method, Url, #{<<"content-type">> => <<"application/json">>}, Data) of
        {ok, Code, Headers, Body} = Response when is_list(ExpectedCodes) ->
            case is_code_expected(Code, ExpectedCodes) of
                true -> 
                    Response;
                _ ->
                    ?debug("~p ~p in harvester ~p, index: ~p returned unexpected response ~p:~n ~p~n~p",
                        [Method, Url, HarvesterId, IndexId, Code, Headers, json_utils:decode(Body)]),
                    ?ERROR_BAD_DATA(<<"payload">>)
            end;
        {ok,_,_,_} = Response ->
            Response;
        {error, _} = Error ->
            ?error("~p in harvester ~p, index ~p was unsuccessful due to ~p", 
                [Method, HarvesterId, IndexId, Error]),
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

