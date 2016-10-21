%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Utility functions used in REST tests.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_test_utils).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("subscriptions_test_utils.hrl").
-include("subscriptions/subscriptions.hrl").
-include_lib("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%% API
-export([set_config/1, check_rest_call/1]).


set_config(Config) ->
    % Resolve REST URLs of oz-worker nodes
    Nodes = ?config(oz_worker_nodes, Config),
    RestURLs = lists:map(fun(Node) ->
        NodeIP = test_utils:get_docker_ip(Node),
        {ok, RestPort} = rpc:call(
            Node, application, get_env, [?APP_Name, rest_port]
        ),
        {ok, RestAPIPrefix} = rpc:call(
            Node, application, get_env, [?APP_Name, rest_api_prefix]
        ),
        str_utils:format_bin(
            "https://~s:~B~s", [NodeIP, RestPort, RestAPIPrefix]
        )
    end, Nodes),
    % Save the config and REST URLs in process dict.
    put(config, Config),
    put(rest_urls, RestURLs),
    ok.


get_config(Key) ->
    case get(Key) of
        undefined ->
            error("Use rest_test_utils:set_config/1 first.");
        Val ->
            Val
    end.

%%--------------------------------------------------------------------
%% Performs a REST call and check the output if it matches the expected.
%% Returns true when id does and mismatch details when it does not, so
%% it is strongly recommended to wrap the call to this function in an assertion.
%% Args map looks like following:
%% #{
%%    request => #{
%%      method => get, % Optional, default: get
%%      path => [<<"/parts">>, <<"/to/be">>, <<"/concatenated">>], % Mandatory
%%      headers => [{<<"key">>, <<"value">>}], % Optional, default: ct=app/json
%%      body => <<"body content">>, % Optional, default: <<"">>
%%      auth => <<"uid">> orelse undefined, % Optional, default: undefined
%%      opts => [http_client_option] % Optional, default: []
%%    },
%%    expect => #{
%%      code => 200, % Optional, by default not validated
%%      headers => [{<<"key">>, <<"value">>}], % Optional, by def. not validated
%%                 {contains, [{<<"key">>, <<"value">>}]} % checks if given
%%                      (key, value) pair is included in response headers
%%      body => <<"binary">> orelse #{} % Optional, by default not validated
%%      % Specifying a map here will cause validation of JSON content-wise
%%      % (if the JSON object specified by map is equal to the one in reply)
%%    }
%% }
%%--------------------------------------------------------------------
check_rest_call(ArgsMap) ->
    try
        RequestMap = maps:get(request, ArgsMap),
        ExpectMap = maps:get(expect, ArgsMap),

        ReqMethod = maps:get(method, RequestMap, get),
        ReqPath = case maps:get(path, RequestMap) of
            Bin when is_binary(Bin) ->
                [Bin];
            List ->
                List
        end,
        ReqHeaders = maps:get(headers, RequestMap, [
            {<<"content-type">>, <<"application/json">>}
        ]),
        ReqBody = case maps:get(body, RequestMap, <<"">>) of
            Bin2 when is_binary(Bin2) ->
                Bin2;
            Map2 when is_map(Map2) ->
                json_utils:encode_map(Map2)
        end,
        ReqAuth = maps:get(auth, RequestMap, undefined),
        ReqOpts = maps:get(opts, RequestMap, []),
        ReqURL = maps:get(url, RequestMap, get_random_oz_url()),

        ExpCode = maps:get(code, ExpectMap, undefined),
        ExpHeaders = maps:get(headers, ExpectMap, undefined),
        ExpBody = maps:get(body, ExpectMap, undefined),

        URL = str_utils:join_binary([ReqURL | ReqPath], <<"">>),
        HeadersPlusAuth = case ReqAuth of
            undefined ->
                ReqHeaders;
            UserId ->
                % Cache user auth tokens, if none in cache create a new one.
                Macaroon = case get({macaroon, UserId}) of
                    undefined ->
                        Mac = oz_test_utils:get_client_token(
                            get_config(config), UserId
                        ),
                        put({macaroon, UserId}, Mac),
                        Mac;
                    Mac ->
                        Mac
                end,
                % Use "macaroon" and "X-Auth-Token" headers variably, as they
                % both should be accepted.
                HeaderName = case rand:uniform(2) of
                    1 -> <<"macaroon">>;
                    2 -> <<"X-Auth-Token">>
                end,
                [{HeaderName, Macaroon} | ReqHeaders]
        end,
        % Add insecure option - we do not want the GR server cert to be checked.
        {ok, RespCode, RespHeaders, RespBody} = http_client:request(
            ReqMethod, URL, HeadersPlusAuth, ReqBody, [insecure | ReqOpts]
        ),

        % Check response code if specified
        case ExpCode of
            undefined ->
                ok;
            _ ->
                case RespCode of
                    ExpCode ->
                        ok;
                    _ ->
                        throw({code, RespCode, ExpCode})
                end
        end,

        % Check response headers if specified
        case ExpHeaders of
            undefined ->
                ok;
            {contains, ActualExpHeaders} ->
                NormExpHeaders = normalize_headers(ActualExpHeaders),
                NormRespHeaders = normalize_headers(RespHeaders),
                case NormExpHeaders -- NormRespHeaders of
                    [] -> ok;
                    _ -> throw({headers, NormRespHeaders, NormExpHeaders})
                end;
            _ ->
                NormExpHeaders = normalize_headers(ExpHeaders),
                NormRespHeaders = normalize_headers(RespHeaders),
                case NormRespHeaders of
                    NormExpHeaders ->
                        ok;
                    _ ->
                        throw({headers, NormRespHeaders, NormExpHeaders})
                end
        end,

        % Check response body if specified
        case ExpBody of
            undefined ->
                ok;
            Bin3 when is_binary(Bin3) ->
                case RespBody of
                    ExpBody ->
                        ok;
                    _ ->
                        throw({body, RespBody, ExpBody})
                end;
            Map3 when is_map(Map3) ->
                RespBodyMap = json_utils:decode_map(RespBody),
                case compare_maps(RespBodyMap, ExpBody) of
                    true ->
                        ok;
                    false ->
                        throw({body, RespBodyMap, ExpBody})
                end;
            {contains, Map4} when is_map(Map4) ->
                RespBodyMap = json_utils:decode_map(RespBody),
                case contains_map(Map4, RespBodyMap) of
                    true ->
                        ok;
                    false ->
                        throw({body_contains, RespBodyMap, Map4})
                end;
            #xmlElement{} = ExpBodyXML ->
                {RespBodyXML, _} = xmerl_scan:string(binary_to_list(RespBody)),
                case compare_xml(RespBodyXML, ExpBodyXML) of
                    true ->
                        ok;
                    false ->
                        throw({body, RespBodyXML, ExpBodyXML})
                end
        end,

        % Everything OK, return true
        true
    catch
        % Something wrong, return details. If assert is used, the test will fail
        % and properly display the point of failure.
        {Type, Actual, Expected} ->
            {
                Type,
                {got, Actual},
                {expected, Expected}
            }
    end.


get_random_oz_url() ->
    RestURLs = get_config(rest_urls),
    lists:nth(rand:uniform(length(RestURLs)), RestURLs).


% Convert all header keys to lowercase so comparing is easier
normalize_headers(Headers) ->
    lists:sort(
        lists:map(fun({Key, Value}) ->
            KeyLower = list_to_binary(string:to_lower(binary_to_list(Key))),
            {KeyLower, Value}
        end, Headers)
    ).


% Returns true if two maps have the same contents
compare_maps(Map1, Map2) ->
    sort_map(Map1) =:= sort_map(Map2).

% Returns true if second map has all the mappings of the first map with
% same values.
contains_map(Map1, Map2) ->
    SortedMap1 = sort_map(Map1),
    SortedMap2 = sort_map(Map2),
    lists:all(
        fun(Key) ->
            Value1 = maps:get(Key, SortedMap1),
            case maps:is_key(Key, SortedMap2) of
                false ->
                    false;
                true ->
                    Value2 = maps:get(Key, SortedMap2),
                    Value1 =:= Value2
            end
        end, maps:keys(SortedMap1)).


% Sorts all nested lists in a map and returns the result map
sort_map(OriginalMap) ->
    lists:foldl(
        fun(Key, MapAcc) ->
            case maps:get(Key, MapAcc) of
                List when is_list(List) ->
                    maps:put(Key, lists:sort(List), MapAcc);
                Map when is_map(Map) ->
                    maps:put(Key, sort_map(Map), MapAcc);
                _ ->
                    MapAcc
            end
        end, OriginalMap, maps:keys(OriginalMap)).

compare_xml(_, []) -> true;
compare_xml(#xmlText{value = V}, #xmlText{value = V}) -> true;
compare_xml(#xmlText{value = _V1}, #xmlText{value = _V2}) -> false;
compare_xml(#xmlAttribute{name = N, value = V}, #xmlAttribute{name = N, value = V}) ->
    true;
compare_xml(#xmlAttribute{name = _N1, value = _V1}, #xmlAttribute{name = _N2, value = _V2}) ->
    false;
compare_xml(#xmlElement{name = Name, attributes = _, content = _},
    #xmlElement{name = Name, attributes = [], content = []}) -> true;
compare_xml(#xmlElement{name = Name, attributes = RespAttributes, content = RespContent},
    #xmlElement{name = Name, attributes = ExpAttributes, content = ExpContent}) ->
    case compare_xml(RespAttributes, ExpAttributes) of
        false -> false;
        true -> compare_xml(RespContent, ExpContent)
    end;
compare_xml(Resp, [Exp | ExpRest]) when is_list(Resp) ->
    compare_xml(Resp, Exp) and compare_xml(Resp, ExpRest);
compare_xml(Resp, Exp) when is_list(Resp) ->
    lists:foldl(fun(R, Acc) ->
        compare_xml(R, Exp) or Acc
    end, false, Resp);
compare_xml(_, _) -> false.