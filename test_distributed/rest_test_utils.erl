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
-include_lib("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%% API
-export([check_rest_call/2]).
-export([compare_maps/2, contains_map/2]).


% Use "Macaroon", "X-Auth-Token" and "Authorization: Bearer" headers variably,
% as they all should be accepted.
-define(ACCESS_TOKEN_HEADER(AccessToken), case rand:uniform(3) of
    1 -> #{?HDR_MACAROON => AccessToken}; %% @TODO VFS-5554 Deprecated
    2 -> #{?HDR_X_AUTH_TOKEN => AccessToken};
    3 -> #{?HDR_AUTHORIZATION => <<"Bearer ", AccessToken/binary>>}
end).

-define(CONNECT_OPTS, [
    {connect_timeout, 60000},
    {recv_timeout, 60000}
]).


%%--------------------------------------------------------------------
%% Performs a REST call and check the output if it matches the expected.
%% Returns true when id does and mismatch details when it does not, so
%% it is strongly recommended to wrap the call to this function in an assertion.
%% Args map looks like following:
%% #{
%%    request => #{
%%      method => % Optional, default: get
%%          get
%%          post
%%          put
%%          patch
%%          delete
%%      path => % Mandatory
%%          [<<"/parts">>, <<"/to/be">>, <<"/concatenated">>],
%%      url => % Optional, default: {@link oz_test_utils:oz_rest_url/2}
%%          <<"oz-domain-with:port/and/api/prefix">>
%%      headers => % Optional, default: content-type=app/json
%%          [{<<"key">>, <<"value">>}]
%%      body => % Optional, default: <<"">>
%%          <<"body content">>,
%%      auth => % Optional, default: undefined
%%          nobody
%%          root
%%          {user, <<"uid">>}
%%          {user, <<"uid">>, <<"token">>}
%%          {provider, <<"id">>}
%%          {provider, <<"id">>, <<"token">>}
%%          % Uses the same auth as provider, but indicates the service type
%%          (in the auth header a.k.a. service access token)
%%          {op_panel, <<"id">>}
%%          {op_panel, <<"id">>, <<"token">>}
%%          undefined
%%      opts => % Optional, default: []
%%          [http_client_option]
%%    },
%%    expect => #{
%%      code => % Optional, by default not validated
%%          200
%%      headers => % Optional, by def. not validated
%%          #{<<"key">> => <<"value">>}
%%          fun(Headers) -> boolean() % Verification function
%%          {contains, #{<<"key">> => <<"value">>}} % checks if given
%%              (key, value) pair is included in response headers
%%      body => % Optional, by default not validated
%%          <<"binary">>
%%          #{}
%%          fun(Body) -> boolean() % Verification function
%%          {check_type, binary}
%%          {contains, #{}}
%%      % Specifying a map here will cause validation of JSON content-wise
%%      % (if the JSON object specified by map is equal to the one in reply)
%%    }
%% }
%%--------------------------------------------------------------------
check_rest_call(Config, ArgsMap) ->
    try
        RequestMap = maps:get(request, ArgsMap),
        ExpectMap = maps:get(expect, ArgsMap),

        ReqMethod = maps:get(method, RequestMap, get),
        ReqPath = case maps:get(path, RequestMap) of
            Bin1 when is_binary(Bin1) ->
                [Bin1];
            List ->
                List
        end,
        ReqHeaders = case maps:get(headers, RequestMap, undefined) of
            undefined ->
                #{?HDR_CONTENT_TYPE => <<"application/json">>};
            Map2 when is_map(Map2) ->
                Map2
        end,
        ReqBody = case maps:get(body, RequestMap, undefined) of
            undefined ->
                <<"">>;
            Bin3 when is_binary(Bin3) ->
                Bin3;
            Map3 when is_map(Map3) ->
                json_utils:encode(Map3)
        end,
        ReqOpts = maps:get(opts, RequestMap, []),
        ReqURL = maps:get(url, RequestMap, oz_test_utils:oz_rest_url(Config, <<"">>)),

        ExpCode = maps:get(code, ExpectMap, undefined),
        ExpHeaders = maps:get(headers, ExpectMap, undefined),
        ExpBody = maps:get(body, ExpectMap, undefined),

        URL = str_utils:join_binary([ReqURL | ReqPath]),
        ReqAuth = maps:get(auth, RequestMap, undefined),
        HeadersPlusAuth = case ReqAuth of
            undefined ->
                ReqHeaders;
            nobody ->
                ReqHeaders;
            {provider, ProviderId} ->
                Token = oz_test_utils:acquire_temporary_token(Config, ?SUB(?ONEPROVIDER, ProviderId)),
                maps:merge(ReqHeaders, ?ACCESS_TOKEN_HEADER(Token));
            {provider, _, Token} ->
                maps:merge(ReqHeaders, ?ACCESS_TOKEN_HEADER(Token));
            {op_panel, ProviderId} ->
                Token = oz_test_utils:acquire_temporary_token(Config, ?SUB(?ONEPROVIDER, ProviderId)),
                ServiceToken = tokens:add_oneprovider_service_indication(?OP_PANEL, Token),
                maps:merge(ReqHeaders, ?ACCESS_TOKEN_HEADER(ServiceToken));
            {op_panel, _, Token} ->
                ServiceToken = tokens:add_oneprovider_service_indication(?OP_PANEL, Token),
                maps:merge(ReqHeaders, ?ACCESS_TOKEN_HEADER(ServiceToken));
            {user, UserId} ->
                Token = oz_test_utils:acquire_temporary_token(Config, ?SUB(user, UserId)),
                maps:merge(ReqHeaders, ?ACCESS_TOKEN_HEADER(Token));
            {user, _, Token} ->
                maps:merge(ReqHeaders, ?ACCESS_TOKEN_HEADER(Token))
        end,

%%        %% Useful for debug
%%        ct:pal("[Req]: ~n"
%%        "   ReqMethod: ~tp~n"
%%        "   URL: ~tp~n"
%%        "   HeadersPlusAuth: ~tp~n"
%%        "   ReqBody: ~tp~n"
%%        "   Opts: ~tp~n", [
%%            ReqMethod, URL, HeadersPlusAuth, ReqBody, [{pool, false}, ReqOptsPlusAuth]
%%        ]),

        CaCerts = oz_test_utils:gui_ca_certs(Config),
        SslOpts = proplists:get_value(ssl_options, ReqOpts, []),
        CompleteOpts = [
            {ssl_options, [{cacerts, CaCerts} | SslOpts]} |
            proplists:delete(ssl_options, ReqOpts)
        ],

        {ok, RespCode, RespHeaders, RespBody} = http_client:request(
            ReqMethod,
            URL,
            HeadersPlusAuth,
            ReqBody,
            ?CONNECT_OPTS ++ CompleteOpts
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
                        throw({code, RespCode, ExpCode, {
                            RespCode, RespHeaders, RespBody
                        }})
                end
        end,

        % Check response headers if specified
        case ExpHeaders of
            undefined ->
                ok;
            Fun when is_function(Fun, 1) ->
                Result = try
                    Fun(RespHeaders)
                catch
                    Type1:Message1:Stacktrace1 ->
                        ct:pal(
                            "Headers verification function crashed - ~tp:~tp~n"
                            "Stacktrace: ~ts", [
                                Type1, Message1, lager:pr_stacktrace(Stacktrace1)
                            ]),
                        false
                end,
                case Result of
                    true ->
                        ok;
                    false ->
                        throw({headers, RespHeaders, ExpHeaders, {
                            RespCode, RespHeaders, RespBody
                        }})
                end;
            {contains, ExpContainsHeaders} ->
                case contains_headers(RespHeaders, ExpContainsHeaders) of
                    true ->
                        ok;
                    false ->
                        throw({headers_contain, RespHeaders, ExpContainsHeaders, {
                            RespCode, RespHeaders, RespBody
                        }})
                end;
            _ ->
                case compare_headers(RespHeaders, ExpHeaders) of
                    true ->
                        ok;
                    false ->
                        throw({headers, RespHeaders, ExpHeaders, {
                            RespCode, RespHeaders, RespBody
                        }})
                end
        end,
        % Check response body if specified
        case ExpBody of
            undefined ->
                ok;
            Fun2 when is_function(Fun2, 1) ->
                ActualBodyMap = json_utils:decode(RespBody),
                Result2 = try
                    Fun2(ActualBodyMap)
                catch
                    Type2:Message2:Stacktrace2 ->
                        ct:pal(
                            "Body verification function crashed - ~tp:~tp~n"
                            "Stacktrace: ~ts", [
                                Type2, Message2, lager:pr_stacktrace(Stacktrace2)
                            ]),
                        false
                end,
                case Result2 of
                    true ->
                        ok;
                    false ->
                        throw({body, RespBody, ExpBody, {
                            RespCode, RespHeaders, RespBody
                        }})
                end;
            {check_type, binary} ->
                case RespBody of
                    Bin4 when is_binary(Bin4) ->
                        ok;
                    _ ->
                        throw({body, RespBody, ExpBody, {
                            RespCode, RespHeaders, RespBody
                        }})
                end;
            Bin5 when is_binary(Bin5) ->
                case RespBody of
                    ExpBody ->
                        ok;
                    _ ->
                        throw({body, RespBody, ExpBody, {
                            RespCode, RespHeaders, RespBody
                        }})
                end;
            Map4 when is_map(Map4) ->
                ActualBodyMap = json_utils:decode(RespBody),
                case compare_maps(ActualBodyMap, ExpBody) of
                    true ->
                        ok;
                    false ->
                        throw({body, ActualBodyMap, ExpBody, {
                            RespCode, RespHeaders, RespBody
                        }})
                end;
            {contains, ExpContainsMap} when is_map(ExpContainsMap) ->
                ActualBodyMap = json_utils:decode(RespBody),
                case contains_map(ActualBodyMap, ExpContainsMap) of
                    true ->
                        ok;
                    false ->
                        throw({body_contains, ActualBodyMap, ExpContainsMap, {
                            RespCode, RespHeaders, RespBody
                        }})
                end
        end,

        % Everything OK, return true
        true
    catch
        % Something wrong, return details. If assert is used, the test will fail
        % and properly display the point of failure.
        throw:{Type, Actual, Expected, {Code, Headers, Body}} ->
            BodyMap = try json_utils:decode(Body) catch _:_ -> Body end,
            {
                Type,
                {got, Actual},
                {expected, Expected},
                {response, {Code, Headers, BodyMap}}
            };
        % Unexpected error
        Type:Message:Stacktrace ->
            ct:pal(
                "~tp:check_rest_call failed with unexpected result - ~tp:~tp~n"
                "Stacktrace: ~ts", [
                    ?MODULE, Type, Message, lager:pr_stacktrace(Stacktrace)
                ]),
            false
    end.


compare_headers(ActualHeadersInput, ExpectedHeadersInput) ->
    ExpectedMap = normalize_headers(ExpectedHeadersInput),
    ActualMap = normalize_headers(ActualHeadersInput),
    case maps:keys(ExpectedMap) =:= maps:keys(ActualMap) of
        false ->
            false;
        true ->
            lists:all(
                fun({Key, ExpValue}) ->
                    ActualValue = maps:get(Key, ActualMap),
                    case {ExpValue, ActualValue} of
                        {{match, RegExp}, _} ->
                            match =:= re:run(ActualValue, RegExp, [{capture, none}]);
                        {B1, B2} when is_binary(B1) andalso is_binary(B2) ->
                            B1 =:= B2
                    end
                end, maps:to_list(ExpectedMap))
    end.


contains_headers(ActualHeadersInput, ExpectedHeadersInput) ->
    ExpectedMap = normalize_headers(ExpectedHeadersInput),
    ActualMap = normalize_headers(ActualHeadersInput),
    case maps:keys(ExpectedMap) -- maps:keys(ActualMap) =:= [] of
        false ->
            false;
        true ->
            FilteredActualMap = maps:filter(
                fun(Key, _Value) ->
                    lists:member(Key, maps:keys(ExpectedMap))
                end, ActualMap),
            compare_headers(FilteredActualMap, ExpectedMap)
    end.


% Convert all header keys to maps with lowercase keys so comparing is easier
normalize_headers(HeadersList) when is_list(HeadersList) ->
    normalize_headers(maps:from_list(HeadersList));
normalize_headers(HeadersMap) ->
    maps:from_list(
        lists:map(fun({Key, Value}) ->
            KeyLower = list_to_binary(string:to_lower(binary_to_list(Key))),
            {KeyLower, Value}
        end, maps:to_list(HeadersMap))
    ).


% Returns true if two maps have the same contents
compare_maps(ActualMapInput, ExpectedMapInput) ->
    ExpectedMap = sort_map(ExpectedMapInput),
    ActualMap = sort_map(ActualMapInput),
    case maps:keys(ExpectedMap) =:= maps:keys(ActualMap) of
        false ->
            false;
        true ->
            lists:all(
                fun({Key, ExpValue}) ->
                    ActualValue = maps:get(Key, ActualMap),
                    case {ExpValue, ActualValue} of
                        {{check_type, binary}, ActualValue} ->
                            is_binary(ActualValue);
                        {{list_contains, ExpContains}, ActualValue} ->
                                ExpContains -- ActualValue =:= [];
                        {{list_doesnt_contain, ExpContains}, ActualValue} ->
                                ActualValue -- ExpContains =:= ActualValue;
                        {_, _} ->
                            ExpValue =:= ActualValue
                    end
                end, maps:to_list(ExpectedMap))
    end.

% Returns true if second map has all the mappings of the first map with
% same values.
contains_map(ActualMap, ExpectedMap) ->
    case maps:keys(ExpectedMap) -- maps:keys(ActualMap) =:= [] of
        false ->
            false;
        true ->
            FilteredActualMap = maps:filter(
                fun(Key, _Value) ->
                    lists:member(Key, maps:keys(ExpectedMap))
                end, ActualMap),
            compare_maps(ExpectedMap, FilteredActualMap)
    end.


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
