%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module provides convenience functions that can be used from
%% auth modules.
%% @end
%% ===================================================================
-module(auth_utils).

-include("logging.hrl").
-include("auth_common.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").

%% API
-export([proplist_to_params/1, fully_qualified_url/1]).

-export([get_xrds/1, local_auth_endpoint/0, validate_login/0]).

-export([load_auth_config/0, get_auth_config/1, get_auth_providers/0]).

-export([init_state_memory/0, generate_state_token/1, lookup_state_token/1, clear_expired_tokens/0, generate_uuid/0]).

-export([get_provider_module/1, get_provider_app_id/1, get_provider_app_secret/1]).
-export([get_provider_button_text/1, get_provider_button_icon/1, get_provider_button_color/1]).

-define(STATE_TTL, 60).
-define(STATE_ETS, auth_state_ets).


proplist_to_params(List) ->
    lists:foldl(
        fun(Tuple, Acc) ->
            {KeyEncoded, ValueEncoded} = case Tuple of
                                             {Key, Value, no_encode} ->
                                                 {Key, Value};
                                             {Key, Value} ->
                                                 {gui_utils:to_binary(wf:url_encode(Key)),
                                                     gui_utils:to_binary(wf:url_encode(Value))}
                                         end,
            Suffix = case Acc of
                         <<"">> -> <<"">>;
                         _ -> <<Acc/binary, "&">>
                     end,
            <<Suffix/binary, KeyEncoded/binary, "=", ValueEncoded/binary>>
        end, <<"">>, List).


fully_qualified_url(Binary) ->
    case Binary of
        <<"https://www.", _/binary>> -> Binary;
        <<"https://", Rest/binary>> -> <<"https://www.", Rest/binary>>;
        <<"www.", _/binary>> -> <<"https://", Binary/binary>>;
        _ -> <<"https://www.", Binary/binary>>
    end.


local_auth_endpoint() ->
    <<(auth_utils:fully_qualified_url(gui_utils:get_requested_hostname()))/binary, ?local_auth_endpoint>>.


validate_login() ->
    ParamsProplist = gui_utils:get_request_params(),
    State = proplists:get_value(<<"state">>, ParamsProplist),
    StateInfo = auth_utils:lookup_state_token(State),
    case StateInfo of
        error ->
            {error, invalid_state};
        Props ->
            Module = proplists:get_value(module, Props),
            Redirect = proplists:get_value(redirect_after_login, Props),
            Res = Module:validate_login(proplists:delete(<<"state">>, ParamsProplist)),
            {Res, Redirect}
    end.


init_state_memory() ->
    ets:new(?STATE_ETS, [named_table, public, bag, {read_concurrency, true}]),
    {A_SEED, B_SEED, C_SEED} = now(),
    L_SEED = atom_to_list(node()),
    {_, Sum_SEED} = lists:foldl(fun(Elem_SEED, {N_SEED, Acc_SEED}) ->
        {N_SEED * 137, Acc_SEED + Elem_SEED * N_SEED} end, {1, 0}, L_SEED),
    random:seed(Sum_SEED * 10000 + A_SEED, B_SEED, C_SEED),
    ok.


generate_state_token(HandlerModule) ->
    clear_expired_tokens(),
    {Token, Time} = generate_uuid(),

    RedirectAfterLogin = case wf:q(<<"x">>) of
                             undefined -> <<"/">>;
                             TargetPage -> TargetPage
                         end,
    StateInfo = [
        {module, HandlerModule},
        {redirect_after_login, RedirectAfterLogin}
    ],

    ets:insert(?STATE_ETS, {Token, Time, StateInfo}),
    Token.

generate_uuid() ->
    {M, S, N} = now(),
    Time = M * 1000000000000 + S * 1000000 + N,
    TimeHex = string:right(integer_to_list(Time, 16), 14, $0),
    Rand = [lists:nth(1, integer_to_list(random:uniform(16) - 1, 16)) || _ <- lists:seq(1, 18)],
    UUID = list_to_binary(string:to_lower(string:concat(TimeHex, Rand))),
    {UUID, Time}.


lookup_state_token(Token) ->
    clear_expired_tokens(),
    case ets:lookup(?STATE_ETS, Token) of
        [{Token, Time, LoginInfo}] ->
            ets:delete_object(?STATE_ETS, {Token, Time, LoginInfo}),
            LoginInfo;
        _ ->
            error
    end.


clear_expired_tokens() ->
    {M, S, N} = now(),
    Time = M * 1000000000000 + S * 1000000 + N,
    ets:select_delete(?STATE_ETS, [{{'$1', '$2', '$3'}, [{'<', '$2', Time - (?STATE_TTL * 1000000)}], ['$_']}]).


%% get_xrds/1
%% ====================================================================
%% @doc
%% Downloads an XRDS document from given URL.
%% @end
-spec get_xrds(string()) -> string().
%% ====================================================================
get_xrds(URL) ->
    % Maximum redirection count = 5
    get_xrds(gui_utils:to_list(URL), 5).


load_auth_config() ->
    {ok, [Config]} = file:consult("gui_static/auth.config"),
    application:set_env(veil_cluster_node, auth_config, Config).


get_auth_providers() ->
    {ok, Config} = application:get_env(veil_cluster_node, auth_config),
    lists:map(
        fun({Provider, _}) ->
            Provider
        end, Config).


get_auth_config(Provider) ->
    {ok, Config} = application:get_env(veil_cluster_node, auth_config),
    proplists:get_value(Provider, Config).


get_provider_module(Provider) ->
    proplists:get_value(auth_module, get_auth_config(Provider)).


get_provider_app_id(Provider) ->
    proplists:get_value(app_id, get_auth_config(Provider)).


get_provider_app_secret(Provider) ->
    proplists:get_value(app_secret, get_auth_config(Provider)).


get_provider_button_text(Provider) ->
    proplists:get_value(button_text, get_auth_config(Provider)).


get_provider_button_icon(Provider) ->
    proplists:get_value(button_icon, get_auth_config(Provider)).


get_provider_button_color(Provider) ->
    proplists:get_value(button_color, get_auth_config(Provider)).


%% ===================================================================
%% Internal functions
%% ===================================================================

%% get_xrds/2
%% ====================================================================
%% @doc
%% Downloads xrds file performing GET on provided URL. Supports redirects.
%% @end
-spec get_xrds(string(), integer()) -> string().
%% ====================================================================
get_xrds(URL, Redirects) ->
    ReqHeaders =
        [
            %{"Accept", "application/xrds+xml;level=1, */*"},
            {"Connection", "close"}
        ],
    {ok, "200", _, Response} = ibrowse:send_req(URL, ReqHeaders, get, [], [{response_format, binary}]),
    case Response of
        {ok, Rcode, RespHeaders, _Body} when Rcode > 300 andalso Rcode < 304 andalso Redirects > 0 ->
            case get_redirect_url(URL, RespHeaders) of
                undefined -> Response;
                URL -> Response;
                NewURL -> get_xrds(NewURL, Redirects - 1)
            end;
        Response -> Response
    end.


%% get_redirect_url/1
%% ====================================================================
%% @doc
%% Retrieves redirect URL from a HTTP response.
%% @end
-spec get_redirect_url(string(), list()) -> string().
%% ====================================================================
get_redirect_url(OldURL, Headers) ->
    Location = proplists:get_value("location", Headers),
    case Location of
        "http://" ++ _ -> Location;
        "https://" ++ _ -> Location;
        [$/ | _] = Location ->
            #url{protocol = Protocol, host = Host, port = Port} = ibrowse_lib:parse_url(OldURL),
            PortFrag = case {Protocol, Port} of
                           {http, 80} -> "";
                           {https, 443} -> "";
                           _ -> ":" ++ integer_to_list(Port)
                       end,
            atom_to_list(Protocol) ++ "://" ++ Host ++ PortFrag ++ Location;
        _ -> undefined
    end.