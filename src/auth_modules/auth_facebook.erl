%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module implements auth_module_behaviour and handles singning in
%% via Github.
%% @end
%% ===================================================================
-module(auth_facebook).
-behaviour(auth_module_behaviour).

-include("logging.hrl").

-define(PROVIDER_NAME, facebook).

%% API
-export([get_redirect_url/0, validate_login/1]).


authorize_endpoint() ->
    <<"https://www.facebook.com/dialog/oauth">>.


access_token_endpoint() ->
    <<"https://graph.facebook.com/oauth/access_token">>.


user_info_endpoint() ->
    <<"https://graph.facebook.com/me">>.


get_redirect_url() ->
    try
        ParamsProplist = [
            {<<"client_id">>, auth_utils:get_provider_app_id(?PROVIDER_NAME)},
            {<<"redirect_uri">>, <<(auth_utils:local_auth_endpoint())/binary>>},
            {<<"state">>, auth_utils:generate_state_token(?MODULE)}
        ],
        Params = auth_utils:proplist_to_params(ParamsProplist),

        {ok, <<(authorize_endpoint())/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace(gui_utils:to_list(?PROVIDER_NAME)),
            {error, {Type, Message}}
    end.



validate_login(ParamsProplist) ->
    try
        % Parse out code parameter
        Code = proplists:get_value(<<"code">>, ParamsProplist),
        % Form access token request
        NewParamsProplist = [
            {<<"client_id">>, auth_utils:get_provider_app_id(?PROVIDER_NAME)},
            {<<"client_secret">>, auth_utils:get_provider_app_secret(?PROVIDER_NAME)},
            {<<"redirect_uri">>, <<(auth_utils:local_auth_endpoint())/binary>>},
            {<<"code">>, <<Code/binary>>}
        ],
        % Convert proplist to params string
        Params = auth_utils:proplist_to_params(NewParamsProplist),
        URL = <<(access_token_endpoint())/binary, "?", Params/binary>>,
        % Send request to Facebook endpoint
        {ok, "200", _, Response} = ibrowse:send_req(
            binary_to_list(URL),
            [{content_type, "application/x-www-form-urlencoded"}],
            get, [], [{response_format, binary}]),

        % Parse out received access token
        AccessToken = proplists:get_value(<<"access_token">>, cowboy_http:x_www_form_urlencoded(Response)),

        % Form user info request
        URL2 = <<(user_info_endpoint())/binary, "?access_token=", AccessToken/binary>>,
        % Send request to GitHub endpoint
        {ok, "200", _, JSON} = ibrowse:send_req(
            binary_to_list(URL2),
            [{content_type, "application/x-www-form-urlencoded"}],
            get, [], [{response_format, binary}]),
        % Parse received JSON
        n2o_json:decode(JSON)
    catch
        Type:Message ->
            {error, {Type, Message}}
    end.