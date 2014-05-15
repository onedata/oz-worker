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
-module(auth_twitter).
-behaviour(auth_module_behaviour).

-include("logging.hrl").
-include("auth_common.hrl").

-define(PROVIDER_NAME, twitter).

%% API
-export([get_redirect_url/0, validate_login/1]).


authorize_endpoint() ->
    <<"https://github.com/login/oauth/authorize">>.


access_token_endpoint() ->
    <<"https://github.com/login/oauth/access_token">>.


user_info_endpoint() ->
    <<"https://api.github.com/user">>.


user_emails_endpoint() ->
    <<"https://api.github.com/user/emails">>.


get_redirect_url() ->
    try
        ParamsProplist = [
            {<<"client_id">>, auth_utils:get_provider_app_id(?PROVIDER_NAME)},
            {<<"redirect_uri">>, <<(auth_utils:local_auth_endpoint())/binary>>},
            {<<"scope">>, <<"user,user:email">>},
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
        % Send request to GitHub endpoint
        {ok, "200", _, Response} = ibrowse:send_req(
            binary_to_list(access_token_endpoint()),
            [{content_type, "application/x-www-form-urlencoded"}],
            post, Params, [{response_format, binary}]),

        % Parse out received access token
        AccessToken = proplists:get_value(<<"access_token">>, cowboy_http:x_www_form_urlencoded(Response)),

        % Form user info request
        URL = <<(user_info_endpoint())/binary, "?access_token=", AccessToken/binary>>,
        % Send request to GitHub endpoint
        {ok, "200", _, JSON} = ibrowse:send_req(
            binary_to_list(URL),
            [{content_type, "application/x-www-form-urlencoded"}, {"User-Agent", "od_test_app"}],
            get, [], [{response_format, binary}]),

        % Form user email request
        URLEmail = <<(user_emails_endpoint())/binary, "?access_token=", AccessToken/binary>>,
        % Send request to GitHub endpoint
        {ok, "200", _, JSONEmails} = ibrowse:send_req(
            binary_to_list(URLEmail),
            [{content_type, "application/x-www-form-urlencoded"}, {"User-Agent", "od_test_app"}],
            get, [], [{response_format, binary}]),

        % Parse received JSON
        {struct, JSONProplist} = n2o_json:decode(JSON),
        ProvUserInfo = #provider_user_info{
            provider_id = ?PROVIDER_NAME,
            user_id = proplists:get_value(<<"id">>, JSONProplist, <<"">>),
            emails = extract_emails(JSONEmails),
            name = proplists:get_value(<<"name">>, JSONProplist, <<"">>),
            login = proplists:get_value(<<"login">>, JSONProplist, <<"">>)
        },
        {ok, ProvUserInfo}
    catch
        Type:Message ->
            {error, {Type, Message}}
    end.


extract_emails(JSON) ->
    EmailsJSON =
        case n2o_json:decode(JSON) of
            {struct, Email} -> [{struct, Email}];
            List when is_list(List) -> List
        end,
    lists:map(
        fun({struct, Email}) ->
            proplists:get_value(<<"email">>, Email)
        end, EmailsJSON).