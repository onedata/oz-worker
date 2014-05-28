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
-module(auth_google).
-behaviour(auth_module_behaviour).

-include("logging.hrl").
-include("auth_common.hrl").

-define(PROVIDER_NAME, google).

%% API
-export([get_redirect_url/1, validate_login/1]).


%% ====================================================================
%% API functions
%% ====================================================================

%% get_redirect_url/1
%% ====================================================================
%% @doc Returns full URL, where the user will be redirected for authorization.
%% See function specification in auth_module_behaviour.
%% @end
%% ====================================================================
-spec get_redirect_url(boolean()) -> method().
%% ====================================================================
get_redirect_url(ConnectAccount) ->
    try
        ParamsProplist = [
            {<<"client_id">>, auth_utils:get_provider_app_id(?PROVIDER_NAME)},
            {<<"response_type">>, <<"code">>},
            {<<"scope">>, <<"openid email profile">>},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"state">>, auth_utils:generate_state_token(?MODULE, ConnectAccount)}
        ],
        Params = auth_utils:proplist_to_params(ParamsProplist),

        {ok, <<(authorize_endpoint())/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace(gui_utils:to_list(?PROVIDER_NAME)),
            {error, {Type, Message}}
    end.


%% validate_login/1
%% ====================================================================
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%% ====================================================================
-spec validate_login([{binary(), binary()}]) -> method().
%% ====================================================================
validate_login(ParamsProplist) ->
    try
        % Parse out code parameter
        Code = proplists:get_value(<<"code">>, ParamsProplist),
        % Form access token request
        NewParamsProplist = [
            {<<"code">>, <<Code/binary>>},
            {<<"client_id">>, auth_utils:get_provider_app_id(?PROVIDER_NAME)},
            {<<"client_secret">>, auth_utils:get_provider_app_secret(?PROVIDER_NAME)},
            {<<"redirect_uri">>, auth_utils:local_auth_endpoint()},
            {<<"grant_type">>, <<"authorization_code">>}
        ],
        % Convert proplist to params string
        Params = auth_utils:proplist_to_params(NewParamsProplist),
        % Send request to Google endpoint
        {ok, "200", _, Response} = ibrowse:send_req(
            binary_to_list(access_token_endpoint()),
            [{content_type, "application/x-www-form-urlencoded"}],
            post, Params, [{response_format, binary}]),

        % Parse out received access token and form a user info request
        AccessToken = parse_json(Response, <<"access_token">>),
        URL = <<(user_info_endpoint())/binary, "?access_token=", AccessToken/binary>>,

        % Send request to Google endpoint
        {ok, "200", _, Response2} = ibrowse:send_req(
            binary_to_list(URL),
            [{content_type, "application/x-www-form-urlencoded"}],
            get, [], [{response_format, binary}]),

        % Parse JSON with user info
        {struct, JSONProplist} = n2o_json:decode(Response2),
        ProvUserInfo = #provider_user_info{
            provider_id = ?PROVIDER_NAME,
            user_id = proplists:get_value(<<"sub">>, JSONProplist, <<"">>),
            emails = extract_emails(JSONProplist),
            name = proplists:get_value(<<"name">>, JSONProplist, <<"">>)
        },
        {ok, ProvUserInfo}
    catch
        Type:Message ->
            {error, {Type, Message}}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% authorize_endpoint/0
%% ====================================================================
%% @doc Provider endpoint, where users are redirected for authorization.
%% @end
%% ====================================================================
-spec authorize_endpoint() -> method().
%% ====================================================================
xrds_endpoint() ->
    proplists:get_value(xrds_endpoint, auth_utils:get_auth_config(?PROVIDER_NAME)).


%% access_token_endpoint/0
%% ====================================================================
%% @doc Provider endpoint, where access token is aquired.
%% @end
%% ====================================================================
-spec access_token_endpoint() -> method().
%% ====================================================================
authorize_endpoint() ->
    parse_json(auth_utils:get_xrds(xrds_endpoint()), <<"authorization_endpoint">>).


%% access_token_endpoint/0
%% ====================================================================
%% @doc Provider endpoint, where access token is aquired.
%% @end
%% ====================================================================
-spec access_token_endpoint() -> method().
%% ====================================================================
access_token_endpoint() ->
    parse_json(auth_utils:get_xrds(xrds_endpoint()), <<"token_endpoint">>).


%% user_info_endpoint/0
%% ====================================================================
%% @doc Provider endpoint, where user info is aquired.
%% @end
%% ====================================================================
-spec user_info_endpoint() -> method().
%% ====================================================================
user_info_endpoint() ->
    parse_json(auth_utils:get_xrds(xrds_endpoint()), <<"userinfo_endpoint">>).


%% extract_emails/1
%% ====================================================================
%% @doc Extracts email list from JSON in erlang format (after decoding).
%% @end
%% ====================================================================
-spec extract_emails([{term(), term()}]) -> method().
%% ====================================================================
extract_emails(JSONProplist) ->
    case proplists:get_value(<<"email">>, JSONProplist, <<"">>) of
        <<"">> -> [];
        Email -> [Email]
    end.


%% parse_json/2
%% ====================================================================
%% @doc Extracts a given key from flat JSON (nested one level).
%% @end
%% ====================================================================
-spec parse_json(binary(), binary()) -> method().
%% ====================================================================
parse_json(JSON, Key) ->
    {struct, Proplist} = n2o_json:decode(JSON),
    proplists:get_value(Key, Proplist).