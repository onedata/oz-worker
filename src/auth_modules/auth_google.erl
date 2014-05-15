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
-export([get_redirect_url/0, validate_login/1]).


xrds_endpoint() ->
    <<"https://accounts.google.com/.well-known/openid-configuration">>.


authorize_endpoint() ->
    parse_json(auth_utils:get_xrds(xrds_endpoint()), <<"authorization_endpoint">>).


access_token_endpoint() ->
    parse_json(auth_utils:get_xrds(xrds_endpoint()), <<"token_endpoint">>).


user_info_endpoint() ->
    parse_json(auth_utils:get_xrds(xrds_endpoint()), <<"userinfo_endpoint">>).


get_redirect_url() ->
    try
        ParamsProplist = [
            {<<"client_id">>, auth_utils:get_provider_app_id(?PROVIDER_NAME)},
            {<<"response_type">>, <<"code">>},
            {<<"scope">>, <<"openid email profile">>},
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
            {<<"code">>, <<Code/binary>>},
            {<<"client_id">>, auth_utils:get_provider_app_id(?PROVIDER_NAME)},
            {<<"client_secret">>, auth_utils:get_provider_app_secret(?PROVIDER_NAME)},
            {<<"redirect_uri">>, <<(auth_utils:local_auth_endpoint())/binary>>},
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


extract_emails(JSONProplist) ->
    case proplists:get_value(<<"email">>, JSONProplist, <<"">>) of
        <<"">> -> [];
        Email -> [Email]
    end.


parse_json(Body, Key) ->
    {struct, Proplist} = n2o_json:decode(Body),
    proplists:get_value(Key, Proplist).


% Alternative flow: this can be used with id_token to get user id, and then make a request
% to https://www.googleapis.com/plus/v1/people/<user-id>

%% parse_jwt(Token) ->
%%     try
%%         [_Header, ClaimSet, _Signature] = binary:split(Token, [<<".">>], [global]),
%%         % TODO check signature
%% %%         {struct, HeaderJSON} = n2o_json:decode(base64decode(Header)),
%% %%         <<"RS256">> = proplists:get_value(<<"alg">>, HeaderJSON),
%% %%         Kid = proplists:get_value(<<"kid">>, HeaderJSON),
%% %%         Signature2 = base64encode(crypto:hmac(sha256, Kid, <<Header/binary, ".", ClaimSet/binary>>)),
%% %%         Signature3 = public_key:sign(<<Header/binary, ".", ClaimSet/binary>>, sha256, Kid),
%% %%         ?dump(Signature),
%% %%         ?dump(Signature2),
%% %%         ?dump(Signature3),
%%         {struct, ClaimSetJSON} = n2o_json:decode(base64decode(ClaimSet)),
%%         ClaimSetJSON
%%     catch
%%         T:M ->
%%             ?error_stacktrace("~p:~p", [T, M]),
%%             error
%%     end.
%%
%%
%% %% base64encode(Bin) when is_binary(Bin) ->
%% %%     << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin), D =/= $= >>.
%%
%% %% urlencode_digit($/) -> $_;
%% %% urlencode_digit($+) -> $-;
%% %% urlencode_digit(D) -> D.
%%
%%
%% base64decode(Bin) when is_binary(Bin) ->
%%     Bin2 = case byte_size(Bin) rem 4 of
%%                2 -> <<Bin/binary, "==">>;
%%                3 -> <<Bin/binary, "=">>;
%%                _ -> Bin
%%            end,
%%     base64:decode(<<<<(urldecode_digit(D))>> || <<D>> <= Bin2>>).
%%
%%
%%
%% urldecode_digit($_) -> $/;
%% urldecode_digit($-) -> $+;
%% urldecode_digit(D) -> D.