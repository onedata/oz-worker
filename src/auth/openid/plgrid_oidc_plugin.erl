%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin implementing openid_plugin_behaviour, used to handle OpenID
%%% communication in PL-Grid infrastructure. These callbacks are called
%%% by openid_protocol module.
%%% @end
%%%-------------------------------------------------------------------
-module(plgrid_oidc_plugin).
-behavior(openid_plugin_behaviour).
-author("Lukasz Opiola").

-include("auth/auth_common.hrl").
-include("auth/auth_errors.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("xmerl/include/xmerl.hrl").


-define(CFG_XRDS_ENDPOINT(__IdP),
    ?bin(auth_config:get_protocol_config(__IdP, [pluginConfig, xrds_endpoint], required))
).


%% openid_plugin_behaviour callbacks
-export([get_login_endpoint/3, validate_login/3, get_user_info/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link openid_plugin_behaviour} callback get_login_endpoint/3.
%% @end
%%--------------------------------------------------------------------
-spec get_login_endpoint(auth_config:idp(), state_token:id(),
    auth_logic:redirect_uri()) ->
    auth_logic:login_endpoint().
get_login_endpoint(IdP, State, RedirectUri) ->
    Params = #{
        <<"openid.mode">> => <<"checkid_setup">>,
        <<"openid.ns">> => <<"http://specs.openid.net/auth/2.0">>,
        <<"openid.return_to">> => <<RedirectUri/binary, "?state=", State/binary>>,
        <<"openid.claimed_id">> => <<"http://specs.openid.net/auth/2.0/identifier_select">>,
        <<"openid.identity">> => <<"http://specs.openid.net/auth/2.0/identifier_select">>,
        <<"openid.realm">> => oz_worker:get_url(),
        <<"openid.sreg.required">> => <<"nickname,email,fullname">>,
        <<"openid.ns.ext1">> => <<"http://openid.net/srv/ax/1.0">>,
        <<"openid.ext1.mode">> => <<"fetch_request">>,
        <<"openid.ext1.type.dn1">> => <<"http://openid.plgrid.pl/certificate/dn1">>,
        <<"openid.ext1.type.dn2">> => <<"http://openid.plgrid.pl/certificate/dn2">>,
        <<"openid.ext1.type.dn3">> => <<"http://openid.plgrid.pl/certificate/dn3">>,
        <<"openid.ext1.type.teams">> => <<"http://openid.plgrid.pl/userTeamsXML">>,
        <<"openid.ext1.if_available">> => <<"dn1,dn2,dn3,teams">>
    },
    http_utils:append_url_parameters(plgrid_endpoint(IdP), Params).


%%--------------------------------------------------------------------
%% @doc
%% {@link openid_plugin_behaviour} callback validate_login/3.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_config:idp(), auth_logic:query_params(),
    auth_logic:redirect_uri()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
validate_login(IdP, QueryParams, _RedirectUri) ->
    % Make sure received endpoint is really the PLGrid endpoint
    ReceivedEndpoint = maps:get(<<"openid.op_endpoint">>, QueryParams),
    case plgrid_endpoint(IdP) of
        ReceivedEndpoint -> ok;
        _ -> throw(?ERROR_INVALID_AUTH_REQUEST)
    end,

    % 'openid.signed' contains parameters that must be contained in validation request
    SignedAttrsBin = maps:get(<<"openid.signed">>, QueryParams),
    SignedAttrsWithoutPrefix = binary:split(SignedAttrsBin, <<",">>, [global]),
    SignedAttrs = [<<"openid.", A/binary>> || A <- SignedAttrsWithoutPrefix],

    % Gather values for parameters that were signed
    AttrsToVerify = maps:from_list(lists:map(
        fun(Key) ->
            Value = case maps:get(Key, QueryParams, undefined) of
                undefined -> throw(?ERROR_INVALID_AUTH_REQUEST);
                Val -> Val
            end,
            {Key, Value}
        % And add 'openid.sig' and 'openid.signed' params which are required for validation
        end, SignedAttrs ++ [<<"openid.sig">>, <<"openid.signed">>])),

    Params = AttrsToVerify#{<<"openid.mode">> => <<"check_authentication">>},

    % Send validation request, check if server responded positively
    {_, <<"is_valid:true\n">>} = openid_protocol:request_idp(post, 200, ReceivedEndpoint, #{
        <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
    }, Params),

    % Return signed attributes
    {ok, lists:foldl(fun
        (<<"openid.ext1.value.teams">> = TeamsAttr, Acc) ->
            % In case of teams, parse out team names from the teams XML
            ParsedTeams = parse_teams(maps:get(TeamsAttr, QueryParams)),
            Acc#{TeamsAttr => ParsedTeams};
        (Attr, Acc) ->
            Acc#{Attr => maps:get(Attr, QueryParams)}
    end, #{}, SignedAttrs)}.


%%--------------------------------------------------------------------
%% @doc
%% {@link openid_plugin_behaviour} callback get_user_info/2.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(auth_config:idp(), auth_logic:access_token()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
get_user_info(_IdP, _AccessToken) ->
    ?ERROR_NOT_IMPLEMENTED.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Identity provider endpoint, where users are redirected for authorization.
%% @end
%%--------------------------------------------------------------------
-spec plgrid_endpoint(auth_config:idp()) -> binary().
plgrid_endpoint(IdP) ->
    {ok, Endpoint} = simple_cache:get({cached_xrds, IdP}, fun() ->
        {true, discover_plgrid_endpoint(IdP), ?XRDS_CACHE_TTL}
    end),
    Endpoint.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves an Xrds document for PLGrid IdP and parses out the URI which will
%% be used for OpenID login redirect.
%% @end
%%--------------------------------------------------------------------
-spec discover_plgrid_endpoint(auth_config:idp()) -> binary().
discover_plgrid_endpoint(IdP) ->
    {_, Xrds} = openid_protocol:request_idp(get, 200, ?CFG_XRDS_ENDPOINT(IdP), #{
        <<"Accept">> => <<"application/xrds+xml;level=1, */*">>,
        <<"Connection">> => <<"close">>
    }, #{}, [{follow_redirect, true}, {max_redirect, 5}]),
    {Xml, _} = xmerl_scan:string(binary_to_list(Xrds)),
    [#xmlElement{content = [#xmlText{value = Uri} | _]}] = xmerl_xpath:string("//URI", Xml),
    list_to_binary(Uri).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses user's teams from an XML to a list of binaries.
%% Xmerl works on strings in UTF8, not erlang-string unicode! However, its
%% output is a unicode erlang-string. This is why binary_to_list/1 is used
%% before parsing, and str_utils:unicode_list_to_binary/1 afterwards to convert
%% team names to binaries.
%% @end
%%--------------------------------------------------------------------
-spec parse_teams(binary() | string()) -> [binary()].
parse_teams(Binary) when is_binary(Binary) ->
    parse_teams(binary_to_list(Binary));
parse_teams([]) ->
    [];
parse_teams(XMLContent) ->
    {Xml, _} = xmerl_scan:string(XMLContent),
    TeamList = case xmerl_xpath:string("//teams", Xml) of
        [#xmlElement{content = Teams}] -> Teams;
        _ -> []
    end,
    lists:map(fun(#xmlElement{content = [#xmlText{value = Value}]}) ->
        str_utils:unicode_list_to_binary(Value)
    end, TeamList).
