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
-module(auth_plgrid).
-behaviour(auth_module_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("dao/dao_types.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(PROVIDER_NAME, plgrid).

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
-spec get_redirect_url(boolean()) -> binary().
%% ====================================================================
get_redirect_url(ConnectAccount) ->
    try
        HostName = auth_utils:fully_qualified_url(gui_ctx:get_requested_hostname()),
        RedirectURI = <<(auth_utils:local_auth_endpoint())/binary, "?state=", (auth_logic:generate_state_token(?MODULE, ConnectAccount))/binary>>,

        ParamsProplist = [
            {<<"openid.mode">>, <<"checkid_setup">>},
            {<<"openid.ns">>, <<"http://specs.openid.net/auth/2.0">>},
            {<<"openid.return_to">>, RedirectURI},
            {<<"openid.claimed_id">>, <<"http://specs.openid.net/auth/2.0/identifier_select">>},
            {<<"openid.identity">>, <<"http://specs.openid.net/auth/2.0/identifier_select">>},
            {<<"openid.realm">>, HostName},
            {<<"openid.sreg.required">>, <<"nickname,email,fullname">>},
            {<<"openid.ns.ext1">>, <<"http://openid.net/srv/ax/1.0">>},
            {<<"openid.ext1.mode">>, <<"fetch_request">>},
            {<<"openid.ext1.type.dn1">>, <<"http://openid.plgrid.pl/certificate/dn1">>},
            {<<"openid.ext1.type.dn2">>, <<"http://openid.plgrid.pl/certificate/dn2">>},
            {<<"openid.ext1.type.dn3">>, <<"http://openid.plgrid.pl/certificate/dn3">>},
            {<<"openid.ext1.type.teams">>, <<"http://openid.plgrid.pl/userTeamsXML">>},
            {<<"openid.ext1.if_available">>, <<"dn1,dn2,dn3,teams">>}
        ],
        Params = auth_utils:proplist_to_params(ParamsProplist),

        {ok, <<(plgrid_endpoint())/binary, "?", Params/binary>>}
    catch
        Type:Message ->
            ?error_stacktrace("Cannot get redirect URL for ~p", [?PROVIDER_NAME]),
            {error, {Type, Message}}
    end.


%% validate_login/1
%% ====================================================================
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%% ====================================================================
-spec validate_login([{binary(), binary()}]) ->
    {ok, #oauth_account{}} | {error, term()}.
%% ====================================================================
validate_login(ParamsProplist) ->
    try
        % Make sure received endpoint is really the PLGrid endpoint
        ReceivedEndpoint = proplists:get_value(<<"openid.op_endpoint">>, ParamsProplist),
        true = (plgrid_endpoint() =:= ReceivedEndpoint),

        % 'openid.signed' contains parameters that must be contained in validation request
        Signed = proplists:get_value(<<"openid.signed">>, ParamsProplist),
        SignedArgsNoPrefix = binary:split(Signed, <<",">>, [global]),
        % Add 'openid.' prefix to all parameters
        % And add 'openid.sig' and 'openid.signed' params which are required for validation
        SignedArgs = lists:map(
            fun(X) ->
                <<"openid.", X/binary>>
            end, SignedArgsNoPrefix) ++ [<<"openid.sig">>, <<"openid.signed">>],

        % Gather values for parameters that were signed
        NewParamsProplist = lists:map(
            fun(Key) ->
                Value = case proplists:get_value(Key, ParamsProplist) of
                            undefined -> throw("Value for " ++ gui_str:to_list(Key) ++ " not found");
                            Val -> Val
                        end,
                {Key, Value}
            end, SignedArgs),

        % Create a POST request body
        Params = auth_utils:proplist_to_params(NewParamsProplist),
        RequestBody = <<"openid.mode=check_authentication&", Params/binary>>,

        % Send validation request
        {ok, "200", _, Response} = ibrowse:send_req(
            binary_to_list(ReceivedEndpoint),
            [{content_type, "application/x-www-form-urlencoded"}],
            post, RequestBody, [{response_format, binary}]),

        % Check if server responded positively
        Response = <<"is_valid:true\n">>,

        % Gather user info
        Login = get_signed_param(<<"openid.sreg.nickname">>, ParamsProplist, SignedArgs),
        % Login is also user id, cannot be empty
        true = (Login /= <<"">>),
        Name = get_signed_param(<<"openid.sreg.fullname">>, ParamsProplist, SignedArgs),
        Emails =
            case get_signed_param(<<"openid.sreg.email">>, ParamsProplist, SignedArgs) of
                <<"">> -> [];
                Email -> [Email]
            end,

        % TODO Unused
        _Teams = parse_teams(gui_str:to_list(get_signed_param(<<"openid.ext1.value.teams">>, ParamsProplist, SignedArgs))),
        DN1 = get_signed_param(<<"openid.ext1.value.dn1">>, ParamsProplist, SignedArgs),
        DN2 = get_signed_param(<<"openid.ext1.value.dn2">>, ParamsProplist, SignedArgs),
        DN3 = get_signed_param(<<"openid.ext1.value.dn3">>, ParamsProplist, SignedArgs),
        _DnList = lists:filter(
            fun(X) ->
                (X /= [])
            end, [DN1, DN2, DN3]),

        ProvUserInfo = #oauth_account{
            provider_id = ?PROVIDER_NAME,
            user_id = Login,
            login = Login,
            email_list = Emails,
            name = Name
        },
        {ok, ProvUserInfo}

    catch
        Type:Message ->
            {error, {Type, Message}}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% plgrid_endpoint/0
%% ====================================================================
%% @doc Provider endpoint, where users are redirected for authorization.
%% @end
%% ====================================================================
-spec plgrid_endpoint() -> binary().
%% ====================================================================
plgrid_endpoint() ->
    XRDSEndpoint = proplists:get_value(xrds_endpoint, auth_utils:get_auth_config(?PROVIDER_NAME)),
    {ok, XRDS} = gui_utils:https_get(XRDSEndpoint, [
        {"Accept", "application/xrds+xml;level=1, */*"},
        {"Connection", "close"}
    ]),
    discover_op_endpoint(XRDS).


%% discover_op_endpoint/1
%% ====================================================================
%% @doc
%% Retrieves an XRDS document from given endpoint URL and parses out the URI which will
%% be used for OpenID login redirect.
%% @end
-spec discover_op_endpoint(binary()) -> binary().
%% ====================================================================
discover_op_endpoint(XRDS) ->
    {Xml, _} = xmerl_scan:string(binary_to_list(XRDS)),
    list_to_binary(xml_extract_value("URI", Xml)).


%% xml_extract_value/2
%% ====================================================================
%% @doc
%% Extracts value from under a certain key
%% @end
-spec xml_extract_value(string(), #xmlElement{}) -> string().
%% ====================================================================
xml_extract_value(KeyName, Xml) ->
    [#xmlElement{content = [#xmlText{value = Value} | _]}] = xmerl_xpath:string("//" ++ KeyName, Xml),
    Value.


%% parse_teams/1
%% ====================================================================
%% @doc
%% Parses user's teams from XML to a list of strings. Returns an empty list
%% for empty XML.
%% @end
-spec parse_teams(string()) -> [string()].
%% ====================================================================
parse_teams([]) ->
    [];

parse_teams(XMLContent) ->
    {XML, _} = xmerl_scan:string(XMLContent),
    #xmlElement{content = TeamList} = find_XML_node(teams, XML),
    lists:map(
        fun(#xmlElement{content = [#xmlText{value = Value}]}) ->
            binary_to_list(unicode:characters_to_binary(Value, unicode))
        end, TeamList).


%% find_XML_node/2
%% ====================================================================
%% @doc
%% Finds certain XML node. Assumes that node exists, and checks only
%% the first child of every node going deeper and deeper.
%% @end
-spec find_XML_node(atom(), #xmlElement{}) -> [string()].
%% ====================================================================
find_XML_node(NodeName, #xmlElement{name = NodeName} = XMLElement) ->
    XMLElement;

find_XML_node(NodeName, #xmlElement{} = XMLElement) ->
    [SubNode] = XMLElement#xmlElement.content,
    find_XML_node(NodeName, SubNode);

find_XML_node(_NodeName, _) ->
    undefined.


%% get_signed_param/2
%% ====================================================================
%% @doc
%% Retrieves given request parameter, but only if it was signed by the provider
%% @end
-spec get_signed_param(binary(), [binary()], [binary()]) -> string().
%% ====================================================================
get_signed_param(ParamName, ParamsProplist, SignedParams) ->
    case lists:member(ParamName, SignedParams) of
        true -> proplists:get_value(ParamName, ParamsProplist, <<"">>);
        false -> <<"">>
    end.
