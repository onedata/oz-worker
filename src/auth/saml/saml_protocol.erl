%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin implementing auth_protocol_behaviour, used to handle authentication
%%% over SAML protocol. These callbacks are called by auth_logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(saml_protocol).
-behavior(auth_protocol_behaviour).
-author("Lukasz Opiola").

-include("auth/auth_common.hrl").
-include("auth/auth_errors.hrl").
-include_lib("esaml/include/esaml.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

%% auth_protocol_behaviour callbacks
-export([get_login_endpoint/2, validate_login/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link auth_protocol_behaviour} callback get_login_endpoint/2.
%% @end
%%--------------------------------------------------------------------
-spec get_login_endpoint(auth_config:idp(), state_token:state_token()) ->
    {ok, #{binary() => binary() | null}}.
get_login_endpoint(IdP, State) ->
    {ok, SPConfig} = auth_config:get_saml_sp_config(),
    IdpConfig = auth_config:get_saml_idp_config(IdP),
    LoginLocation = case IdpConfig#esaml_idp.preferred_sso_binding of
        http_redirect ->
            IdpConfig#esaml_idp.metadata#esaml_idp_metadata.redirect_login_location;
        http_post ->
            IdpConfig#esaml_idp.metadata#esaml_idp_metadata.post_login_location
    end,
    case LoginLocation of
        undefined ->
            ?alert("Cannot resolve login location for IdP '~p'", [IdP]),
            throw(?ERROR_INTERNAL_SERVER_ERROR);
        _ ->
            ok
    end,
    AuthNReq = esaml_sp:generate_authn_request(LoginLocation, SPConfig),
    case IdpConfig#esaml_idp.preferred_sso_binding of
        http_redirect ->
            Url = esaml_binding:encode_http_redirect(LoginLocation, AuthNReq, State),
            {ok, #{
                <<"method">> => <<"get">>,
                <<"url">> => Url,
                <<"formData">> => null
            }};
        http_post ->
            FormData = esaml_binding:encode_http_post_form_data(AuthNReq, State),
            {ok, #{
                <<"method">> => <<"post">>,
                <<"url">> => iolist_to_binary(LoginLocation),
                <<"formData">> => FormData
            }}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link auth_protocol_behaviour} callback validate_login/2.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_config:idp(), auth_logic:query_params()) ->
    {ok, attribute_mapping:idp_attributes()} | {error, term()}.
validate_login(IdP, QueryParams) ->
    {ok, SPConfig} = auth_config:get_saml_sp_config(),
    IdPConfig = auth_config:get_saml_idp_config(IdP),

    % Check url params for state parameter and validate it
    SAMLEncoding = maps:get(<<"SAMLEncoding">>, QueryParams, undefined),
    SAMLResponse = maps:get(<<"SAMLResponse">>, QueryParams),

    case (catch esaml_binding:decode_response(SAMLEncoding, SAMLResponse)) of
        {'EXIT', _} ->
            throw(?ERROR_INVALID_AUTH_REQUEST);
        Xml ->
            case esaml_sp:validate_assertion(Xml, SPConfig, IdPConfig) of
                {ok, #esaml_assertion{attributes = Attributes}} ->
                    {ok, normalize_attributes(Attributes)};
                {error, Reason} ->
                    ?auth_warning("Invalid login request via SAML. Reason:~p~nPOST params:~n~p",
                        [Reason, QueryParams]),
                    throw(?ERROR_INVALID_AUTH_REQUEST)
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec normalize_attributes(proplists:proplist()) -> #{}.
normalize_attributes(Attributes) ->
    maps:from_list(lists:map(fun({Key, Value}) ->
        {normalize_key(Key), normalize_value(Value)}
    end, Attributes)).


%% @private
-spec normalize_key(term()) -> term().
normalize_key(List) when is_list(List) ->
    list_to_binary(List);
normalize_key(Key) ->
    Key.


%% @private
-spec normalize_value(term()) -> term().
normalize_value(List = [Str | _]) when is_list(Str) ->
    [str_utils:unicode_list_to_binary(E) || E <- List];
normalize_value(Str) when is_list(Str) ->
    str_utils:unicode_list_to_binary(Str);
normalize_value(Value) ->
    Value.
