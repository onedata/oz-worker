%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles configuration of SAML Service Provider and
%%% supported Identity Providers.
%%% @end
%%%-------------------------------------------------------------------
-module(saml_config).

-include("auth_common.hrl").
-include("registered_names.hrl").
-include_lib("esaml/include/esaml.hrl").

-export([get_sp_config/0, get_supported_idps/0, get_idp_config/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the config of the Service Provider represented by this onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_sp_config() -> #esaml_sp{}.
get_sp_config() ->
    SAMLConfig = get_config(),
    SPConfig = maps:get(sp_config, SAMLConfig),

    {ok, Hostname} = application:get_env(?APP_NAME, http_domain),
    OzUrl = "https://" ++ Hostname,

    #esaml_sp{
        entity_id = maps:get(entity_id, SPConfig),
        certificate = esaml_util:load_certificate(maps:get(cert_file, SPConfig)),
        key = esaml_util:load_private_key(maps:get(key_file, SPConfig)),
        consume_uri = OzUrl ++ ?SAML_CONSUME_ENDPOINT,
        metadata_uri = OzUrl ++ ?SAML_METADATA_ENDPOINT,
        org = #esaml_org{
            name = maps:get(organization_name, SPConfig),
            displayname = maps:get(organization_display_name, SPConfig),
            url = OzUrl
        },
        tech = #esaml_contact{
            name = maps:get(tech_contact_name, SPConfig),
            email = maps:get(tech_contact_email, SPConfig)
        },
        sign_requests = maps:get(sign_requests, SPConfig),
        sign_metadata = maps:get(sign_metadata, SPConfig),
        want_assertions_signed = maps:get(want_assertions_signed, SPConfig)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all supported IdPs (their ids).
%% @end
%%--------------------------------------------------------------------
-spec get_supported_idps() -> [atom()].
get_supported_idps() ->
    SAMLConfig = get_config(),
    maps:keys(maps:get(supported_idps, SAMLConfig)).


%%--------------------------------------------------------------------
%% @doc
%% Returns IdP config based on its id.
%% @end
%%--------------------------------------------------------------------
-spec get_idp_config(IdPId :: atom()) -> #esaml_idp{}.
get_idp_config(IdPId) ->
    SAMLConfig = get_config(),
    SupportedIdPs = maps:get(supported_idps, SAMLConfig),
    IdPConfig = maps:get(IdPId, SupportedIdPs),
    #esaml_idp{
        metadata = esaml_util:load_metadata(maps:get(metadata_url, IdPConfig)),
        trusted_fingerprints = esaml_util:convert_fingerprints(maps:get(trusted_fingerprints, IdPConfig)),
        encrypts_assertions = maps:get(encrypts_assertions, IdPConfig),
        signs_assertions = maps:get(signs_assertions, IdPConfig),
        signs_envelopes = maps:get(signs_envelopes, IdPConfig),
        signs_logout_requests = false, % TODO Logout currently not supported
        attribute_mapping = maps:get(attribute_mapping, IdPConfig)
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads SAML config from file and returns it as a map.
%% TODO consider caching the config file
%% @end
%%--------------------------------------------------------------------
-spec get_config() -> maps:map().
get_config() ->
    {ok, SAMLConfigFile} = application:get_env(?APP_NAME, saml_config_file),
    {ok, [SAMLConfig]} = file:consult(SAMLConfigFile),
    SAMLConfig.