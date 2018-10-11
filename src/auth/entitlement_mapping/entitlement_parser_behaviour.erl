%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies the API for an auth plugin that maps IdP
%%% entitlements into Onedata entitlements (group memberships).
%%% @end
%%%-------------------------------------------------------------------
-module(entitlement_parser_behaviour).


%%--------------------------------------------------------------------
%% @doc
%% Parses an entitlement coming from given IdP into internal Onedata format.
%% @end
%%--------------------------------------------------------------------
-callback parse(auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().


%%--------------------------------------------------------------------
%% @doc
%% Returns entitlement parsing validation examples to be evaluated during startup.
%% @end
%%--------------------------------------------------------------------
-callback validation_examples() ->
    [{auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config(),
    entitlement_mapping:idp_entitlement() | {error, malformed}}].


-optional_callbacks([validation_examples/0]).
