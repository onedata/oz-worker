%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when configuration page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_configuration).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("registered_names.hrl").

-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(new_gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"GET">>, Req) ->
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        json_utils:encode(get_config()),
        Req
    ).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_config() -> maps:map().
get_config() ->
    CompatibleOpVersions = oz_worker:get_env(compatible_op_versions, []),
    CompatibleOpVersionsBin = [list_to_binary(V) || V <- CompatibleOpVersions],
    SubdomainDelegationEnabled = oz_worker:get_env(subdomain_delegation_enabled, true),
    ProviderRegistrationPolicy = oz_worker:get_env(provider_registration_policy, open),
    #{
        <<"name">> => oz_worker:get_name(),
        <<"version">> => oz_worker:get_version(),
        <<"build">> => oz_worker:get_build_version(),
        <<"compatibleOneproviderVersions">> => CompatibleOpVersionsBin,
        <<"subdomainDelegationEnabled">> => SubdomainDelegationEnabled,
        <<"providerRegistrationPolicy">> => ProviderRegistrationPolicy
    }.
