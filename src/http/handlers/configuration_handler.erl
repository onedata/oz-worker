%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles requests asking for current configuration of Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(configuration_handler).
-author("Bartosz Walkowicz").

-behaviour(cowboy_handler).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([init/2]).


%%--------------------------------------------------------------------
%% @doc Cowboy handler callback.
%% Handles a request returning current configuration of Onezone.
%% @end
%%--------------------------------------------------------------------
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(#{method := <<"GET">>} = Req, State) ->
    Configuration = json_utils:encode_map(get_config()),
    NewReq = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>}, Configuration, Req
    ),
    {ok, NewReq, State};
init(Req, State) ->
    NewReq = cowboy_req:reply(405, #{<<"allow">> => <<"GET">>}, Req),
    {ok, NewReq, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec get_config() -> #{}.
get_config() ->
    SubdomainDelegationEnabled = application:get_env(
        ?APP_NAME, subdomain_delegation_enabled, true
    ),
    ProviderRegistrationPolicy = application:get_env(
        ?APP_NAME, provider_registration_policy, open
    ),
    #{
        <<"subdomainDelegationEnabled">> => SubdomainDelegationEnabled,
        <<"providerRegistrationPolicy">> => ProviderRegistrationPolicy
    }.
