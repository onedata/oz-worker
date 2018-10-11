%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module parses IdP groups into internal Onedata group format.
%%% Each group is mapped into one group in Onedata (flat structure).
%%% See more in idp_groups module.
%%% @end
%%%-------------------------------------------------------------------
-module(flat_entitlement_parser).
-behavior(entitlement_parser_behaviour).

-include("auth/entitlement_mapping.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([parse/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link entitlement_parser_behaviour} callback parse/3.
%% @end
%%--------------------------------------------------------------------
-spec parse(auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().
parse(IdP, Entitlement, ParserConfig) ->
    Type = maps:get(groupType, ParserConfig, team),
    GroupPrivilegesInVo = maps:get(groupPrivilegesInVo, ParserConfig, member),
    UserPrivileges = maps:get(userPrivileges, ParserConfig, member),
    #idp_entitlement{idp = IdP, path = [
        #idp_group{type = Type, name = Entitlement, privileges = GroupPrivilegesInVo}
    ], privileges = UserPrivileges}.
