%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module parses IdP groups into internal Onedata group format.
%%% Each group is mapped into a chain of nested groups in Onedata.
%%% See more in idp_groups module.
%%% @end
%%%-------------------------------------------------------------------
-module(nested_entitlement_parser).
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
    SplitWith = str_utils:unicode_list_to_binary(maps:get(splitWith, ParserConfig, "/")),
    UserPrivileges = maps:get(userPrivileges, ParserConfig, member),

    TopGroupType = maps:get(topGroupType, ParserConfig, team),
    TopGroupPrivilegesInVo = maps:get(topGroupPrivilegesInVo, ParserConfig, member),

    SubGroupsType = maps:get(subGroupsType, ParserConfig, team),
    SubGroupsPrivilegesInParent = maps:get(subGroupsPrivilegesInParent, ParserConfig, member),

    [TopGroup | SubGroups] = binary:split(Entitlement, SplitWith, [global, trim_all]),
    ParsedTopGroup = idp_group(TopGroupType, TopGroup, TopGroupPrivilegesInVo),
    ParsedSubGroups = [idp_group(SubGroupsType, G, SubGroupsPrivilegesInParent) || G <- SubGroups],

    #idp_entitlement{
        idp = IdP,
        path = [ParsedTopGroup | ParsedSubGroups],
        privileges = UserPrivileges
    }.


%% @private
-spec idp_group(od_group:type(), od_group:name(), entitlement_mapping:privileges()) ->
    entitlement_mapping:idp_group().
idp_group(Type, Name, PrivilegesInParent) ->
    #idp_group{type = Type, name = Name, privileges = PrivilegesInParent}.
