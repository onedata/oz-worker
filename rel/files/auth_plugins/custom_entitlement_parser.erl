%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module can be used to implement custom IdP entitlement parsing per IdP.
%%% Please refer to Onedata documentation to learn the entitlement format used
%%% in onedata. Custom parsers must return results in this format.
%%% https://onedata.org/#/home/documentation/doc/administering_onedata/openid_saml_configuration[custom-entitlement-parsers-advanced].html
%%%
%%% validation_examples/0 callback can be used to provide examples to be
%%% evaluated upon the start of Onezone to make sure that parser logic works
%%% as expected.
%%%
%%% Whenever a parser call crashes, stacktrace is written to the debug log and
%%% {error, malformed} is returned, which results in the entitlement being
%%% discarded.
%%% @end
%%%-------------------------------------------------------------------
-module(custom_entitlement_parser).
-behavior(auth_plugin_behaviour).
-behavior(entitlement_parser_behaviour).

-include("auth/entitlement_mapping.hrl").

%% API
-export([type/0]).
-export([parse/3]).
-export([validation_examples/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the type of this plugin. Depending on the type, the plugin must
%% implement certain behaviour:
%%      entitlement_parser -> entitlement_parser_behaviour
%%      openid_plugin -> openid_plugin_behaviour
%%      attribute_mapper -> attribute_mapper_behaviour
%% @end
%%--------------------------------------------------------------------
-callback type() -> entitlement_parser.
type() ->
    entitlement_parser.


%%--------------------------------------------------------------------
%% @doc
%% Parses an entitlement coming from given IdP into internal Onedata format.
%% @end
%%--------------------------------------------------------------------
-spec parse(auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().
parse(egi, Entitlement, _ParserConfig) ->
    parse_egi_entitlement(Entitlement).


%%--------------------------------------------------------------------
%% @doc
%% Returns entitlement mapping validation examples to be evaluated during startup.
%% @end
%%--------------------------------------------------------------------
-spec validation_examples() ->
    [{auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config(),
        entitlement_mapping:idp_entitlement() | {error, malformed}}].
validation_examples() ->
    lists:flatten(
        [{egi, Input, #{}, Output} || {Input, Output} <- egi_validation_examples()]
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a string that represents user's group membership for EGI.
%%
%% Group format:
%%      urn:mace:egi.eu:group:<VO>[[:<GROUP>][:<SUBGROUP>*]][:role=<ROLE>]#<GROUP-AUTHORITY>
%% where:
%%      <VO> is the name of the Virtual Organisation
%%      <GROUP> is the name of a group in the identified <VO>;
%%          specifying a group is optional
%%      zero or more <SUBGROUP> components represent the hierarchy of subgroups
%%          in the <GROUP>; specifying sub-groups is optional
%%      the optional <ROLE> component is scoped to the rightmost (sub)group;
%%          if no group information is specified, the role applies to the VO
%%      <GROUP-AUTHORITY> is a non-empty string that indicates the authoritative
%%          source for the entitlement value. For example, it can be the FQDN of
%%          the group management system that is responsible for the identified
%%          group membership information
%% @end
%%--------------------------------------------------------------------
-spec parse_egi_entitlement(entitlement_mapping:raw_entitlement()) -> entitlement_mapping:idp_entitlement().
parse_egi_entitlement(<<"urn:mace:egi.eu:group:", Group/binary>>) ->
    % Strip out the prefix standard for EGI

    [GroupStructureEncoded, _GroupAuthority] = binary:split(Group, <<"#">>),
    % Replace plus sings with spaces
    GroupStructure = binary:replace(GroupStructureEncoded, <<"+">>, <<" ">>, [global]),
    GroupTokens = binary:split(GroupStructure, <<":">>, [global, trim_all]),

    {Groups, RoleStr} = case lists:last(GroupTokens) of
        <<"role=", Role/binary>> ->
            {lists:sublist(GroupTokens, length(GroupTokens) - 1), Role};
        _ ->
            {GroupTokens, undefined}
    end,

    UserPrivileges = case RoleStr of
        <<"member">> -> member;
        <<"manager">> -> manager;
        <<"admin">> -> admin;
        <<"chair">> -> admin;
        _ -> member
    end,

    #idp_entitlement{
        idp = egi,
        path = [#idp_group{type = team, name = G, privileges = member} || G <- Groups],
        privileges = UserPrivileges
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns entitlement mapping validation examples for EGI.
%% @end
%%--------------------------------------------------------------------
-spec egi_validation_examples() ->
    [{entitlement_mapping:raw_entitlement(), entitlement_mapping:idp_entitlement() | {error, malformed}}].
egi_validation_examples() -> [
    {
        <<"urn:mace:egi.eu:group:fedcloud.egi.eu:role=vm_operator#aai.egi.eu">>,
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = team, name = <<"fedcloud.egi.eu">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"unconfromant-group-name">>,
        {error, malformed}
    }
].

