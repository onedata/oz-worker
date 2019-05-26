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
-behavior(onezone_plugin_behaviour).
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
parse(egi, Entitlement, ParserConfig) ->
    parse_egi_entitlement(Entitlement, ParserConfig);
parse(plgrid, Entitlement, ParserConfig) ->
    parse_plgrid_entitlement(Entitlement, ParserConfig).


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
        [{egi, Input, ParserConfig, Output} || {Input, ParserConfig, Output} <- egi_validation_examples()],
        [{plgrid, Input, ParserConfig, Output} || {Input, ParserConfig, Output} <- plgrid_validation_examples()]
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns an #idp_entitlement{} that represents user's group membership for EGI.
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
-spec parse_egi_entitlement(entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().
parse_egi_entitlement(<<"urn:mace:egi.eu:group:", Group/binary>>, ParserConfig) ->
    % Strip out the prefix standard for EGI

    OriginGroupType = maps:get(originGroupType, ParserConfig, organization),
    TopGroupType = maps:get(topGroupType, ParserConfig, team),
    SubGroupsType = maps:get(subGroupsType, ParserConfig, team),

    [GroupStructureEncoded, Origin] = binary:split(Group, <<"#">>),
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
        <<"owner">> -> admin;
        _ -> member
    end,

    Path = lists:flatten([
        #idp_group{type = OriginGroupType, name = Origin},
        #idp_group{type = TopGroupType, name = hd(Groups)},
        [#idp_group{type = SubGroupsType, name = G, privileges = member} || G <- tl(Groups)]
    ]),

    #idp_entitlement{
        idp = egi,
        path = Path,
        privileges = UserPrivileges
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns an #idp_entitlement{} that represents user's group membership for PlGrid.
%%
%% Group format:
%%      group-short-name(Group long name)
%% Examples:
%%      plgg-group-1(plgg-group-1)
%%      plgg-admins(PlGrid admin group)
%%
%% Long names are not always specified, in this case the short name is repeated.
%% This parser adds a space before the parenthesis for better readability, or
%% removes the part in parenthesis completely if the long name is a duplicated or
%% a substring of the short name.
%% @end
%%--------------------------------------------------------------------
-spec parse_plgrid_entitlement(entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().
parse_plgrid_entitlement(RawEntitlement, ParserConfig) ->
    GroupType = maps:get(groupType, ParserConfig, team),
    GroupName = case binary:split(RawEntitlement, [<<"(">>, <<")">>], [global, trim_all]) of
        [Name] ->
            Name;
        [ShortName, LongName] ->
            case binary:match(ShortName, LongName) of
                nomatch -> <<ShortName/binary, " (", LongName/binary, ")">>;
                _ -> ShortName
            end
    end,

    #idp_entitlement{
        idp = plgrid,
        path = [#idp_group{type = GroupType, name = GroupName}],
        privileges = member
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns entitlement mapping validation examples for EGI.
%% @end
%%--------------------------------------------------------------------
-spec egi_validation_examples() ->
    [{entitlement_mapping:raw_entitlement(), auth_config:parser_config(), entitlement_mapping:idp_entitlement() | {error, malformed}}].
egi_validation_examples() -> [
    {
        <<"urn:mace:egi.eu:group:fedcloud.egi.eu:role=vm_operator#aai.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"fedcloud.egi.eu">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"urn:mace:egi.eu:group:fedcloud.egi.eu:child:role=member#sso.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"sso.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"fedcloud.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"child">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"urn:mace:egi.eu:group:fedcloud.egi.eu:child:role=owner#aai.egi.eu">>,
        #{
            originGroupType => unit,
            topGroupType => team,
            subGroupsType => role_holders
        },
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = unit, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"fedcloud.egi.eu">>, privileges = member},
            #idp_group{type = role_holders, name = <<"child">>, privileges = member}
        ], privileges = admin}
    },
    {
        <<"urn:mace:egi.eu:group:egi-engage-members:role=manager#sso.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"sso.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"egi-engage-members">>, privileges = member}
        ], privileges = manager}
    },
    {
        <<"urn:mace:egi.eu:group:egi-engage-members:role=admin#aai.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"egi-engage-members">>, privileges = member}
        ], privileges = admin}
    },
    {
        <<"urn:mace:egi.eu:group:egi-engage-members:role=chair#other.origin.com">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"other.origin.com">>, privileges = member},
            #idp_group{type = team, name = <<"egi-engage-members">>, privileges = member}
        ], privileges = admin}
    },
    {
        <<"urn:mace:egi.eu:bad-prefix:egi-engage-members:role=chair#other.origin.com">>,
        #{},
        {error, malformed}
    },
    {
        <<"urn:mace:egi.eu:group:group-without-origin">>,
        #{},
        {error, malformed}
    },
    {
        <<"unconfromant-group-name">>,
        #{},
        {error, malformed}
    }
].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns entitlement mapping validation examples for PlGrid.
%% @end
%%--------------------------------------------------------------------
-spec plgrid_validation_examples() ->
    [{entitlement_mapping:raw_entitlement(), auth_config:parser_config(), entitlement_mapping:idp_entitlement() | {error, malformed}}].
plgrid_validation_examples() -> [
    {
        <<"plgg-group-1(plgg-group-1)">>,
        #{},
        #idp_entitlement{idp = plgrid, path = [
            #idp_group{type = team, name = <<"plgg-group-1">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"plgg-group-1(plgg-group-1)">>,
        #{groupType => unit},
        #idp_entitlement{idp = plgrid, path = [
            #idp_group{type = unit, name = <<"plgg-group-1">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"plgg-team-alpha-research(plgg-team-alpha)">>,
        #{},
        #idp_entitlement{idp = plgrid, path = [
            #idp_group{type = team, name = <<"plgg-team-alpha-research">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"plgg-team-alpha-research(plgg-team-alpha)">>,
        #{groupType => unit},
        #idp_entitlement{idp = plgrid, path = [
            #idp_group{type = unit, name = <<"plgg-team-alpha-research">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"plgg-admin-group(Longer description)">>,
        #{},
        #idp_entitlement{idp = plgrid, path = [
            #idp_group{type = team, name = <<"plgg-admin-group (Longer description)">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"plgg-admin-group(Longer description)">>,
        #{groupType => role_holders},
        #idp_entitlement{idp = plgrid, path = [
            #idp_group{type = role_holders, name = <<"plgg-admin-group (Longer description)">>, privileges = member}
        ], privileges = member}
    }
].

