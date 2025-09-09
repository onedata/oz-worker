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
%%% https://onedata.org/#/home/documentation/topic/latest/oidc-saml-custom-entitlement-parsers
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


% The top group for all the groups coming from EGI IdP.
-define(EGI_ORIGIN_GROUP, <<"aai.egi.eu">>).


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
    parse_plgrid_entitlement(Entitlement, ParserConfig);
parse(pracelab, Entitlement, ParserConfig) ->
    parse_pracelab_entitlement(Entitlement, ParserConfig).


%%--------------------------------------------------------------------
%% @doc
%% Returns entitlement mapping validation examples to be evaluated during startup.
%% @end
%%--------------------------------------------------------------------
-spec validation_examples() ->
    [{auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config(),
        entitlement_mapping:idp_entitlement() | {error, malformed}}].
validation_examples() ->
    lists:flatten([
        [{egi, Input, ParserConfig, Output} || {Input, ParserConfig, Output} <- egi_validation_examples()],
        [{plgrid, Input, ParserConfig, Output} || {Input, ParserConfig, Output} <- plgrid_validation_examples()],
        [{pracelab, Input, ParserConfig, Output} || {Input, ParserConfig, Output} <- pracelab_validation_examples()]
    ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns an #idp_entitlement{} that represents user's group membership for EGI.
%%
%% The mapping is performed with compliance to the AARC-G069 guidelines. The information
%% in this doc is extracted from the guidelines for convenience.
%%      https://aarc-community.org/guidelines/aarc-g069
%%
%% Group format:
%%      <NAMESPACE>:group:<GROUP>[:<SUBGROUP>*][:role=<ROLE>][#<AUTHORITY>]
%% where:
%%      <NAMESPACE> is the namespace identifier associated with a URN namespace
%%           registered with IANA, as per [RFC8141], ensuring global uniqueness.
%%
%%      The literal string "group" indicates a value expressing group membership information.
%%
%%      <GROUP> is the name of a Virtual Organisation (VO), research collaboration or a top
%%           level arbitrary group. Group names MUST be unique within a given namespace.
%%
%%      An optional list of <SUBGROUP> components represents the hierarchy of subgroups in
%%           the <GROUP>.
%%
%%      The optional <ROLE> component is scoped to the rightmost (sub)group; if no subgroup
%%           information is specified, the role applies to the top level group/VO.
%%
%%      The <AUTHORITY> can be specified in the optional f-component of the URN.
%%          Specifying the <AUTHORITY> is deprecated by this specification and may be removed
%%          in a future revision. When specified, the <AUTHORITY> MUST be a non-empty string
%%          introduced by the number sign ("#") character and terminated by the end of the URN.
%%
%% IMPORTANT NOTES:
%%     * Currently, only groups with namespace "urn:mace:egi.eu" are accepted.
%%     * Currently, all groups from EGI are plugged as children of the ?EGI_ORIGIN_GROUP,
%%       although this group is not transmitted in the entitlements (it's added artificially).
%%     * Group names are expected to be URL-encoded as specified in the AARC-G069 guidelines.
%%     * The "#<AUTHORITY>" suffix is ignored, present or not, to account for its
%%       deprecation and prepare for its removal in the future.
%% @end
%%--------------------------------------------------------------------
-spec parse_egi_entitlement(entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().
parse_egi_entitlement(<<"urn:mace:egi.eu:group:", Group/binary>>, ParserConfig) ->
    % strip out the prefix standard for EGI - currently the only accepted namespace

    % As of AARC-G069 Guidelines, the "#<AUTHORITY>" suffix is deprecated and
    % should be ignored.
    GroupStructureEncoded = hd(binary:split(Group, <<"#">>)),

    GroupTokensEncoded = binary:split(GroupStructureEncoded, <<":">>, [global, trim_all]),
    GroupTokens = lists:map(fun http_utils:url_decode/1, GroupTokensEncoded),

    {GroupNames, RoleStr} = case lists:last(GroupTokens) of
        <<"role=", Role/binary>> ->
            {lists:sublist(GroupTokens, length(GroupTokens) - 1), Role};
        _ ->
            {GroupTokens, undefined}
    end,

    TopGroupName = hd(GroupNames),
    SubGroupNames = tl(GroupNames),

    OriginGroupType = maps:get(originGroupType, ParserConfig, organization),
    TopGroupType = maps:get(topGroupType, ParserConfig, team),
    TopGroupPrivileges = maps:get(topGroupPrivileges, ParserConfig, none),
    SubGroupsType = maps:get(subGroupsType, ParserConfig, team),

    Path = lists:flatten([
        #idp_group{type = OriginGroupType, name = ?EGI_ORIGIN_GROUP},
        #idp_group{type = TopGroupType, name = TopGroupName, privileges = TopGroupPrivileges},
        [#idp_group{type = SubGroupsType, name = SubGroupName} || SubGroupName <- SubGroupNames]
    ]),

    #idp_entitlement{
        idp = egi,
        path = Path,
        privileges = case RoleStr of
            <<"member">> -> member;
            <<"manager">> -> manager;
            <<"admin">> -> admin;
            <<"chair">> -> admin;
            <<"owner">> -> admin;
            _ -> member
        end
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
%% Returns an #idp_entitlement{} that represents user's group membership for PRACE-Lab project.
%%
%% Group format:
%%      ^/PL\d{4,}[AEFO]?$
%%          or
%%      ^/PL\d{4,}-\d+[AEFO]?$
%% Examples:
%%      /PL4500
%%      /PL4500A
%%      /PL4500O
%%      /PL4500E
%%      /PL4500F
%%      /PL450091
%%      /PL450091E
%%      /PL4500-1
%%      /PL4500-1E
%%      /PL4500-1A
%%      /PL4500-1O
%%      /PL4500-1F
%%      /PL450091-1
%%      /PL450091-1F
%%
%% The slash at the beginning should be ignored.
%%
%% Letters A, E, F, O represent roles in PRACE-Lab spaces. They are not a part of the
%% group name (e.g. entitlement "PL4500F" should map to group "PL4500" and "PL4500-1E" to "PL4500-1".
%%
%% The letter "E" meas that the user is entitled to use the service represented by the group.
%% Hence, only the entitlements ending with "E" should be considered and mapped to "member" privileges;
%% others should be discarded.
%% @end
%%--------------------------------------------------------------------
-spec parse_pracelab_entitlement(entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().
parse_pracelab_entitlement(RawEntitlement, ParserConfig) ->
    $E = binary:last(RawEntitlement),
    match = re:run(RawEntitlement, "^\\/PL\\d{4,}([AEOF]|(\\-\\d+[AEOF]?))?$", [{capture, none}, unicode, ucp]),
    GroupType = maps:get(groupType, ParserConfig, team),
    GroupName = binary:part(RawEntitlement, 1, byte_size(RawEntitlement) - 2),
    #idp_entitlement{
        idp = pracelab,
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
            #idp_group{type = team, name = <<"fedcloud.egi.eu">>, privileges = none}
        ], privileges = member}
    },
    {
        <<"urn:mace:egi.eu:group:fedcloud.egi.eu:child:role=manager#aai.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"fedcloud.egi.eu">>, privileges = none},
            #idp_group{type = team, name = <<"child">>, privileges = member}
        ], privileges = manager}
    },
    {
        <<"urn:mace:egi.eu:group:fedcloud.egi.eu:members:role=owner#aai.egi.eu">>,
        #{
            originGroupType => unit,
            topGroupType => team,
            topGroupPrivileges => manager,
            subGroupsType => role_holders
        },
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = unit, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"fedcloud.egi.eu">>, privileges = manager},
            #idp_group{type = role_holders, name = <<"members">>, privileges = member}
        ], privileges = admin}
    },
    {
        <<"urn:mace:egi.eu:group:egi-engage-members:role=admin#aai.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"egi-engage-members">>, privileges = none}
        ], privileges = admin}
    },
    {
        % the #<AUTHORITY> bit is ignored so this should be equivalent to the above
        <<"urn:mace:egi.eu:group:egi-engage-members:role=admin#sso.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"egi-engage-members">>, privileges = none}
        ], privileges = admin}
    },
    {
        % the #<AUTHORITY> bit is ignored so this should be equivalent to the above
        <<"urn:mace:egi.eu:group:egi-engage-members:role=admin">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"egi-engage-members">>, privileges = none}
        ], privileges = admin}
    },
    {
        <<"urn:mace:egi.eu:group:vo.access.egi.eu:admins:role=owner#aai.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"vo.access.egi.eu">>, privileges = none},
            #idp_group{type = team, name = <<"admins">>, privileges = member}
        ], privileges = admin}
    },
    {
        <<"urn:mace:egi.eu:group:vo.access.egi.eu:subgroup:admins:role=manager#aai.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"vo.access.egi.eu">>, privileges = none},
            #idp_group{type = team, name = <<"subgroup">>, privileges = member},
            #idp_group{type = team, name = <<"admins">>, privileges = member}
        ], privileges = manager}
    },
    {
        <<"urn:mace:egi.eu:group:vo.access.egi.eu:subgroup:admins:uberadmins:role=chair#aai.egi.eu">>,
        #{
            topGroupPrivileges => member
        },
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"vo.access.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"subgroup">>, privileges = member},
            #idp_group{type = team, name = <<"admins">>, privileges = member},
            #idp_group{type = team, name = <<"uberadmins">>, privileges = member}
        ], privileges = admin}
    },
    {
        <<"urn:mace:egi.eu:group:parent:Minun%20Ryhm%c3%A4ni:za%C5%BC%C3%B3%C5%82%C4%87%20%5B%5C%22%27%3B%3A%2C.%3C%3E%21%40%23%24%25%5E%26%2A%28%29%60%7B%7C%7D%5D#aai.egi.eu">>,
        #{},
        #idp_entitlement{idp = egi, path = [
            #idp_group{type = organization, name = <<"aai.egi.eu">>, privileges = member},
            #idp_group{type = team, name = <<"parent">>, privileges = none},
            #idp_group{type = team, name = <<"Minun Ryhmäni"/utf8>>, privileges = member},
            #idp_group{type = team, name = <<"zażółć [\\\"';:,.<>!@#$%^&*()`{|}]"/utf8>>, privileges = member}
        ], privileges = member}
    },
    {
        <<"urn:mace:egi.eu:bad-prefix:egi-engage-members:role=chair#other.origin.com">>,
        #{},
        {error, malformed}
    },
    {
        <<"unconformant-group-name">>,
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns entitlement mapping validation examples for PRACE-Lab.
%% @end
%%--------------------------------------------------------------------
-spec pracelab_validation_examples() ->
    [{entitlement_mapping:raw_entitlement(), auth_config:parser_config(), entitlement_mapping:idp_entitlement() | {error, malformed}}].
pracelab_validation_examples() -> [
    {
        <<"fdglk;ajg987ayga9g">>,
        #{},
        {error, malformed}
    },
    {
        <<"PL4500">>,
        #{},
        {error, malformed}
    },
    {
        <<"/PL4500">>,
        #{},
        {error, malformed}
    },
    {
        <<"/PL45009">>,
        #{},
        {error, malformed}
    },
    {
        <<"/PL450091">>,
        #{},
        {error, malformed}
    },
    {
        <<"/PL4500F">>,
        #{groupType => unit},
        {error, malformed}
    },
    {
        <<"/PL4500E">>,
        #{},
        #idp_entitlement{idp = pracelab, path = [
            #idp_group{type = team, name = <<"PL4500">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"/PL45009E">>,
        #{},
        #idp_entitlement{idp = pracelab, path = [
            #idp_group{type = team, name = <<"PL45009">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"/PL450091E">>,
        #{groupType => organization},
        #idp_entitlement{idp = pracelab, path = [
            #idp_group{type = organization, name = <<"PL450091">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"/PL4500-7E">>,
        #{groupType => unit},
        #idp_entitlement{idp = pracelab, path = [
            #idp_group{type = unit, name = <<"PL4500-7">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"/PL450091-18E">>,
        #{groupType => role_holders},
        #idp_entitlement{idp = pracelab, path = [
            #idp_group{type = role_holders, name = <<"PL450091-18">>, privileges = member}
        ], privileges = member}
    },
    {
        <<"/PL4500-18">>,
        #{groupType => role_holders},
        {error, malformed}
    },
    {
        <<"/PL4500-18F">>,
        #{groupType => role_holders},
        {error, malformed}
    },
    {
        <<"/PL4500F-E">>,
        #{groupType => unit},
        {error, malformed}
    },
    {
        <<"/PL4500F-8E">>,
        #{groupType => unit},
        {error, malformed}
    },
    {
        <<"/PL4500-8E8">>,
        #{groupType => unit},
        {error, malformed}
    }
].

