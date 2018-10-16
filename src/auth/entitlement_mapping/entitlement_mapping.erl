%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Entitlement mapping is used to automatically map users entitlements in an
%%% IdP to group memberships in Onedata. Entitlements can be understood as the
%%% right to be a member of a group (or, possibly, a group structure) with
%%% certain privileges. The section in auth.config concerning the Entitlement
%%% Mapping has the following structure (example):
%%%
%%%     entitlementMapping => #{
%%%         enabled => true,
%%%         voGroupName => undefined,
%%%         adminGroup => undefined,
%%%         parser => nested_entitlement_parser,
%%%         parserConfig => #{
%%%             topGroupType => unit,
%%%             topGroupPrivilegesInVo => member,
%%%             subGroupsType => team,
%%%             subGroupsPrivilegesInParent => member,
%%%             userPrivileges => member
%%%         }
%%%     }
%%%
%%%       enabled - enable / disable entitlement mapping for given IdP
%%%   voGroupName - if specified, a special VO group will be created and all
%%%                  groups originating from given IdP will be added as members
%%%                  (subgroups) to it.
%%%    adminGroup - if specified, defines a Admin Group for given IdP. It
%%%                 should be an existing entitlement, in the format that is
%%%                 the same as the output of your entitlement mapping rules
%%%                 (see the example below). The Admin Group will be given admin
%%%                 privileges to all groups originating from given IdP.
%%%                 Consequently, all members of the Admin Group will
%%%                 inherit admin privileges to all groups from the IdP. For
%%%                 example, if there is an "admins" group in an IdP, and its
%%%                 specified here, users of the original "admins" group will be
%%%                 mapped to Onedata "admins" group and will automatically gain
%%%                 admin privileges in the IdP groups.
%%%   parser & parserConfig - see below
%%%
%%% Entitlement parsers are used to convert user entitlements from an IdP to
%%% group memberships in Onedata. There are two predefined parsers that can be
%%% used to covers most common use cases:
%%%
%%%     flat_idp_group_parser - converts every entitlement to one group
%%%         membership in Onedata. Each of created groups has configured type
%%%         (groupType) and the user with such entitlement is added to the group
%%%         with given set of privileges (userPrivileges). The name of the group
%%%         is the same as in the entitlement, but normalized to allowed chars.
%%%         If the IdP has a specified voGroupName, the group will be added as
%%%         a subgroup with given set of privileges (groupPrivilegesInVo),
%%%         otherwise this parameter has no effect.
%%%
%%%         Example: having the following entitlementMapping config:
%%%             entitlementMapping => #{
%%%                 enabled => true,
%%%                 voGroupName => "my-organization",
%%%                 adminGroup => undefined,
%%%                 parser => flat_entitlement_parser,
%%%                 parserConfig => #{
%%%                     groupType => team,
%%%                     groupPrivilegesInVo => member,
%%%                     userPrivileges => manager
%%%                 }
%%%             }
%%%         Consider the following JSON received from the IdP:
%%%             {
%%%                 "groups": ["developers", "admins"],
%%%                 ...
%%%             }
%%%         And the following attribute mapping rules:
%%%             attributeMapping => #{
%%%                 entitlements => {optional, "groups"},
%%%                 ...
%%%             }
%%%         After user's log-in, the following group structure would be created:
%%%                 my-organization [organization]
%%%                     u     u
%%%       (member privs>|     |<member privs)
%%%                     |     |
%%%      [team] developers   admins [team]
%%%                    u        u
%%%      (manager privs>\      /<manager privs)
%%%                      \    /
%%%                      <user>
%%%
%%%
%%%     nested_entitlement_parser - converts every entitlement into a chain of
%%%         nested groups in Onedata by splitting the input on splitWith string.
%%%         The created groups have configured types (topGroupType, subGroupsType),
%%%         depending on their position in the structure. The user with such
%%%         entitlement is added only to the bottom group with given set of
%%%         privileges (userPrivileges). The names of the groups are the same as
%%%         the effect of splitting the entitlement, but normalized to allowed
%%%         chars. If the IdP has a specified voGroupName, the top group will be
%%%         added to it as a subgroup with given set of privileges
%%%         (topGroupPrivilegesInVo). All nested groups will have the type of
%%%         subGroupType and the set of privileges specified in
%%%         subGroupsPrivilegesInParent towards their parent group.
%%%
%%%         Example: having the following entitlementMapping config:
%%%             entitlementMapping => #{
%%%                 enabled => true,
%%%                 % Don't create a VO group, unlike in above
%%%                 % flat_entitlement_parser example, but define adminGroup
%%%                 voGroupName => undefined,
%%%                 adminGroup => "all_users:admins",
%%%                 parser => nested_entitlement_parser,
%%%                 parserConfig => #{
%%%                     splitWith => ":",
%%%                     topGroupType => unit,
%%%                     topGroupPrivilegesInVo => member,
%%%                     subGroupsType => team,
%%%                     subGroupsPrivilegesInParent => member,
%%%                     userPrivileges => manager
%%%                 }
%%%         Consider the following JSON received from the IdP:
%%%             {
%%%                 "groups": [
%%%                     "all_users:admins",
%%%                     "all_users:cloud_users:vm_managers"
%%%                 ],
%%%                 ...
%%%             }
%%%         And the following attribute mapping rules:
%%%             attributeMapping => #{
%%%                 entitlements => {optional, "groups"},
%%%                 ...
%%%             }
%%%         After user's log-in, the following group structure would be created:
%%%                    all_users [unit]
%%%                     u     u
%%%        (admin privs>|      \<member privs)
%%%                     |       \
%%%           .---------'   cloud_users [team]
%%%          /                u   u
%%%          \  (admin privs>/     \<member privs)
%%%           \             /       \
%%%            \        .--'     vm_managers [team]
%%%             '---.  |               u    u
%%%                  \ | (admin privs>/    /<manager privs)
%%%                   \|             /    /
%%%              [team] admins------'    /
%%%                          u          /
%%%            (manager privs>\        /
%%%                            \      /
%%%                             <user>
%%%
%%%         Note how the adminGroup ("all_users:admins") is added to all groups
%%%         in the IdP with admin privileges. The user will inherit all those
%%%         privileges and will effectively be an admin in all those groups.
%%%
%%% GROUP TYPES
%%% There are four group types in Onedata. Their purpose is to facilitate
%%% reflecting existing hierarchies from organizations. Apart from the visual
%%% representation in GUI and intuitive meaning, the group type does not have a
%%% functional effect on the system usage. Their interpretation is up to the
%%% admins and users, but the recommended usage of the types is:
%%%     * organization - represents an organization; institution or virtual
%%%         organization (VO), e.g. "Elixir Europe"
%%%     * unit - represents a unit in organization, e.g. "R&D department"
%%%     * team - represents a collaborating team of users that address a
%%%         specific issue / topic, e.g. "WP5.1"
%%%     * role_holders - groups people that posses a certain role, e.g. "Admins"
%%%         (in Onedata, there is no concept of roles - rather than that, users
%%%         with the same role/privileges should be organized in groups)
%%%
%%%
%%% PRIVILEGES
%%% It is possible to specify privileges of the user towards the bottom group
%%% of the nested structure or privileges of the groups in the nested chain
%%% towards their parents. These privileges will be set only when creating a new
%%% membership - if a membership already exists, the privileges are not
%%% accounted. It means that any changes in privileges introduced by users in an
%%% existing structure will never be overwritten by the entitlements mapping.
%%% There are three possible sets of privileges: member, manager, admin.
%%% They expand to a certain set of Onedata group privileges:
%%%     * member -> [group_view]
%%%     * manager -> [group_view,
%%%         group_invite_user, group_remove_user,
%%%         group_add_parent, group_leave_parent,
%%%         group_add_child, group_remove_child]
%%%     * admin -> [group_view,
%%%         group_invite_user, group_remove_user,
%%%         group_add_parent, group_leave_parent,
%%%         group_add_child, group_remove_child,
%%%         group_update, group_delete,
%%%         group_view_privileges, group_set_privileges,
%%%         group_add_space, group_leave_space,
%%%         group_create_handle_service, group_leave_handle_service,
%%%         group_create_handle, group_leave_handle]
%%%
%%% ADVANCED: Custom entitlement parsers
%%% Entitlement parsers are Erlang modules that implement
%%% entitlement_parser_behaviour. They are used to convert a raw entitlement
%%% coming from an IdP into Onedata's internal, unified format, which looks like
%%% this:
%%%
%%%     #idp_entitlement{
%%%         idp = myIdP,
%%%         path = [
%%%             #idp_group{type = organization, name = <<"my-org">>, privileges = member},
%%%             #idp_group{type = unit, name = <<"my-unit">>, privileges = member},
%%%             #idp_group{type = team, name = <<"my-team">>, privileges = manager}
%%%         ],
%%%         % user privileges in the bottom group
%%%         privileges = admin
%%%     }.
%%%
%%% Such entitlement expresses that a chain of nested groups should be created,
%%% and user be added to the bottom one with given privileges set.
%%% Every entry in the path denotes the type and name of group in Onedata and
%%% the privileges of that group in its parent group (if existent).
%%% The above entitlement would yield the following group structure in Onezone:
%%%
%%%     my-organization [organization]
%%%         u
%%%         |<member privs)
%%%         |
%%%         my-unit [unit]
%%%             u
%%%             |<manager privs)
%%%             |
%%%             my-team [team]
%%%                 u
%%%                 |<admin privs)
%%%                 |
%%%                 <user>
%%%
%%% If an admin group is specified for myIdP, let it be called "admins", and
%%% the user has the following entitlements (after the mapping):
%%% [
%%%     #idp_entitlement{
%%%         idp = myIdP,
%%%         path = [
%%%             #idp_group{type = organization, name = <<"my-org">>, privileges = member},
%%%             #idp_group{type = unit, name = <<"my-unit">>, privileges = member},
%%%             #idp_group{type = team, name = <<"my-team">>, privileges = manager}
%%%         ],
%%%         % user privileges in the bottom group
%%%         privileges = admin
%%%     },
%%%     #idp_entitlement{
%%%         path = [
%%%             #idp_group{type = organization, name = <<"my-org">>, privileges = member},
%%%             #idp_group{type = role_holders, name = <<"admins">>, privileges = <irrelevant>}
%%%         ],
%%%         privileges = manager
%%%     }
%%% ]
%%%
%%% The following group structure would be created:
%%%
%%%                          my-organization [organization]
%%%                          u   u
%%%                          |   |<member privs)
%%%                .---------'   |
%%%   (admin privs>|             my-unit [unit]
%%%                |             u   u
%%%                | .-----------'   |<manager privs)
%%%                | |               |
%%%                | |<admin privs)  my-team [team]
%%%                | |                u   u
%%%                | |   (admin privs>|   |<admin privs)
%%%                | |                |   |
%%% [role_holders] admins-------------'   |
%%%                   u                   |
%%%    (manager privs>|                   |
%%%                   '----------------- <user>
%%%
%%% Furthermore, when voGroupName is specified as "my-vo-group", the whole
%%% structure is added as children to that VO (organization) group. In such
%%% case, the above entitlements would yield the following group structure:
%%%
%%%               my-vo-group [organization]
%%%                  u   u
%%%     (admin privs>|   |<member privs)
%%%                  |   |
%%%           .------'   '---------------my-organization [organization]
%%%           |                          u   u
%%%           |                          |   |<member privs)
%%%           |                .---------'   |
%%%           |   (admin privs>|             my-unit [unit]
%%%           |                |             u   u
%%%           |                | .-----------'   |<manager privs)
%%%           |                | |               |
%%%           |                | |<admin privs)  my-team [team]
%%%           |                | |                u   u
%%%           '--------------. | |   (admin privs>|   |<admin privs)
%%%                           \| |                |   |
%%%            [role_holders] admins--------------'   |
%%%                               u                   |
%%%                (manager privs>|                   |
%%%                               '----------------- <user>
%%%
%%% To implement your own entitlement parser, you must create an Erlang module
%%% that implements the behaviour 'auth_plugin_behaviour' and returns the atom
%%% 'entitlement_parser' from the type/0 callback, and implements the
%%% 'entitlement_parser_behaviour'.
%%% An exemplary custom entitlement parser that supports EGI group format can
%%% be found in /etc/oz_worker/auth_plugins/custom_entitlement_parser.
%%%
%%% Custom entitlement parsers must be placed in the auth plugins directory.
%%% They will be loaded (and validated) on Onezone startup.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(entitlement_mapping).

-include("auth/auth_common.hrl").
-include("auth/auth_errors.hrl").
-include("auth/entitlement_mapping.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").


-type raw_entitlement() :: binary().
-type idp_group() :: #idp_group{}.
-type idp_entitlement() :: #idp_entitlement{}.
-type privileges() :: member | manager | admin.
-export_type([raw_entitlement/0, idp_group/0, idp_entitlement/0, privileges/0]).


-define(DEFAULT_PARSER, flat_entitlement_parser).

% Macros for reading certain config params of given IdP
-define(cfg(__IdP, __Path, __Policy), auth_config:get_entitlement_mapping_config(
    __IdP, __Path, __Policy
)).
-define(CFG_ENTITLEMENT_MAPPING_ENABLED(__IdP), ?cfg(__IdP, [enabled], {default, false})).
-define(CFG_VO_GROUP_NAME(__IdP), ?bin(?cfg(__IdP, [voGroupName], {default, undefined}))).
-define(CFG_ADMIN_GROUP(__IdP), ?bin(?cfg(__IdP, [adminGroup], {default, undefined}))).
-define(CFG_PARSER(__IdP), ?cfg(__IdP, [parser], {default, ?DEFAULT_PARSER})).
-define(CFG_PARSER_CONFIG(__IdP), ?cfg(__IdP, [parserConfig], {default, #{}})).


%% API
-export([
    enabled/1,
    coalesce_entitlements/3,
    gen_group_id/1,
    map_entitlement/2,
    map_privileges/1
]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns if given IdP has entitlement mapping enabled.
%% @end
%%--------------------------------------------------------------------
-spec enabled(auth_config:idp()) -> boolean().
enabled(IdP) ->
    auth_config:idp_exists(IdP) andalso ?CFG_ENTITLEMENT_MAPPING_ENABLED(IdP).


%%--------------------------------------------------------------------
%% @doc
%% Adds the user to new groups resulting from his entitlements and removes him
%% from the one he lost access to, based on the list of linked accounts and
%% previous entitlements.
%% @end
%%--------------------------------------------------------------------
-spec coalesce_entitlements(od_user:id(), [od_user:linked_account()], [od_group:id()]) ->
    [od_group:id()].
coalesce_entitlements(UserId, LinkedAccounts, OldEntitlementGroupIds) ->
    NewEntitlements = lists:flatmap(fun(LinkedAccount) ->
        map_entitlements(LinkedAccount)
    end, LinkedAccounts),

    EntitlementsToAdd = lists:filter(fun({GroupId, _IdPEntitlement}) ->
        not lists:member(GroupId, OldEntitlementGroupIds)
    end, NewEntitlements),

    EntitlementsToRemove = lists:foldl(fun({GroupId, _IdPEntitlement}, AccGroups) ->
        lists:delete(GroupId, AccGroups)
    end, OldEntitlementGroupIds, NewEntitlements),

    lists:foreach(fun({GroupId, IdPEnt = #idp_entitlement{privileges = UserPrivileges}}) ->
        ensure_group_structure(IdPEnt),
        group_logic:add_user(?ROOT, GroupId, UserId, map_privileges(UserPrivileges))
    end, EntitlementsToAdd),

    lists:foreach(fun(GroupId) ->
        group_logic:remove_user(?ROOT, GroupId, UserId)
    end, EntitlementsToRemove),

    {GroupIds, _IdPEntitlements} = lists:unzip(NewEntitlements),
    GroupIds.


%%--------------------------------------------------------------------
%% @doc
%% Encodes a group path into group id used in database.
%% @end
%%--------------------------------------------------------------------
-spec gen_group_id(idp_entitlement() | [idp_group()]) -> binary().
gen_group_id(#idp_entitlement{path = Path}) ->
    gen_group_id(Path);
gen_group_id(Path) ->
    GroupNames = lists:map(fun(#idp_group{type = Type, name = Name}) ->
        <<(encode_type(Type))/binary, ":", Name/binary>>
    end, Path),
    datastore_utils:gen_key(<<"">>, str_utils:join_binary(GroupNames, <<"/">>)).


%%--------------------------------------------------------------------
%% @doc
%% Maps a raw entitlement into entitlement in Onedata format.
%% @end
%%--------------------------------------------------------------------
-spec map_entitlement(auth_config:idp(), raw_entitlement()) ->
    {ok, {od_group:id(), idp_entitlement()}} | {error, malformed}.
map_entitlement(IdP, RawEntitlement) ->
    VoGroupName = ?CFG_VO_GROUP_NAME(IdP),
    AdminGroup = ?CFG_ADMIN_GROUP(IdP),
    Parser = ?CFG_PARSER(IdP),
    ParserConfig = ?CFG_PARSER_CONFIG(IdP),
    try
        ParsedEntitlement = Parser:parse(IdP, RawEntitlement, ParserConfig),
        ParsedEntitlementWithVo = prepend_vo_group(ParsedEntitlement, VoGroupName),
        IdPEntitlement = case RawEntitlement of
            AdminGroup -> convert_to_admin_group(ParsedEntitlementWithVo);
            _ -> ParsedEntitlementWithVo
        end,
        GroupId = gen_group_id(IdPEntitlement),
        ?debug("Successfully mapped entitlement '~ts' to Onedata group: '~s'", [
            RawEntitlement, GroupId
        ]),
        {ok, {GroupId, IdPEntitlement}}
    catch Type:Reason ->
        ?debug_stacktrace("Cannot parse entitlement \"~s\" for IdP '~p' due to ~p:~p", [
            RawEntitlement, IdP, Type, Reason
        ]),
        {error, malformed}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of privileges represented by given privileges set.
%% @end
%%--------------------------------------------------------------------
-spec map_privileges(privileges()) -> [privileges:group_privilege()].
map_privileges(member) -> privileges:group_user();
map_privileges(manager) -> privileges:group_manager();
map_privileges(admin) -> privileges:group_admin().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Maps a list of raw entitlements into entitlements in Onedata format.
%% @end
%%--------------------------------------------------------------------
-spec map_entitlements(od_user:linked_account()) -> [idp_entitlement()].
map_entitlements(#linked_account{idp = IdP, entitlements = Entitlements}) ->
    map_entitlements(IdP, Entitlements).

-spec map_entitlements(auth_config:idp(), [raw_entitlement()]) -> [idp_entitlement()].
map_entitlements(IdP, Entitlements) ->
    case ?CFG_ENTITLEMENT_MAPPING_ENABLED(IdP) of
        false ->
            [];
        true ->
            lists:filtermap(fun(RawEntitlement) ->
                case map_entitlement(IdP, RawEntitlement) of
                    {ok, IdpEntitlement} -> {true, IdpEntitlement};
                    {error, malformed} -> false
                end
            end, Entitlements)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates all missing groups in a chain defined by the idp_entitlement.
%% Adds the admin group to newly created groups, if specified in config.
%% @end
%%--------------------------------------------------------------------
-spec ensure_group_structure(idp_entitlement()) -> od_group:id().
ensure_group_structure(#idp_entitlement{idp = IdP, path = Path}) ->
    ensure_group_structure(1, Path, undefined, resolve_admin_group(IdP)).

-spec ensure_group_structure(Depth :: integer(), [idp_group()], ParentId :: undefined | od_group:id(),
    AdminGroupId :: undefined | od_group:id()) -> od_group:id().
ensure_group_structure(Depth, Path, ParentId, _AdminGroupId) when Depth > length(Path) ->
    ParentId;
ensure_group_structure(Depth, Path, ParentId, AdminGroupId) ->
    SubGroupPath = lists:sublist(Path, Depth),
    GroupId = ensure_group(SubGroupPath, ParentId),
    add_admin_group_as_child(GroupId, AdminGroupId),
    ensure_group_structure(Depth + 1, Path, GroupId, AdminGroupId).


%% @private
-spec ensure_group([idp_group()], ParentId :: undefined | od_group:id()) -> od_group:id().
ensure_group(Path, ParentId) ->
    GroupId = gen_group_id(Path),
    #idp_group{type = Type, name = Name, privileges = Privileges} = lists:last(Path),
    % Update will create the group if it does not exist
    {ok, _} = od_group:update(GroupId, fun(Group) -> {ok, Group} end, #od_group{
        name = entity_logic:normalize_name(Name),
        type = Type
    }),
    ParentId /= undefined andalso group_logic:add_group(
        ?ROOT, ParentId, GroupId, map_privileges(Privileges)
    ),
    GroupId.


%% @private
-spec add_admin_group_as_child(od_group:id(), AdminGroupId :: undefined | od_group:id()) -> ok.
add_admin_group_as_child(_GroupId, undefined) ->
    ok;
add_admin_group_as_child(GroupId, GroupId) ->
    % Do not add the admin group to itself
    ok;
add_admin_group_as_child(GroupId, AdminGroupId) ->
    group_logic:add_group(?ROOT, GroupId, AdminGroupId, map_privileges(admin)),
    ok.


%% @private
-spec prepend_vo_group(idp_entitlement(), undefined | binary()) -> idp_entitlement().
prepend_vo_group(IdPEntitlement, undefined) ->
    IdPEntitlement;
prepend_vo_group(IdPEntitlement = #idp_entitlement{path = Path}, VoGroupName) ->
    VoEntry = #idp_group{type = organization, name = VoGroupName, privileges = member},
    IdPEntitlement#idp_entitlement{path = [VoEntry | Path]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the admin group id for given IdP, or undefined. If the admin group
%% does not exist, it is created. Uses simple cache to cache the results
%% if the admin group was already created.
%% @end
%%--------------------------------------------------------------------
-spec resolve_admin_group(auth_config:idp()) -> undefined | od_group:id().
resolve_admin_group(IdP) ->
    case ?CFG_ADMIN_GROUP(IdP) of
        undefined ->
            undefined;
        RawAdminGroup ->
            {ok, GroupId} = simple_cache:get({admin_group, {IdP, RawAdminGroup}}, fun() ->
                case create_admin_group(IdP, RawAdminGroup) of
                    false ->
                        % Do not cache in case of failure to map the entitlement
                        {false, undefined};
                    {true, GroupId} ->
                        {true, GroupId}
                end
            end),
            GroupId
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the admin group for given IdP. Returns false if no admin group is
%% specified in config, or {true, GroupId} if it was created.
%% @end
%%--------------------------------------------------------------------
-spec create_admin_group(auth_config:idp(), raw_entitlement()) -> false | {true, od_group:id()}.
create_admin_group(IdP, RawAdminGroup) ->
    case map_entitlement(IdP, RawAdminGroup) of
        {error, malformed} ->
            ?warning("Cannot parse admin group for IdP '~p'", [IdP]),
            false;
        {ok, {AdminGroupId, #idp_entitlement{path = Path}}} ->
            AdminGroupPath = convert_to_admin_group(Path),
            % Create the admin group first, as ensure_group_structure/4 adds the
            % admin group to all created groups on the path.
            ensure_group(AdminGroupPath, undefined),
            {true, ensure_group_structure(1, AdminGroupPath, undefined, AdminGroupId)}
    end.


%% @private
-spec convert_to_admin_group(idp_entitlement() | [idp_group()]) ->
    idp_entitlement() | [idp_group()].
convert_to_admin_group(IdPEntitlement = #idp_entitlement{path = Path}) ->
    IdPEntitlement#idp_entitlement{path = convert_to_admin_group(Path)};
convert_to_admin_group(Path) when is_list(Path) ->
    [AdminGroup | ParentGroups] = lists:reverse(Path),
    lists:reverse([AdminGroup#idp_group{privileges = admin} | ParentGroups]).


%% @private
-spec encode_type(od_group:type()) -> binary().
encode_type(organization) -> <<"vo">>;
encode_type(unit) -> <<"ut">>;
encode_type(team) -> <<"tm">>;
encode_type(role_holders) -> <<"rl">>.