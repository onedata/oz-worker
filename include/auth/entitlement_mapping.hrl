%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Michal Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions concerning idp entitlements.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ENTITLEMENT_MAPPING_HRL).
-define(ENTITLEMENT_MAPPING_HRL, 1).

% Record expressing a single IdP group
-record(idp_group, {
    name :: od_group:name(),
    type = team :: od_group:type(),
    % group privileges in the parent group (if existent)
    privileges = member :: entitlement_mapping:privileges()
}).

% Record expressing an idp entitlement - a path of nested groups and user's
% privileges in the bottom one.
-record(idp_entitlement, {
    idp :: auth_config:idp(),
    % List expressing the nesting of the groups, analyzed from the left
    % (top group). Every entry expresses the #idp_group on the path, which
    % includes its privileges in the parent (if any) - see the #idp_group record.
    % If a VO group is specified for the IdP, the privileges in the first
    % #idp_group expresses the privileges of the top group towards the VO group.
    path :: [entitlement_mapping:idp_group()],
    % user privileges in the bottom group
    privileges = member :: entitlement_mapping:privileges()
}).

% See entitlement_mapping.erl for more info on the use of these records.

-endif.
