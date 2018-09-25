%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% Common definitions concerning idp group mapping.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(IDP_GROUP_MAPPING_HRL).
-define(IDP_GROUP_MAPPING_HRL, 1).

% Record expressing idp group.
-record(idp_group, {
    name :: od_group:name(),
    type = team :: od_group:type()
}).

% Record expressing idp entitlement.
-record(idp_entitlement, {
    path :: [#idp_group{}],
    privileges = member :: member | manager | admin
}).

-endif.
