%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour should be implemented by modules that act as plugin for
%%% retrieving groups and roles of users logging in via SAML / OIDC.
%%% @end
%%%-------------------------------------------------------------------
-module(group_mapping_plugin_behaviour).
-include("auth_common.hrl").


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of strings that represent user's group memberships for given
%% IdP. They are strings complying with specification in idp_group_mapping
%% module. Returned values will be used to compute a diff in memberships
%% every time a user logs in, so he can be added to / removed from
%% certain groups. Because of this, the same values coming from IdP must always
%% be mapped to the same specs.
%% @end
%%--------------------------------------------------------------------
-callback normalized_membership_specs(auth_utils:idp(), maps:map()) ->
    [idp_group_mapping:membership_spec()].
