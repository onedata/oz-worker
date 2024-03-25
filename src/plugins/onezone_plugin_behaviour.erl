%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies the API for a plugin that can be used to
%%% customize Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(onezone_plugin_behaviour).


-type type() :: entitlement_parser | openid_plugin | attribute_mapper
              | harvesting_backend | handle_metadata_plugin.
-export_type([type/0]).

%%--------------------------------------------------------------------
%% @doc
%% Returns the type of this plugin. Depending on the type, the plugin must
%% implement certain behaviour:
%%      entitlement_parser -> entitlement_parser_behaviour
%%      openid_plugin -> openid_plugin_behaviour
%%      attribute_mapper -> attribute_mapper_behaviour
%%      harvesting_backend -> attribute_mapper_behaviour
%%      handle_metadata_plugin -> handle_metadata_plugin_behaviour
%% @end
%%--------------------------------------------------------------------
-callback type() -> type().
