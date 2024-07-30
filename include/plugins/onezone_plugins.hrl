%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions concerning onezone plugins.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ONEZONE_PLUGINS_HRL).
-define(ONEZONE_PLUGINS_HRL, 1).


-include("datastore/oz_datastore_models.hrl").


%% @formatter:off
-record(handle_metadata_plugin_validation_example, {
    % provide only valid XMLs here; invalid ones are handled before the plugin is even called
    input_raw_xml :: od_handle:raw_metadata(),
    input_qualifies_for_publication :: boolean(),
    exp_revised_metadata_generator:: undefined | fun(
        (od_share:id(), od_share:record()) -> (od_handle:raw_metadata())
    ),
    exp_final_metadata_generator:: undefined | fun(
        (od_share:id(), od_share:record(), od_handle:public_handle()) -> (od_handle:raw_metadata())
    ),
    exp_oai_pmh_metadata_generator:: undefined | fun(
        (od_share:id(), od_share:record(), od_handle:public_handle()) -> (od_handle:raw_metadata())
    )
}).
%% @formatter:on


-endif.
