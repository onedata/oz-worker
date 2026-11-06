%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions concerning automation.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_AUTOMATION_HRL).
-define(OZ_AUTOMATION_HRL, 1).


-define(MIN_SUPPORTED_SCHEMA_FORMAT_VERSION, 2).
-define(CURRENT_SCHEMA_FORMAT_VERSION, 3).

-define(SCHEMA_FORMAT_VERSION_SPEC, {integer, lists:seq(
    ?MIN_SUPPORTED_SCHEMA_FORMAT_VERSION, ?CURRENT_SCHEMA_FORMAT_VERSION
)}).


-endif.
