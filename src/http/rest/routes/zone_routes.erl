%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of zone REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(zone_routes).

-include("http/rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of zone REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    %% Returns public configuration of Onezone service.
    %% This operation requires one of the following privileges:
    {<<"/configuration">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = oz_worker, id = undefined, aspect = configuration}
    }},
    %% Returns test image.
    %% This operation requires one of the following privileges:
    {<<"/test_image">>, #rest_req{
        method = 'GET',
        produces = [<<"image/png">>],
        b_gri = #b_gri{type = oz_worker, id = undefined, aspect = test_image}
    }}
].
