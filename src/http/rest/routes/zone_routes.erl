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

-include("rest.hrl").

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
        b_gri = #b_gri{type = oz_worker, id = undefined, aspect = configuration}
    }}
].
