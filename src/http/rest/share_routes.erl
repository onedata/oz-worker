%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of share REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(share_routes).

-include("rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of share REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    {<<"/shares">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_share, aspect = list}
    }},
    {<<"/shares">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_share, aspect = instance}
    }},
    {<<"/shares/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance, scope = private}
    }},
    {<<"/shares/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/shares/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance}
    }}
].
