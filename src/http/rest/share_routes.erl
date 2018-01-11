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
    %% Create share
    %% This operation requires one of the following privileges:
    %% - space_manage_shares
    {<<"/shares">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = od_share, id = undefined, aspect = instance}
    }},
    %% List all shares
    %% This operation requires one of the following privileges:
    %% - oz_shares_list
    {<<"/shares">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_share, id = undefined, aspect = list}
    }},
    %% Get share details
    %% This operation does not require any specific privileges.
    {<<"/shares/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance, scope = private}
    }},
    %% Modify share details
    %% This operation requires one of the following privileges:
    %% - space_manage_shares
    {<<"/shares/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance}
    }},
    %% Remove share
    %% This operation requires one of the following privileges:
    %% - space_manage_shares
    {<<"/shares/:id">>, #rest_req{
        method = 'DELETE',
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance}
    }}
].
