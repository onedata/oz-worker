%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of share REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(share_routes).

-include("http/rest.hrl").

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
    %% List all shares
    %% This operation requires one of the following privileges:
    %% - oz_shares_list
    {<<"/shares">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_share, id = undefined, aspect = list}
    }},
    %% Get share details
    %% This operation requires one of the following privileges:
    %% - space_view
    %% - oz_shares_view
    {<<"/shares/:id">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance, scope = private}
    }},
    %% Modify share details
    %% This operation requires one of the following privileges:
    %% - space_manage_shares
    %% - oz_shares_update
    {<<"/shares/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance}
    }},
    %% Get public share details
    %% This operation does not require any specific privileges.
    {<<"/shares/:id/public">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance, scope = public}
    }},
    %% Get shared file or directory data
    %% This operation does not require any specific privileges.
    {<<"/shares/data/:file_id/[...]">>, #rest_req{
        method = 'GET',
        produces = [<<"application/json">>],
        b_gri = #b_gri{type = od_share, id = undefined, aspect = {shared_data, ?BINDING(file_id)}, scope = public}
    }}
].
