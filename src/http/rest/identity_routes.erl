%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains definitions of identity REST methods.
%%% @end
%%%--------------------------------------------------------------------
-module(identity_routes).

-include("rest.hrl").

-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle REST paths.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{binary(), #rest_req{}}].
routes() -> [
    {<<"/publickey/:id">>, #rest_req{
        method = 'GET',
        b_gri = #b_gri{type = owned_identity, aspect = {publickey, ?BINDING(id)}}
    }},
    {<<"/publickey/:id">>, #rest_req{
        method = 'PATCH',
        b_gri = #b_gri{type = owned_identity, aspect = {publickey, ?BINDING(id)}}
    }},
    {<<"/provider_data/:id">>, #rest_req{
        method = 'POST',
        b_gri = #b_gri{type = owned_identity, aspect = {provider, ?BINDING(id)}}
    }}
].
