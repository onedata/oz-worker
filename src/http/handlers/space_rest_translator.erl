%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of system errors into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(space_rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).

response(get, SpaceId, entity, {ok, Space}) ->
    #od_space{
        name = Name,
        providers = Providers,
        shares = Shares
    } = Space,
    #rest_resp{code = ?HTTP_200_OK, body = #{
        <<"spaceId">> => SpaceId,
        <<"name">> => Name,
        % TODO VFS-2918
        <<"canonicalName">> => Name,
        <<"providersSupports">> => Providers,
        <<"shares">> => Shares
    }}.