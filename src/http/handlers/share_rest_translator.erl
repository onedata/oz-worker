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
-module(share_rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).


response(create, undefined, entity, {ok, GroupId}) ->
    n_rest_handler:created_reply([<<"groups">>, GroupId]);

response(get, ShareId, data, {ok, ShareData}) ->
    n_rest_handler:ok_body_reply(ShareData#{
        <<"shareId">> => ShareId
    }).
