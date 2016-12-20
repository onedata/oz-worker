%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains definitions of all REST operations, in a
%%% format of cowboy router.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

% TODO definy dla codow

-export([response/5]).
-export([created_reply/1, updated_reply/0]).

-define(PROVIDER_PLUGIN, n_provider_logic_plugin).


response(_, _, _, _, {error, Type}) ->
    error_rest_translator:response({error, Type});
response(Function, ?PROVIDER_PLUGIN, EntityId, Resource, Result) ->
    provider_rest_translator:response(Function, EntityId, Resource, Result).


% Make sure there is no leading slash (so filename can be used for joining path)
created_reply([<<"/", Path/binary>> | Tail]) ->
    created_reply([Path | Tail]);
created_reply(PathTokens) ->
    {ok, RestPrefix} = application:get_env(?APP_NAME, rest_api_prefix),
    LocationHeader = #{
        <<"location">> => filename:join([RestPrefix | PathTokens])
    },
    #rest_resp{code = 201, headers = LocationHeader}.

updated_reply() ->
    #rest_resp{code = 204}.