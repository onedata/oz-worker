%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called to acquire
%%% access tokens dedicated for GUI.
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_preauthorize).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include_lib("ctool/include/http/codes.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

-export([handle/2]).

-define(ERROR_REPLY(Error, Req), cowboy_req:reply(
    errors:http_code(Error),
    #{},
    json_utils:encode(errors:to_json(Error)),
    Req
)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"POST">>, Req) ->
    case gui_session:validate(Req) of
        {error, no_session_cookie} ->
            ?ERROR_REPLY(?ERROR_UNAUTHORIZED, Req);
        {error, invalid} ->
            ?ERROR_REPLY(?ERROR_UNAUTHORIZED, Req);
        {ok, _, Cookie, Req2} ->
            try
                GuiPrefix = cowboy_req:binding(gui_prefix, Req2),
                ClusterId = cowboy_req:binding(gui_id, Req2),

                Service = try
                    onedata:service_by_gui(onedata:gui_by_prefix(GuiPrefix), ClusterId)
                catch error:badarg ->
                    throw(?ERROR_NOT_FOUND)
                end,

                ClusterType = onedata:service_to_cluster_type(Service),
                case cluster_logic:get(?ROOT, ClusterId) of
                    {ok, #od_cluster{type = ClusterType}} -> ok;
                    _ -> throw(?ERROR_NOT_FOUND)
                end,

                SessionId = gui_session:get_session_id(Cookie),
                Audience = ?AUD(Service, ClusterId),
                cowboy_req:reply(
                    ?HTTP_200_OK,
                    #{<<"content-type">> => <<"application/json">>},
                    generate_gui_token(SessionId, Audience),
                    Req2
                )
            catch
                throw:{error, _} = Error ->
                    ?ERROR_REPLY(Error, Req2);
                Type:Message ->
                    ?debug_stacktrace("Bad request in ~p - ~p:~p", [?MODULE, Type, Message]),
                    ?ERROR_REPLY(?ERROR_MALFORMED_DATA, Req2)
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec generate_gui_token(session:id(), aai:audience()) -> binary() | no_return().
generate_gui_token(SessionId, Audience) ->
    {ok, UserId} = session:get_user_id(SessionId),
    case token_logic:create_gui_access_token(?USER(UserId), UserId, SessionId, Audience) of
        {error, _} = Error ->
            throw(Error);
        {ok, {Token, Ttl}} ->
            {ok, Serialized} = tokens:serialize(Token),
            json_utils:encode(#{
                <<"token">> => Serialized,
                <<"ttl">> => Ttl
            })
    end.
