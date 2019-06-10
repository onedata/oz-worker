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

-include("http/codes.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").

-export([handle/2]).

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
            cowboy_req:reply(?HTTP_401_UNAUTHORIZED, Req);
        {error, invalid} ->
            cowboy_req:reply(?HTTP_401_UNAUTHORIZED, Req);
        {ok, _, Cookie, Req2} ->
            try
                GuiPrefix = cowboy_req:binding(gui_prefix, Req2),
                ClusterId = cowboy_req:binding(gui_id, Req2),

                Service = try
                    onedata:service_by_gui(onedata:gui_by_prefix(GuiPrefix), ClusterId)
                catch error:badarg ->
                    throw(?HTTP_404_NOT_FOUND)
                end,

                ClusterType = onedata:service_to_cluster_type(Service),
                case cluster_logic:get(?ROOT, ClusterId) of
                    {ok, #od_cluster{type = ClusterType}} -> ok;
                    _ -> throw(?HTTP_404_NOT_FOUND)
                end,

                SessionId = gui_session:get_session_id(Cookie),
                cowboy_req:reply(
                    ?HTTP_200_OK,
                    #{<<"content-type">> => <<"application/json">>},
                    generate_gui_token(SessionId, Service, ClusterId),
                    Req2
                )
            catch
                throw:HttpCode when is_integer(HttpCode) ->
                    cowboy_req:reply(HttpCode, Req2);
                Type:Message ->
                    ?debug_stacktrace("Bad request in ~p - ~p:~p", [?MODULE, Type, Message]),
                    cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req2)
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec generate_gui_token(session:id(), onedata:service(), od_cluster:id()) ->
    Response :: binary() | no_return().
generate_gui_token(SessionId, Service, ClusterId) ->
    ClusterType = onedata:service_to_cluster_type(Service),
    {ok, UserId} = session:get_user_id(SessionId),

    can_generate_gui_token(UserId, Service, ClusterId) orelse throw(?HTTP_403_FORBIDDEN),

    {ok, {Macaroon, Expires}} = session:acquire_gui_macaroon(
        SessionId, ClusterType, ClusterId
    ),
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    json_utils:encode(#{
        <<"token">> => Token,
        <<"ttl">> => Expires - time_utils:cluster_time_seconds()
    }).


%% @private
-spec can_generate_gui_token(od_user:id(), onedata:service(), od_cluster:id()) -> boolean().
can_generate_gui_token(_UserId, ?OZ_WORKER, ?ONEZONE_CLUSTER_ID) ->
    % All users can generate a token for Onezone
    true;
can_generate_gui_token(UserId, ?OZ_PANEL, ?ONEZONE_CLUSTER_ID) ->
    cluster_logic:has_eff_user(?ONEZONE_CLUSTER_ID, UserId);
can_generate_gui_token(UserId, ?OP_WORKER, ProviderId) ->
    provider_logic:has_eff_user(ProviderId, UserId);
can_generate_gui_token(UserId, ?OP_PANEL, ProviderId) ->
    cluster_logic:has_eff_user(ProviderId, UserId).
