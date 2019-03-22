%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called to create
%%% access tokens dedicated for GUI.
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_token).
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
        {ok, _, Cookie, Req2} ->
            try
                SessionId = gui_session:get_session_id(Cookie),
                {ok, RequestBody, Req3} = cowboy_req:read_body(Req2),
                cowboy_req:reply(
                    ?HTTP_200_OK,
                    #{<<"content-type">> => <<"application/json">>},
                    generate_gui_token(SessionId, RequestBody),
                    Req3
                )
            catch
                throw:HttpCode when is_integer(HttpCode) ->
                    cowboy_req:reply(HttpCode, Req2);
                Type:Message ->
                    ?debug_stacktrace("Bad request in ~p - ~p:~p", [?MODULE, Type, Message]),
                    cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req2)
            end;
        {error, no_session_cookie} ->
            cowboy_req:reply(?HTTP_401_UNAUTHORIZED, Req);
        {error, invalid} ->
            cowboy_req:reply(?HTTP_401_UNAUTHORIZED, Req)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec generate_gui_token(session:id(), RequestBody :: binary()) ->
    Response :: binary() | no_return().
generate_gui_token(SessionId, RequestBody) ->
    {ok, UserId} = session:get_user_id(SessionId),
    #{
        <<"clusterType">> := ClusterTypeBin,
        <<"clusterId">> := ClusterId
    } = json_utils:decode(RequestBody),
    ClusterType = binary_to_existing_atom(ClusterTypeBin, utf8),
    % Ensure request correctness or crash with a badmatch
    {ok, Cluster = #od_cluster{type = ClusterType}} = cluster_logic:get(?ROOT, ClusterId),
    can_generate_gui_token(UserId, Cluster) orelse throw(?HTTP_403_FORBIDDEN),

    {ok, {Macaroon, Expires}} = session:acquire_gui_macaroon(
        SessionId, ClusterType, Cluster#od_cluster.service_id
    ),
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    {ok, Domain} = cluster_logic:get_domain(Cluster),
    json_utils:encode(#{
        <<"token">> => Token,
        <<"ttl">> => Expires - time_utils:cluster_time_seconds(),
        <<"domain">> => Domain
    }).


%% @private
-spec can_generate_gui_token(od_user:id(), od_cluster:record()) -> boolean().
can_generate_gui_token(_UserId, #od_cluster{type = ?ONEZONE}) ->
    % All users can generate a token for Onezone
    true;
can_generate_gui_token(UserId, Cluster = #od_cluster{type = ?ONEPROVIDER, service_id = ProviderId}) ->
    % Only members of provider/cluster can generate a token for Oneprovider
    provider_logic:has_eff_user(ProviderId, UserId) orelse
        cluster_logic:has_eff_user(Cluster, UserId).
