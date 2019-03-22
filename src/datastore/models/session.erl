%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for session record - used to store session details.
%%% @end
%%%-------------------------------------------------------------------
-module(session).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_definitions.hrl").

%% API
-export([
    create/2,
    get/1, get_user_id/1,
    update/2,
    delete/1, delete/2, delete/3,
    list/0,
    acquire_gui_macaroon/3,
    verify_gui_macaroon/4
]).

%% datastore_model callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #session{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

%% @formatter:off
-type gui_macaroons_cache() :: #{
    onedata:cluster_type() => #{
        od_cluster:service_id() => macaroon_logic:gui_macaroon()
    }
}.
%% @formatter:on
-export_type([gui_macaroons_cache/0]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined,
    fold_enabled => true
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new session.
%% @end
%%--------------------------------------------------------------------
-spec create(id(), record()) -> ok | {error, term()}.
create(SessionId, Session) ->
    case datastore_model:create(?CTX, #document{key = SessionId, value = Session}) of
        {ok, _} ->
            od_user:add_session(Session#session.user_id, SessionId),
            ok;
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves session by Id.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(Id) ->
    datastore_model:get(?CTX, Id).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user id - the owner of given session.
%% @end
%%--------------------------------------------------------------------
-spec get_user_id(id()) -> {ok, doc()} | {error, term()}.
get_user_id(Id) ->
    case ?MODULE:get(Id) of
        {ok, #document{value = #session{user_id = UserId}}} ->
            {ok, UserId};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates session by Id.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(Id, Diff) ->
    datastore_model:update(?CTX, Id, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes session by Id and clears all expired user sessions with no grace
%% period (terminates the connections immediately).
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
delete(SessionId) ->
    delete(SessionId, undefined, true).


%%--------------------------------------------------------------------
%% @doc
%% Deletes session by Id and clears all expired user sessions with given grace
%% period (delay before connections are terminated).
%% @end
%%--------------------------------------------------------------------
-spec delete(id(), GracePeriod :: undefined | non_neg_integer()) -> ok | {error, term()}.
delete(SessionId, GracePeriod) ->
    delete(SessionId, GracePeriod, true).


%%--------------------------------------------------------------------
%% @doc
%% Deletes session by Id, allows to decide if expired user sessions should be
%% cleared.
%% @end
%%--------------------------------------------------------------------
-spec delete(id(), GracePeriod :: undefined | non_neg_integer(),
    ClearExpiredUserSessions :: boolean()) -> ok | {error, term()}.
delete(SessionId, GracePeriod, ClearExpiredUserSessions) ->
    case ?MODULE:get(SessionId) of
        {ok, #document{value = #session{user_id = UserId, gui_macaroons = GuiMacaroons}}} ->
            delete_user_session(SessionId, UserId, GracePeriod, ClearExpiredUserSessions),
            delete_gui_macaroons(GuiMacaroons),
            datastore_model:delete(?CTX, SessionId);
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all sessions.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).


%%--------------------------------------------------------------------
%% @doc
%% Creates or reuses an existing GUI macaroon. The macaroon is valid only for
%% given SessionId, ClusterType (?ONEPROVIDER or ?ONEZONE) and ServiceId
%% (?ONEZONE_SERVICE_ID or ProviderId).
%% @end
%%--------------------------------------------------------------------
-spec acquire_gui_macaroon(session:id(), onedata:cluster_type(), od_cluster:service_id()) ->
    {ok, {macaroon:macaroon(), macaroon_logic:expires()}} | {error, term()}.
acquire_gui_macaroon(SessionId, ClusterType, ServiceId) ->
    case ?MODULE:get(SessionId) of
        {error, _} = Error ->
            Error;
        {ok, #document{value = #session{gui_macaroons = GuiMacaroons, user_id = UserId}}} ->
            case reuse_gui_macaroon(GuiMacaroons, ClusterType, ServiceId) of
                {true, {_Identifier, Macaroon, Expires}} ->
                    {ok, {Macaroon, Expires}};
                false ->
                    {ok, {Identifier, Macaroon, Expires}} = macaroon_logic:create_gui_macaroon(
                        UserId, SessionId, ClusterType, ServiceId
                    ),
                    update(SessionId, fun(Session = #session{gui_macaroons = GuiMacaroons}) ->
                        {ok, Session#session{gui_macaroons = add_gui_macaroon(
                            GuiMacaroons, ClusterType, ServiceId, Identifier, Macaroon, Expires
                        )}}
                    end),
                    {ok, {Macaroon, Expires}}
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies given GUI macaroon against GuiSessionId, ClusterType and ServiceId.
%% GuiSessionId can be:
%%  * Web client's cookie session id in case of Onezone GUI
%%  * undefined in case it is a delegated token (e.g. from Oneprovider that is
%%      handling a user request in its GUI).
%% @end
%%--------------------------------------------------------------------
-spec verify_gui_macaroon(macaroon:macaroon(), session:id() | undefined, onedata:cluster_type(),
    od_cluster:service_id()) -> {ok, od_user:id()} | {error, term()}.
verify_gui_macaroon(SubjectMacaroon, GuiSessionId, ClusterType, ServiceId) ->
    SessionVerifyFun = fun(SessionId, Identifier) ->
        case {SessionId, GuiSessionId} of
            {_, undefined} ->
                has_gui_macaroon(SessionId, ClusterType, ServiceId, Identifier);
            {MatchingSessId, MatchingSessId} ->
                has_gui_macaroon(SessionId, ClusterType, ServiceId, Identifier);
            {_, _} ->
                false
        end
    end,
    macaroon_logic:verify_gui_macaroon(SubjectMacaroon, ClusterType, ServiceId, SessionVerifyFun).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes model.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | {error, term()}.
init() ->
    datastore_model:init(?CTX).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes session by Id and performs required cleanup.
%% @end
%%--------------------------------------------------------------------
-spec delete_user_session(id(), od_user:id(), GracePeriod :: undefined | non_neg_integer(),
    ClearExpiredUserSessions :: boolean()) -> ok | {error, term()}.
delete_user_session(SessionId, UserId, GracePeriod, false = _ClearExpiredUserSessions) ->
    % Terminate all user connections related to this session
    user_connections:clear(SessionId, GracePeriod),
    od_user:remove_session(UserId, SessionId);

delete_user_session(SessionId, UserId, GracePeriod, true = _ClearExpiredUserSessions) ->
    delete_user_session(SessionId, UserId, GracePeriod, false),
    clear_expired_sessions(UserId, GracePeriod).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes expired sessions of given user.
%% @end
%%--------------------------------------------------------------------
-spec clear_expired_sessions(od_user:id(), GracePeriod :: undefined | non_neg_integer()) ->
    ok | {error, term()}.
clear_expired_sessions(UserId, GracePeriod) ->
    {ok, ActiveUserSessions} = od_user:get_all_sessions(UserId),
    lists:foreach(fun(UserSessionId) ->
        case ?MODULE:get(UserSessionId) of
            {ok, #document{value = #session{last_refresh = LastRefresh}}} ->
                case gui_session:is_expired(LastRefresh) of
                    true ->
                        delete_user_session(UserSessionId, UserId, GracePeriod, false);
                    _ ->
                        ok
                end;
            {error, _} ->
                delete_user_session(UserSessionId, UserId, GracePeriod, false)
        end
    end, ActiveUserSessions).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes all gui macaroons issued for this session.
%% @end
%%--------------------------------------------------------------------
-spec delete_gui_macaroons(gui_macaroons_cache()) -> ok.
delete_gui_macaroons(GuiMacaroons) ->
    maps:fold(fun(_ClusterType, Services, _) ->
        maps:fold(fun(_ServiceId, {_, Macaroon, _}, _) ->
            macaroon_logic:delete_gui_macaroon(Macaroon)
        end, ok, Services)
    end, ok, GuiMacaroons).


-spec add_gui_macaroon(gui_macaroons_cache(), onedata:cluster_type(), od_cluster:service_id(),
    macaroon_logic:id(), macaroon:macaroon(), macaroon_logic:expires()) -> gui_macaroons_cache().
add_gui_macaroon(GuiMacaroons, ClusterType, ServiceId, Identifier, Macaroon, Expires) ->
    MacaroonPerClusterType = maps:get(ClusterType, GuiMacaroons, #{}),
    GuiMacaroons#{ClusterType => MacaroonPerClusterType#{
        ServiceId => {Identifier, Macaroon, Expires}
    }}.


-spec get_gui_macaroon(gui_macaroons_cache(), onedata:cluster_type(), od_cluster:service_id()) ->
    undefined | macaroon_logic:gui_macaroon().
get_gui_macaroon(GuiMacaroons, ClusterType, ServiceId) ->
    MacaroonPerClusterType = maps:get(ClusterType, GuiMacaroons, #{}),
    maps:get(ServiceId, MacaroonPerClusterType, undefined).


-spec has_gui_macaroon(session:id() | gui_macaroons_cache(), onedata:cluster_type(),
    od_cluster:service_id(), macaroon_logic:id()) -> boolean().
has_gui_macaroon(SessionId, ClusterType, ServiceId, Identifier) when is_binary(SessionId) ->
    case ?MODULE:get(SessionId) of
        {error, _} ->
            false;
        {ok, #document{value = #session{gui_macaroons = GuiMacaroons}}} ->
            has_gui_macaroon(GuiMacaroons, ClusterType, ServiceId, Identifier)
    end;
has_gui_macaroon(GuiMacaroons, ClusterType, ServiceId, Identifier) ->
    case get_gui_macaroon(GuiMacaroons, ClusterType, ServiceId) of
        {Identifier, _, _} -> true;
        _ -> false
    end.


-spec reuse_gui_macaroon(gui_macaroons_cache(), onedata:cluster_type(), od_cluster:service_id()) ->
    false | {true, macaroon_logic:gui_macaroon()}.
reuse_gui_macaroon(GuiMacaroons, ClusterType, ServiceId) ->
    case get_gui_macaroon(GuiMacaroons, ClusterType, ServiceId) of
        undefined ->
            false;
        {Identifier, Macaroon, Expires} ->
            case macaroon_logic:should_refresh_gui_macaroon(Expires) of
                true -> false;
                false -> {true, {Identifier, Macaroon, Expires}}
            end
    end.



