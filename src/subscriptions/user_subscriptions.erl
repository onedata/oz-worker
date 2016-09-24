%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module resolves what should be pushed on new user joining.
%%% @end
%%%-------------------------------------------------------------------
-module(user_subscriptions).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_common_internal.hrl").
-include_lib("ctool/include/logging.hrl").

-export([updates/2]).

%%--------------------------------------------------------------------
%% @doc
%% Fetches documents pushed when new users are declared in subscription
%% for the first time.
%% Special sequence number (-1) is used to force update push.
%% @end
%%--------------------------------------------------------------------
-spec updates(ProviderID :: binary(), NewUserIDs :: [binary()]) ->
    [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}].
updates(ProviderID, NewUserIDs) ->
    UserChanges = get_users(ProviderID, NewUserIDs),
    GroupChanges = get_groups(ProviderID, UserChanges),
    SpaceChanges = get_spaces(ProviderID, UserChanges)
        ++ get_group_spaces(ProviderID, GroupChanges),
    ShareChanges = get_shares(ProviderID, SpaceChanges),
    HandleServiceChanges = get_handle_services(ProviderID, UserChanges),
    HandleChanges = get_handles(ProviderID, UserChanges),
    lists:flatten([
        UserChanges,
        SpaceChanges,
        GroupChanges,
        ShareChanges,
        HandleServiceChanges,
        HandleChanges
    ]).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc @private
%% Fetches user documents.
%% @end
%%--------------------------------------------------------------------
-spec get_users(ProviderID :: binary(), NewUserIDs :: [binary()]) ->
    [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}].
get_users(ProviderID, NewUserIDs) ->
    lists:filtermap(fun(UserID) ->
        case get_with_revs(onedata_user, UserID) of
            {ok, Doc} -> {true, {-1, Doc, onedata_user}};
            {error, _} ->
                ?warning("Missing user ~p; provider ~p", [UserID, ProviderID]),
                false
        end
    end, NewUserIDs).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches spaces of the users.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(ProviderID :: binary(),
    UserChanges :: [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}]) ->
    [{Seq1 :: -1, Doc1 :: datastore:document(), Model1 :: atom()}].
get_spaces(ProviderID, UserChanges) ->
    lists:flatmap(fun({_, UserDoc, _}) ->
        #document{value = #onedata_user{spaces = Spaces}} = UserDoc,
        lists:filtermap(fun(SpaceID) ->
            case get_with_revs(space, SpaceID) of
                {ok, Doc} -> {true, {-1, Doc, space}};
                {error, _} ->
                    ?warning("Missing space ~p; provider ~p", [SpaceID, ProviderID]),
                    false
            end
        end, Spaces)
    end, UserChanges).


%%--------------------------------------------------------------------
%% @doc @private
%% Fetches groups of the users.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(ProviderID :: binary(),
    UserChanges :: [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}]) ->
    [{Seq1 :: -1, Doc1 :: datastore:document(), Model1 :: atom()}].
get_groups(ProviderID, UserChanges) ->
    lists:flatmap(fun({_, UserDoc, _}) ->
        #document{value = #onedata_user{effective_groups = Groups}} = UserDoc,

        AllGroups = lists:usort(lists:flatmap(fun(GroupID) ->
            case user_group:get(GroupID) of
                {ok, #document{value = #user_group{nested_groups = Tuples}}} ->
                    {NestedIDs, _} = lists:unzip(Tuples),
                    [GroupID | NestedIDs];
                {error, _} ->
                    ?warning("Missing group ~p; provider ~p", [GroupID, ProviderID]),
                    [GroupID]
            end
        end, Groups)),

        lists:filtermap(fun(GroupID) ->
            case get_with_revs(user_group, GroupID) of
                {ok, Doc} -> {true, {-1, Doc, user_group}};
                {error, _} ->
                    ?warning("Missing group ~p; provider ~p", [GroupID, ProviderID]),
                    false
            end
        end, AllGroups)
    end, UserChanges).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches spaces of the users that are accessible due to group membership.
%% @end
%%--------------------------------------------------------------------
-spec get_group_spaces(ProviderID :: binary(),
    GroupChanges :: [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}]) ->
    [{Seq1 :: -1, Doc1 :: datastore:document(), Model1 :: atom()}].
get_group_spaces(ProviderID, GroupChanges) ->
    lists:flatmap(fun({_, GroupDoc, _}) ->
        #document{value = #user_group{spaces = Spaces}} = GroupDoc,
        lists:filtermap(fun(SpaceID) ->
            case get_with_revs(space, SpaceID) of
                {ok, Doc} -> {true, {-1, Doc, space}};
                {error, _} ->
                    ?warning("Missing space ~p; provider ~p", [SpaceID, ProviderID]),
                    false
            end
        end, Spaces)
    end, GroupChanges).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches all handle_services in given list of spaces.
%% @end
%%--------------------------------------------------------------------
-spec get_shares(ProviderID :: binary(),
    SpaceChanges :: [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}]) ->
    [{Seq1 :: -1, Doc1 :: datastore:document(), Model1 :: atom()}].
get_shares(ProviderID, SpaceChanges) ->
    lists:flatmap(fun({_, SpaceDoc, _}) ->
        #document{value = #space{shares = Shares}} = SpaceDoc,
        lists:filtermap(fun(ShareId) ->
            case get_with_revs(share, ShareId) of
                {ok, Doc} -> {true, {-1, Doc, share}};
                {error, _} ->
                    ?warning("Missing share ~p; provider ~p", [ShareId, ProviderID]),
                    false
            end
        end, Shares)
    end, SpaceChanges).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches all handle_services of given users.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_services(ProviderID :: binary(),
    UserChanges :: [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}]) ->
    [{Seq1 :: -1, Doc1 :: datastore:document(), Model1 :: atom()}].
get_handle_services(ProviderID, UserChanges) ->
    lists:flatmap(fun({_, UserDoc, _}) ->
        HandleServices = user_logic:get_all_handle_services(UserDoc),
        lists:filtermap(fun(HandleServiceId) ->
            case get_with_revs(handle_service, HandleServiceId) of
                {ok, Doc} -> {true, {-1, Doc, handle_service}};
                {error, _} ->
                    ?warning("Missing handle_service ~p; provider ~p",
                        [HandleServiceId, ProviderID]),
                    false
            end
        end, HandleServices)
    end, UserChanges).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches all handles of given users.
%% @end
%%--------------------------------------------------------------------
-spec get_handles(ProviderID :: binary(),
    UserChanges :: [{Seq :: -1, Doc :: datastore:document(), Model :: atom()}]) ->
    [{Seq1 :: -1, Doc1 :: datastore:document(), Model1 :: atom()}].
get_handles(ProviderID, UserChanges) ->
    lists:flatmap(fun({_, UserDoc, _}) ->
        Handles = user_logic:get_all_handles(UserDoc),
        lists:filtermap(fun(HandleId) ->
            case get_with_revs(handle, HandleId) of
                {ok, Doc} -> {true, {-1, Doc, handle}};
                {error, _} ->
                    ?warning("Missing handle ~p; provider ~p",
                        [HandleId, ProviderID]),
                    false
            end
        end, Handles)
    end, UserChanges).

%%--------------------------------------------------------------------
%% @doc @private
%% Fetches document from couchbase with revisions tuple instead regular
%% binary in 'rev' field (as in changes stream).
%% @end
%%--------------------------------------------------------------------
-spec get_with_revs(Model :: atom(), Key :: binary()) ->
    {ok, datastore:document()} | {error, Reason :: binary()}.
get_with_revs(Model, Key) ->
    couchdb_datastore_driver:get_with_revs(Model:model_init(), Key).