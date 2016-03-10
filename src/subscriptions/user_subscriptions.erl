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

updates(ProviderID, NewUsers) ->
    UserChanges = get_users(ProviderID, NewUsers),
    GroupChanges = get_groups(ProviderID, UserChanges),
    SpaceChanges = get_spaces(ProviderID, UserChanges)
        ++ get_group_spaces(ProviderID, GroupChanges),
    Changes = UserChanges ++ SpaceChanges ++ GroupChanges,

    lists:map(fun({Seq, Doc, Model}) ->
        {Seq, Doc#document{rev = fetch_revs(Doc, Model)}, Model}
    end, Changes).

get_users(ProviderID, NewUsers) ->
    lists:filtermap(fun(UserID) ->
        case onedata_user:get(UserID) of
            {ok, Doc} -> {true, {-1, Doc, onedata_user}};
            {error, _} ->
                ?warning("Missing user ~p; provider ~p", [UserID, ProviderID]),
                false
        end
    end, NewUsers).

get_spaces(ProviderID, UserChanges) ->
    lists:flatmap(fun({_, UserDoc, _}) ->
        #document{value = #onedata_user{spaces = Spaces}} = UserDoc,
        lists:filtermap(fun(SpaceID) ->
            case space:get(SpaceID) of
                {ok, Doc} -> {true, {-1, Doc, space}};
                {error, _} ->
                    ?warning("Missing space ~p; provider ~p", [SpaceID, ProviderID]),
                    false
            end
        end, Spaces)
    end, UserChanges).

get_groups(ProviderID, UserChanges) ->
    lists:flatmap(fun({_, UserDoc, _}) ->
        #document{value = #onedata_user{groups = Groups}} = UserDoc,
        lists:filtermap(fun(GroupID) ->
            case user_group:get(GroupID) of
                {ok, Doc} -> {true, {-1, Doc, user_group}};
                {error, _} ->
                    ?warning("Missing group ~p; provider ~p", [GroupID, ProviderID]),
                    false
            end
        end, Groups)
    end, UserChanges).


get_group_spaces(ProviderID, GroupChanges) ->
    lists:flatmap(fun({_, GroupDoc, _}) ->
        #document{value = #user_group{spaces = Spaces}} = GroupDoc,
        lists:filtermap(fun(SpaceID) ->
            case space:get(SpaceID) of
                {ok, Doc} -> {true, {-1, Doc, space}};
                {error, _} ->
                    ?warning("Missing space ~p; provider ~p", [SpaceID, ProviderID]),
                    false
            end
        end, Spaces)
    end, GroupChanges).

fetch_revs(Doc, Model) ->
    #document{key = Key, rev = Rev} = Doc,
    #model_config{bucket = Bucket} = Model:model_init(),
    DbKey = base64:encode(term_to_binary({Bucket, Key})),
    {ok, {RawRichDoc}} = couchdb_datastore_driver:db_run(couchbeam, open_doc,
        [DbKey, [{<<"revs">>, <<"true">>}, {<<"rev">>, Rev}]], 3),
    {_, {RevsRaw}} = lists:keyfind(<<"_revisions">>, 1, RawRichDoc),
    {_, Revs} = lists:keyfind(<<"ids">>, 1, RevsRaw),
    {_, Start} = lists:keyfind(<<"start">>, 1, RevsRaw),
    {Start, Revs}.