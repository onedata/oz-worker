%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_share_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("errors.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/2, authorize/4, validate/2]).
-export([entity_to_string/1]).


get_entity(ShareId) ->
    case od_share:get(ShareId) of
        {ok, #document{value = Share}} ->
            {ok, Share};
        _ ->
            ?ERROR_NOT_FOUND
    end.


create(_Client, _, entity, Data) ->
    ShareId = maps:get(<<"shareId">>, Data),
    Name = maps:get(<<"name">>, Data),
    SpaceId = maps:get(<<"spaceId">>, Data),
    RootFileId = maps:get(<<"rootFileId">>, Data),
    Share = #document{key = ShareId, value = #od_share{
        name = Name,
        root_file = RootFileId,
        public_url = n_share_logic:share_id_to_public_url(ShareId)
    }},
    case od_share:create(Share) of
        {ok, ShareId} ->
            entity_graph:add_relation(
                od_share, ShareId,
                od_space, SpaceId
            ),
            {ok, ShareId};
        _ ->
            % This can potentially happen if a share with given share id
            % has been created between data verification and create
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


get(_, undefined, undefined, list) ->
    {ok, ShareDocs} = od_share:list(),
    {ok, [ShareId || #document{key = ShareId} <- ShareDocs]};

get(_, _ShareId, #od_share{} = Share, data) ->
    #od_share{
        name = Name, public_url = PublicUrl, space = SpaceId,
        root_file = RootFileId, handle = HandleId
    } = Share,
    {ok, #{
        <<"name">> => Name, <<"publicUrl">> => PublicUrl,
        <<"spaceId">> => SpaceId, <<"rootFileId">> => RootFileId,
        <<"handleId">> => HandleId
    }}.


update(ShareId, entity, #{<<"name">> => NewName}) ->
    {ok, _} = od_share:update(ShareId, #{name => NewName}),
    ok.


delete(ShareId, entity) ->
    entity_graph:delete_with_relations(od_share, ShareId).


exists(undefined, _) ->
    true;
exists(_ShareId, entity) ->
    {internal, fun(#od_share{}) ->
        % If the share with ShareId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, entity, ?USER(UserId)) ->
    {data_dependent, fun(Data) ->
        SpaceId = maps:get(<<"spaceId">>, Data, <<"">>),
        n_space_logic:has_eff_privilege(
            SpaceId, UserId, ?SPACE_MANAGE_SHARES
        )
    end};
authorize(get, _ShareId, entity, ?USER(UserId)) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        n_space_logic:has_eff_user(SpaceId, UserId)
    end};


authorize(update, _ShareId, entity, ?USER(UserId)) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        n_space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_MANAGE_SHARES)
    end};

authorize(delete, _ShareId, entity, ?USER(UserId)) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        n_space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_MANAGE_SHARES)
    end}.


validate(create, entity) -> #{
    required => #{
        <<"shareId">> => {binary, {not_exists, fun(Value) ->
            not n_share_logic:exists(Value)
        end}},
        <<"name">> => {binary, non_empty},
        <<"rootFileId">> => {binary, non_empty},
        <<"spaceId">> => {binary, {exists, fun(Value) ->
            n_space_logic:exists(Value)
        end}}
    }
};
validate(update, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
}.


entity_to_string(ShareId) ->
    od_share:to_string(ShareId).


