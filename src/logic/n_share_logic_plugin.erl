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

-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create/4, get_entity/1, get_internal/4, get_external/2, update/2,
    delete/1]).
-export([exists/2, authorize/5, validate/2]).


create(#client{type = user}, _, entity, Data) ->
    ShareId = maps:get(<<"shareId">>, Data),
    Name = maps:get(<<"name">>, Data),
    SpaceId = maps:get(<<"spaceId">>, Data),
    RootFileId = maps:get(<<"rootFileId">>, Data),
    Share = #document{key = ShareId, value = #od_share{
        name = Name,
        root_file = RootFileId,
        public_url = n_share_logic:share_id_to_public_url(ShareId)
    }},
    % TODO error handling a nie badmatch
    {ok, ShareId} = od_share:create(Share),
    % TODO error handling a nie badmatch
    ok = entity_graph:add_relation(
        od_space, SpaceId,
        od_share, ShareId
    ),
    {ok, ShareId}.


get_entity(ShareId) ->
    case od_share:get(ShareId) of
        {ok, #document{value = Share}} ->
            {ok, Share};
        _ ->
            ?EL_NOT_FOUND
    end.


get_internal(#client{type = user}, _ShareId, _, _) ->
    ok.


get_external(#client{type = user}, _) ->
    ok.


update(ShareId, Data) when is_binary(ShareId) ->
    {ok, _} = od_share:update(ShareId, fun(Share) ->
        % TODO czy cos sie da update?
        {ok, Share#od_share{}}
    end),
    ok.


delete(ShareId) when is_binary(ShareId) ->
    ok = od_share:delete(ShareId).


exists(undefined, entity) ->
    true;
exists(ShareId, entity) when is_binary(ShareId) ->
    {internal, fun(#od_share{}) ->
        % If the share with ShareId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(#client{type = user, id = UserId}, create, undefined, entity, Data) ->
    SpaceId = maps:get(<<"spaceId">>, Data, <<"">>),
    {external, fun() ->
        n_space_logic:has_eff_privilege(
            SpaceId, UserId, space_manage_shares
        )
    end};
authorize(#client{type = user, id = UserId}, get, _ShareId, entity, _) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        n_space_logic_plugin:has_eff_user(SpaceId, UserId)
    end};


authorize(#client{type = user, id = UserId}, update, _ShareId, entity, _) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        n_space_logic:has_eff_privilege(SpaceId, UserId, space_manage_shares)
    end};

authorize(#client{type = user, id = UserId}, delete, _ShareId, entity, _) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        n_space_logic:has_eff_privilege(SpaceId, UserId, space_manage_shares)
    end}.


validate(create, entity) -> #{
    required => #{
        <<"shareId">> => {binary, {not_exists, fun(Value) ->
            not share_logic:exists(Value) end}
        },
        <<"name">> => {binary, non_empty},
        <<"rootFileId">> => {binary, non_empty},
        <<"spaceId">> => {binary, {exists, fun(Value) ->
            space_logic:exists(Value) end}
        }
    }
};
validate(update, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
}.

