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
-module(n_provider_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create_impl/4, get_entity/1, get_internal/4, get_external/2, update_impl/2,
    delete_impl/1]).
-export([exists_impl/2, authorize_impl/5, validate_impl/2]).


create_impl(nobody, _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    {ok, ProviderId} = od_provider:create(
        #document{value = #od_provider{name = Name}}
    ),
    {ok, ProviderId};
create_impl({provider, ProviderId}, ProviderId, spaces, Data) ->
    Token = maps:get(<<"token">>, Data),
    % TODO walidacja w walidate!
    {true, Macaroon} = token_logic:validate(Token, space_support_token),
    % TODO to inaczej?
    {ok, {space, SpaceId}} = token_logic:consume(Macaroon),
    SupportSize = maps:get(<<"size">>, Data),
    entity_graph:add_relation(
        od_space, SpaceId,
        od_provider, ProviderId,
        SupportSize
    ),
    {ok, ProviderId}.


get_entity(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            {ok, Provider};
        _ ->
            ?EL_NOT_FOUND
    end.


get_internal({provider, _ProviderId}, _, _, _) ->
    ok.


get_external({user, _UserId}, _) ->
    ok.


update_impl(ProviderId, Data) when is_binary(ProviderId) ->
    {ok, _} = od_provider:update(ProviderId, fun(Provider) ->
        #od_provider{name = OldName} = Provider,
        NewName = maps:get(<<"name">>, Data, OldName),
        {ok, Provider#od_provider{name = NewName}}
    end),
    ok.


delete_impl(ProviderId) when is_binary(ProviderId) ->
    ok = od_provider:delete(ProviderId).


exists_impl(undefined, entity) ->
    true;
exists_impl(ProviderId, entity) when is_binary(ProviderId) ->
    {internal, fun(#od_provider{}) ->
        % If the provider with ProviderId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(ProviderId, spaces) when is_binary(ProviderId) ->
    {internal, fun(#od_provider{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize_impl(nobody, create, undefined, entity, _) ->
    true;
authorize_impl({provider, ProviderId}, create, ProviderId, spaces, _) ->
    true.


validate_impl(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
};
validate_impl(create, spaces) -> #{
    required => #{
        <<"token">> => {binary, non_empty},
        <<"size">> => {positive_integer, any}
    }
};
validate_impl(update, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
}.
