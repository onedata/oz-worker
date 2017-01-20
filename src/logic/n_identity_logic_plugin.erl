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
-module(n_identity_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("errors.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/1, authorize/4, validate/2]).
-export([entity_to_string/1]).


get_entity(_) ->
    ?ERROR_NOT_FOUND.


create(_Client, undefined, {provider, Id}, Data) ->
    EncodedPublicKey = maps:get(<<"publicKey">>, Data),
    URLs = maps:get(<<"urls">>, Data),
    RedirectionPoint = maps:get(<<"redirectionPoint">>, Data),
    case plugins:apply(identity_repository, publish, [Id, EncodedPublicKey]) of
        ok ->
            Provider = #od_provider{name = Id, urls = URLs, redirection_point = RedirectionPoint},
            {ok, _} = od_provider:save(#document{key = Id, value = Provider}),
            ok;
        {error, Reason} ->
            ?warning("Unable to create new provider with ID ~p due to ~p", [Id, Reason]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


get(_, undefined, undefined, {publickey, Id}) ->
    plugins:apply(identity_repository, get, [Id]).


update(undefined, {publickey, Id}, #{<<"publicKey">> := EncodedPublicKey}) ->
    case plugins:apply(identity_repository, publish, [Id, EncodedPublicKey]) of
        ok ->
            ok;
        {error, _Reason} ->
            ?warning("Unsucessful trial to override key of ~p", [Id]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


delete(_, _) ->
    ?ERROR_NOT_IMPLEMENTED.


exists({publickey, Id}) ->
    {external, fun() ->
        case plugins:apply(identity_repository, get, [Id]) of
            {error, _} -> false;
            {ok, _} -> true
        end
    end}.


authorize(create, undefined, {provider, _Id}, _Client) ->
    true;

authorize(get, undefined, {publickey, _Id}, _Client) ->
    true;

authorize(update, undefined, {publickey, Id}, ?PROVIDER(ProviderId)) ->
    Id =:= ProviderId;

authorize(delete, undefined, _, _Client) ->
    false.


validate(create, {provider, _Id}) -> #{
    required => #{
        <<"publicKey">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty}
    }
};
validate(update, {publickey, _Id}) -> #{
    required => #{
        <<"publicKey">> => {binary, non_empty}
    }
}.


entity_to_string(Id) ->
    <<"identity:", Id/binary>>.


