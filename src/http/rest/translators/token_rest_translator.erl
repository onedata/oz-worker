%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% onezone configuration into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(token_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Wojciech Geisler").

-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").

-export([create_response/4, get_response/2]).

-define(TOKEN_REPLY(Token), rest_translator:ok_body_reply(#{
    <<"token">> => element(2, {ok, _} = tokens:serialize(Token))
})).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback create_response/4.
%% @end
%%--------------------------------------------------------------------
-spec create_response(entity_logic:gri(), entity_logic:auth_hint(),
    entity_logic:data_format(), Result :: term() | {entity_logic:gri(), term()} |
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> no_return().
create_response(#gri{aspect = examine}, _, value, TokenData) ->
    #{
        <<"onezoneDomain">> := OnezoneDomain,
        <<"id">> := TokenId,
        <<"persistence">> := Persistence,
        <<"subject">> := Subject,
        <<"type">> := Type,
        <<"caveats">> := Caveats
    } = TokenData,
    rest_translator:ok_body_reply(#{
        <<"onezoneDomain">> => OnezoneDomain,
        <<"id">> => TokenId,
        <<"persistence">> => Persistence,
        <<"subject">> => aai:subject_to_json(Subject),
        <<"type">> => tokens:type_to_json(Type),
        <<"caveats">> => [caveats:to_json(C) || C <- Caveats]
    });

create_response(#gri{aspect = confine}, _, value, Token) ->
    ?TOKEN_REPLY(Token);

create_response(#gri{aspect = verify_access_token}, _, value, Subject) ->
    rest_translator:ok_body_reply(#{<<"subject">> => aai:subject_to_json(Subject)});
create_response(#gri{aspect = verify_identity_token}, _, value, Subject) ->
    rest_translator:ok_body_reply(#{<<"subject">> => aai:subject_to_json(Subject)});
create_response(#gri{aspect = verify_invite_token}, _, value, Subject) ->
    rest_translator:ok_body_reply(#{<<"subject">> => aai:subject_to_json(Subject)});

create_response(#gri{aspect = {user_named_token, _}}, _, resource, {_, #{<<"token">> := Token}}) ->
    ?TOKEN_REPLY(Token);
create_response(#gri{aspect = {provider_named_token, _}}, _, resource, {_, #{<<"token">> := Token}}) ->
    ?TOKEN_REPLY(Token);
create_response(#gri{aspect = {user_temporary_token, _}}, _, value, Token) ->
    ?TOKEN_REPLY(Token);
create_response(#gri{aspect = {provider_temporary_token, _}}, _, value, Token) ->
    ?TOKEN_REPLY(Token).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, TokenIds) ->
    rest_translator:ok_body_reply(#{<<"tokens">> => TokenIds});
get_response(#gri{id = undefined, aspect = {user_named_tokens, _}}, Tokens) ->
    rest_translator:ok_body_reply(#{<<"tokens">> => Tokens});
get_response(#gri{id = undefined, aspect = {provider_named_tokens, _}}, Tokens) ->
    rest_translator:ok_body_reply(#{<<"tokens">> => Tokens});

get_response(#gri{aspect = instance}, TokenData) ->
    named_token_reply(TokenData);
get_response(#gri{aspect = {user_named_token, _}}, TokenData) ->
    named_token_reply(TokenData);
get_response(#gri{aspect = {provider_named_token, _}}, TokenData) ->
    named_token_reply(TokenData).


%% @private
-spec named_token_reply(entity_logic:data()) -> #rest_resp{}.
named_token_reply(TokenData) ->
    #{
        <<"id">> := TokenId,
        <<"name">> := Name,
        <<"subject">> := Subject,
        <<"type">> := Type,
        <<"caveats">> := Caveats,
        <<"metadata">> := Metadata,
        <<"revoked">> := Revoked,
        <<"token">> := Token
    } = TokenData,
    rest_translator:ok_body_reply(#{
        <<"id">> => TokenId,
        <<"name">> => Name,
        <<"subject">> => aai:subject_to_json(Subject),
        <<"type">> => tokens:type_to_json(Type),
        <<"caveats">> => [caveats:to_json(C) || C <- Caveats],
        <<"metadata">> => Metadata,
        <<"revoked">> => Revoked,
        <<"token">> => element(2, {ok, _} = tokens:serialize(Token))
    }).
