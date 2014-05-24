%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for tokens created by users.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(token_logic).
-author("Konrad Zemek").

-include("dao/dao_types.hrl").


%% Atoms representing types of valid tokens.
-type token_type() :: group_invite_token | space_create_token |
    space_invite_user_token | space_invite_group_token | accounts_merge_token |
    space_support_token.


%% Atoms representing valid resource types.
-type resource_type() :: user | group | space.


%% API
-export([is_valid/2, create/2, consume/2]).
-export_type([token_type/0, resource_type/0]).


%% is_valid/2
%% ====================================================================
%% @doc Checks if a given token is a valid token of a given type.
%% ====================================================================
-spec is_valid(Token :: binary(), TokenType :: token_type()) ->
    boolean().
%% ====================================================================
is_valid(Token, TokenType) ->
    case decrypt(Token) of
        false -> false;
        {true, TokenId} ->
            case logic_helper:token_exists(TokenId) of
                false -> false;
                true ->
                    #token{type = Type} = logic_helper:token(TokenId), %% @todo: expiration time
                    Type =:= TokenType
            end
    end.


%% create/2
%% ====================================================================
%% @doc Creates a token of a given type.
%% ====================================================================
-spec create(TokenType :: token_type(), Resource :: {resource_type(), binary()}) ->
    {ok, Token :: binary()} | no_return().
%% ====================================================================
create(TokenType, Resource) ->
    TokenRec = #token{type = TokenType, resource = Resource}, %% @todo: expiration time
    TokenId = logic_helper:save(TokenRec),
    encrypt(TokenId).


%% consume/2
%% ====================================================================
%% @doc Consumes a token, returning associated resource.
%% ====================================================================
-spec consume(Token :: binary(), TokenType :: token_type()) ->
    {ok, {resource_type(), binary()}}.
%% ====================================================================
consume(Token, TokenType) ->
    {true, TokenId} = decrypt(Token),
    #token{type = TokenType, resource = Resource} = logic_helper:token(TokenId), %% @todo: expiration time
    logic_helper:token_remove(TokenId),
    {ok, Resource}.


%% encrypt/1
%% ====================================================================
%% @doc Encrypts a token with registry's public key.
%% ====================================================================
-spec encrypt(Token :: binary()) ->
    {ok, EncryptedToken :: binary()} | no_return().
%% ====================================================================
encrypt(Token) -> %% @todo: encryption
    {ok, Token}.


%% decrypt/1
%% ====================================================================
%% @doc Decrypts a token with registry's private key.
%% ====================================================================
-spec decrypt(EncryptedToken :: binary()) ->
    {true, Token :: binary()} | false.
%% ====================================================================
decrypt(Token) -> %% @todo: decryption
    {true, Token}.
