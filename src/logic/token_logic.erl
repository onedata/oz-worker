%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for tokens created by users.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(token_logic).
-author("Konrad Zemek").

-include("registered_names.hrl").
-include("dao/dao_types.hrl").

%% Length of generated tokens, before base64.
-define(TOKEN_LENGTH, 30). %% 30-byte sequence encodes into base64 without padding

-define(DB(Function, Arg), dao_lib:apply(dao_tokens, Function, [Arg], 1)).

%% Atoms representing types of valid tokens.
-type token_type() :: group_invite_token | space_create_token |
space_invite_user_token | space_invite_group_token | accounts_merge_token |
space_support_token.

%% Atoms representing valid resource types.
-type resource_type() :: user | group | space.

%% API
-export([is_valid/2, create/2, consume/2, random_token/0]).
-export_type([token_type/0, resource_type/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a given token is a valid token of a given type.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec is_valid(Token :: binary(), TokenType :: token_type()) ->
    boolean().
is_valid(Token, TokenType) -> %% @todo: expiration time
    case ?DB(get_token_by_value, Token) of
        {ok, #db_document{record = #token{type = TokenType}}} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Creates a token of a given type.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec create(TokenType :: token_type(), Resource :: {resource_type(), binary()}) ->
    {ok, Token :: binary()}.
create(TokenType, Resource) ->
    Token = random_token(),
    TokenRec = #token{value = Token, type = TokenType, resource = Resource}, %% @todo: expiration time
    {ok, _} = ?DB(save_token, TokenRec),
    {ok, Token}.

%%--------------------------------------------------------------------
%% @doc Consumes a token, returning associated resource.
%% Throws exception when call to dao fails, or token doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec consume(Token :: binary(), TokenType :: token_type()) ->
    {ok, {resource_type(), binary()}}.
consume(Token, TokenType) ->
    {ok, TokenDoc} = ?DB(get_token_by_value, Token), %% @todo: expiration time
    #db_document{uuid = TokenId,
        record = #token{type = TokenType, resource = Resource}} = TokenDoc,

    ok = ?DB(remove_token, TokenId),
    {ok, Resource}.

%%--------------------------------------------------------------------
%% @doc Generates a globally unique random token.
%%--------------------------------------------------------------------
-spec random_token() -> binary().
random_token() ->
    mochiweb_base64url:encode([crypto:rand_bytes(?TOKEN_LENGTH)]).
