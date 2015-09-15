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

-define(DB(Function, Arg), dao_lib:apply(dao_tokens, Function, [Arg], 1)).

%% Atoms representing types of valid tokens.
-type token_type() :: group_invite_token | space_create_token |
space_invite_user_token | space_invite_group_token | accounts_merge_token |
space_support_token.

%% Atoms representing valid resource types.
-type resource_type() :: user | group | space.

%% API
-export([validate/2, create/2, consume/1]).
-export_type([token_type/0, resource_type/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a given token is a valid macaroon of a given type.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec validate(Token :: binary(), TokenType :: token_type()) ->
    {true, macaroon:macaroon()} | false.
validate(Token, TokenType) ->
    case macaroon:deserialize(Token) of
        {error, _} -> false;
        {ok, M} ->
            case ?DB(get_token, macaroon:identifier(M)) of
                {error, _} -> false;
                {ok, #db_document{record = #token{secret = Secret}}} ->
                    {ok, V} = macaroon_verifier:create(),
                    ok = macaroon_verifier:satisfy_exact(V,
                        ["tokenType = ", atom_to_list(TokenType)]),

                    case macaroon_verifier:verify(V, M, Secret) of
                        ok -> {true, M};
                        _ -> false
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Creates a macaroon token of a given type.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec create(TokenType :: token_type(), Resource :: {resource_type(), binary()}) ->
    {ok, Token :: binary()} | {error, Reason :: any()}.
create(TokenType, {ResourceType, ResourceId}) ->
    Secret = crypto:rand_bytes(macaroon:suggested_secret_length()),
    TokenData = #token{secret = Secret,
        resource = ResourceType, resource_id = ResourceId},

    {ok, Identifier} = ?DB(save_token, TokenData),

    % @todo expiration time
    {ok, M1} = macaroon:create("registry", Secret, Identifier),
    {ok, M2} = macaroon:add_first_party_caveat(M1,
        ["tokenType = ", atom_to_list(TokenType)]),

    macaroon:serialize(M2).

%%--------------------------------------------------------------------
%% @doc Consumes a token, returning associated resource.
%% Throws exception when call to dao fails, or token doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec consume(Macaroon :: macaroon:macaroon()) ->
    {ok, {resource_type(), binary()}}.
consume(M) ->
    {ok, Identifier} = macaroon:identifier(M),
    {ok, TokenDoc} = ?DB(get_token, Identifier),
    #db_document{record = #token{resource = ResourceType,
        resource_id = ResourceId}} = TokenDoc,

    ok = ?DB(remove_token, Identifier),
    {ok, {ResourceType, ResourceId}}.

