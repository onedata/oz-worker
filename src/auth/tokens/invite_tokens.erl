%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @todo VFS-5554 This module is deprecated, kept for backward compatibility
%%% @doc
%%% The module implementing the business logic for tokens created by users.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(invite_tokens).
-author("Konrad Zemek").

-include("invite_tokens.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/logging.hrl").

%% Atoms representing types of valid tokens.
-type token_type() :: ?GROUP_INVITE_USER_TOKEN | ?GROUP_INVITE_GROUP_TOKEN |
?SPACE_INVITE_USER_TOKEN | ?SPACE_INVITE_GROUP_TOKEN |
?SPACE_SUPPORT_TOKEN | ?PROVIDER_REGISTRATION_TOKEN |
?HARVESTER_INVITE_USER_TOKEN | ?HARVESTER_INVITE_GROUP_TOKEN |
?CLUSTER_INVITE_USER_TOKEN | ?CLUSTER_INVITE_GROUP_TOKEN.

%% Atoms representing valid resource types.
-type resource_type() :: od_user | od_group | od_space | od_provider | od_harvester | od_cluster.

-export_type([token_type/0, resource_type/0]).

%% API
-export([validate/2, create/3, consume/2, delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a given token is a valid macaroon of a given type.
%% @end
%%--------------------------------------------------------------------
-spec validate(macaroon:macaroon(), TokenType :: token_type()) ->
    ok | inexistent | bad_macaroon | bad_type.
validate(Macaroon, TokenType) ->
    try
        Id = macaroon:identifier(Macaroon),
        case token:get(Id) of
            {error, _} ->
                inexistent;
            {ok, #document{value = #token{secret = Secret}}} ->
                V = macaroon_verifier:create(),
                V1 = macaroon_verifier:satisfy_exact(V,
                    ["tokenType = ", atom_to_list(TokenType)]),

                case macaroon_verifier:verify(V1, Macaroon, Secret) of
                    ok ->
                        ok;
                    {error, Reason} ->
                        ?debug("Bad macaroon ~p: ~p", [Id, Reason]),
                        bad_type
                end
        end
    catch
        _:_ ->
            bad_macaroon
    end.


%%--------------------------------------------------------------------
%% @doc Creates a macaroon token of a given type.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create(Issuer :: aai:auth(), TokenType :: token_type(),
    Resource :: {resource_type(), binary()}) ->
    {ok, macaroon:macaroon()} | {error, Reason :: any()}.
create(Issuer, TokenType, {ResourceType, ResourceId}) ->
    Secret = crypto:strong_rand_bytes(macaroon:suggested_secret_length()),
    TokenData = #token{secret = Secret, issuer = Issuer#auth.subject,
        resource = ResourceType, resource_id = ResourceId},

    {ok, #document{key = Identifier}} = token:save(#document{value = TokenData}),

    M1 = macaroon:create(oz_worker:get_domain(), Secret, Identifier),
    M2 = macaroon:add_first_party_caveat(M1,
        ["tokenType = ", atom_to_list(TokenType)]),
    {ok, M2}.


%%--------------------------------------------------------------------
%% @doc 
%% Token is consumed only if ConsumeFun succeeds.
%% Throws exception when call to the datastore fails or token doesn't exist in db.
%% Returns value returned by ConsumeFun.
%% @end
%%--------------------------------------------------------------------
-spec consume(Macaroon :: macaroon:macaroon(), 
    ConsumeFun :: fun((entity_logic:entity_type(), entity_logic:entity_id()) -> term())) ->
    term().
consume(M, ConsumeFun) ->
    case set_locked(M, true) of
        {error, already_locked} -> throw(?ERROR_TOKEN_INVALID);
        {ok, TokenDoc} ->
            #document{value = #token{resource = ResourceType,
                resource_id = ResourceId}} = TokenDoc,
            try 
                Result = ConsumeFun(ResourceType, ResourceId),
                delete(M),
                Result
            after 
                % Unlock token if operation failed. 
                % If token is deleted update will silently fail.
                set_locked(M, false)
            end
    end.


%%--------------------------------------------------------------------
%% @doc Deletes a client token.
%% Throws exception when call to the datastore fails, or token doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec delete(Macaroon :: macaroon:macaroon()) -> ok.
delete(M) ->
    Identifier = macaroon:identifier(M),
    ok = token:delete(Identifier).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 
%% Locks token by id.
%% @end
%%--------------------------------------------------------------------
-spec set_locked(M :: macaroon:macaroon(), Value :: boolean()) -> 
    {ok, token:record()} | {error, term()}.
set_locked(M, Value) ->
    TokenId = macaroon:identifier(M),
    token:update(TokenId, fun(Token) ->
        case {Token#token.locked, Value} of
            {true, true} ->
                {error, already_locked};
            _ ->
                {ok, Token#token{locked = Value}}
        end
    end).
