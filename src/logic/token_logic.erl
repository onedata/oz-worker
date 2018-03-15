%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2014 ACK CYFRONET AGH
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

-include("tokens.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% Atoms representing types of valid tokens.
-type token_type() :: ?GROUP_INVITE_USER_TOKEN | ?GROUP_INVITE_GROUP_TOKEN |
?SPACE_INVITE_USER_TOKEN | ?SPACE_INVITE_GROUP_TOKEN |
?SPACE_SUPPORT_TOKEN | ?PROVIDER_REGISTRATION_TOKEN.

%% Atoms representing valid resource types.
-type resource_type() :: od_user | od_group | od_space | od_provider.

-export_type([token_type/0, resource_type/0]).

%% API
-export([serialize/1, deserialize/1]).
-export([validate/2, create/3, get_issuer/1, consume/1, delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Serializes a macaroon into a binary token.
%% @end
%%--------------------------------------------------------------------
-spec serialize(macaroon:macaroon()) -> {ok, Token :: binary()} | {error, term()}.
serialize(Macaroon) ->
    onedata_macaroons:serialize(Macaroon).


%%--------------------------------------------------------------------
%% @doc
%% Deserializes a macaroon from a binary token.
%% @end
%%--------------------------------------------------------------------
-spec deserialize(Token :: binary()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
deserialize(Token) ->
    onedata_macaroons:deserialize(Token).


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
-spec create(Issuer :: entity_logic:client(), TokenType :: token_type(),
    Resource :: {resource_type(), binary()}) ->
    {ok, macaroon:macaroon()} | {error, Reason :: any()}.
create(Issuer, TokenType, {ResourceType, ResourceId}) ->
    Secret = crypto:strong_rand_bytes(macaroon:suggested_secret_length()),
    TokenData = #token{secret = Secret, issuer = Issuer,
        resource = ResourceType, resource_id = ResourceId},

    {ok, #document{key = Identifier}} = token:save(#document{value = TokenData}),

    % @todo expiration time
    M1 = macaroon:create("onezone", Secret, Identifier),
    M2 = macaroon:add_first_party_caveat(M1,
        ["tokenType = ", atom_to_list(TokenType)]),
    {ok, M2}.


%%--------------------------------------------------------------------
%% @doc Returns token issuer.
%% Throws exception when the token is invalid, a call to dao fails,
%% or token doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec get_issuer(macaroon:macaroon()) -> {ok, maps:map()}.
get_issuer(Macaroon) ->
    Identifier = macaroon:identifier(Macaroon),
    {ok, TokenDoc} = token:get(Identifier),
    #document{value = #token{
        issuer = #client{type = ClientType, id = ClientId}
    }} = TokenDoc,

    {ok, #{
        <<"clientType">> => ClientType,
        <<"clientId">> => ClientId
    }}.


%%--------------------------------------------------------------------
%% @doc Consumes a token, returning associated resource.
%% Throws exception when call to the datastore fails, or token doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec consume(Macaroon :: macaroon:macaroon()) ->
    {ok, {resource_type(), undefined | binary()}}.
consume(M) ->
    Identifier = macaroon:identifier(M),
    {ok, TokenDoc} = token:get(Identifier),
    #document{value = #token{resource = ResourceType,
        resource_id = ResourceId}} = TokenDoc,

    ok = token:delete(Identifier),
    {ok, {ResourceType, ResourceId}}.


%%--------------------------------------------------------------------
%% @doc Deletes a client token.
%% Throws exception when call to the datastore fails, or token doesn't exist in db.
%% @end
%%--------------------------------------------------------------------
-spec delete(Macaroon :: macaroon:macaroon()) -> ok.
delete(M) ->
    Identifier = macaroon:identifier(M),
    ok = token:delete(Identifier).