%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all token logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(token_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

%% API
-export([examine/2]).
-export([confine/2]).
-export([verify_access_token/2]).
-export([verify_identity_token/2]).
-export([verify_invite_token/2]).
-export([create_user_named_token/3, create_provider_named_token/3]).
-export([create_user_temporary_token/3, create_provider_temporary_token/3]).
-export([create_gui_access_token/4]).
-export([list/1]).
-export([list_user_named_tokens/2, list_provider_named_tokens/2]).
-export([get_named_token/2]).
-export([get_user_named_token_by_name/3]).
-export([get_provider_named_token_by_name/3]).
-export([exists/1]).
-export([update_named_token/3]).
-export([delete_named_token/2]).
-export([delete_all_user_named_tokens/2, delete_all_provider_named_tokens/2]).
-export([revoke_all_user_temporary_tokens/2, revoke_all_provider_temporary_tokens/2]).
-export([create_legacy_invite_token/3]).
-export([migrate_deprecated_tokens/0]).

-define(GUI_TOKEN_TTL, oz_worker:get_env(gui_token_ttl, 600)).

%%%===================================================================
%%% API
%%%===================================================================

-spec examine(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
examine(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = examine, scope = public},
        data = Data
    })).


-spec confine(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
confine(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = confine, scope = public},
        data = Data
    })).


-spec verify_access_token(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
verify_access_token(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = verify_access_token, scope = public},
        data = Data
    })).


-spec verify_identity_token(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
verify_identity_token(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = verify_identity_token, scope = public},
        data = Data
    })).


-spec verify_invite_token(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
verify_invite_token(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = verify_invite_token, scope = public},
        data = Data
    })).


-spec create_user_named_token(aai:auth(), od_user:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_user_named_token(Auth, UserId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_named_token, UserId}},
        data = Data
    })).


-spec create_provider_named_token(aai:auth(), od_provider:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_provider_named_token(Auth, ProviderId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_named_token, ProviderId}},
        data = Data
    })).


-spec create_user_temporary_token(aai:auth(), od_user:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_user_temporary_token(Auth, UserId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_temporary_token, UserId}},
        data = Data
    })).


-spec create_provider_temporary_token(aai:auth(), od_provider:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_provider_temporary_token(Auth, ProviderId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_temporary_token, ProviderId}},
        data = Data
    })).


-spec create_gui_access_token(aai:auth(), od_user:id(), session:id(), aai:audience()) ->
    {ok, {tokens:token(), time_utils:seconds()}} | errors:error().
create_gui_access_token(Auth, UserId, SessionId, Audience) ->
    Ttl = ?GUI_TOKEN_TTL,
    Result = create_user_temporary_token(Auth, UserId, #{
        <<"type">> => ?GUI_ACCESS_TOKEN(SessionId),
        <<"caveats">> => [
            #cv_time{valid_until = time_utils:cluster_time_seconds() + Ttl},
            #cv_audience{whitelist = [Audience]}
        ]
    }),
    case Result of
        {ok, Token} -> {ok, {Token, Ttl}};
        {error, _} = Error -> Error
    end.


-spec list(aai:auth()) -> {ok, [tokens:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = list}
    }).


-spec list_user_named_tokens(aai:auth(), od_user:id()) ->
    {ok, [tokens:id()]} | errors:error().
list_user_named_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_named_tokens, UserId}}
    }).


-spec list_provider_named_tokens(aai:auth(), od_provider:id()) ->
    {ok, [tokens:id()]} | errors:error().
list_provider_named_tokens(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ProviderId}}
    }).


-spec get_named_token(aai:auth(), tokens:id()) ->
    {ok, od_token:record()} | errors:error().
get_named_token(Auth, TokenId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenId, aspect = instance}
    }).


-spec get_user_named_token_by_name(aai:auth(), od_user:id(), od_token:name()) ->
    {ok, od_token:record()} | errors:error().
get_user_named_token_by_name(Auth, UserId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {user_named_token, UserId}}
    }).


-spec get_provider_named_token_by_name(aai:auth(), od_provider:id(), od_token:name()) ->
    {ok, od_token:record()} | errors:error().
get_provider_named_token_by_name(Auth, ProviderId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {provider_named_token, ProviderId}}
    }).


-spec exists(tokens:id()) -> boolean().
exists(TokenId) ->
    {ok, Exists} = od_token:exists(TokenId),
    Exists.


-spec update_named_token(aai:auth(), tokens:id(), entity_logic:data()) ->
    ok | errors:error().
update_named_token(Auth, TokenId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenId, aspect = instance},
        data = Data
    }).


-spec delete_named_token(aai:auth(), tokens:id()) -> ok | errors:error().
delete_named_token(Auth, TokenId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenId, aspect = instance}
    }).


-spec delete_all_user_named_tokens(aai:auth(), od_user:id()) -> ok | errors:error().
delete_all_user_named_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_named_tokens, UserId}}
    }).


-spec delete_all_provider_named_tokens(aai:auth(), od_provider:id()) -> ok | errors:error().
delete_all_provider_named_tokens(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ProviderId}}
    }).


-spec revoke_all_user_temporary_tokens(aai:auth(), od_user:id()) -> ok | errors:error().
revoke_all_user_temporary_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_temporary_tokens, UserId}}
    }).


-spec revoke_all_provider_temporary_tokens(aai:auth(), od_provider:id()) -> ok | errors:error().
revoke_all_provider_temporary_tokens(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_temporary_tokens, ProviderId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 19.09.*
%% Function used from *_logic_plugin modules to create legacy invite tokens.
%% @end
%%--------------------------------------------------------------------
-spec create_legacy_invite_token(aai:auth(), tokens:invite_token_type(), gri:entity_id()) ->
    entity_logic:create_result().
create_legacy_invite_token(Auth, InviteTokenType, TargetEntityId) ->
    Data = #{
        <<"name">> => ?INVITE_TOKEN_NAME(InviteTokenType),
        <<"type">> => ?INVITE_TOKEN(InviteTokenType, TargetEntityId),
        <<"usageLimit">> => 1
    },
    Result = case Auth#auth.subject of
        ?SUB(user, UserId) -> create_user_named_token(Auth, UserId, Data);
        ?SUB(?ONEPROVIDER, PrId) -> create_provider_named_token(Auth, PrId, Data)
    end,
    case Result of
        {ok, #{<<"token">> := Token}} -> {ok, value, Token};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Migrates deprecated user and provider client tokens to the new model.
%% Dedicated for upgrading Onezone from 19.02.* to the next major release.
%% @end
%%--------------------------------------------------------------------
-spec migrate_deprecated_tokens() -> ok.
migrate_deprecated_tokens() ->
    ?info("Migrating legacy user tokens..."),
    migrate_user_tokens(),
    ?notice("Successfully migrated legacy user tokens"),
    ?info("Migrating legacy provider tokens..."),
    migrate_provider_root_tokens(),
    ?notice("Successfully migrated legacy provider tokens").

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec migrate_user_tokens() -> ok.
migrate_user_tokens() ->
    {ok, UserDocs} = od_user:list(),
    lists:foreach(fun(#document{key = UserId, value = #od_user{client_tokens = ClientTokens}}) ->
        lists:foldl(fun(Serialized, Counter) ->
            try
                migrate_user_token(UserId, Serialized, Counter)
            catch Type:Message ->
                ?warning_stacktrace("Failed to migrate user token, UserId: ~s, Token: ~s... - ~p:~p", [
                    UserId, binary:part(Serialized, 0, 30), Type, Message
                ])
            end
        end, 1, ClientTokens)
    end, UserDocs).


%% @private
-spec migrate_provider_root_tokens() -> ok.
migrate_provider_root_tokens() ->
    {ok, ProviderDocs} = od_provider:list(),
    lists:foreach(fun(#document{key = ProviderId, value = #od_provider{root_token = RootTokenId}}) ->
        try
            migrate_provider_root_token(ProviderId, RootTokenId)
        catch Type:Message ->
            ?warning_stacktrace("Failed to migrate provider root token, ProviderId: ~s, Id: ~s - ~p:~p", [
                ProviderId, RootTokenId, Type, Message
            ])
        end
    end, ProviderDocs).


-spec migrate_user_token(od_user:id(), tokens:serialized(), Counter :: integer()) -> NewCounter :: integer().
migrate_user_token(UserId, Serialized, Counter) ->
    {ok, #token{id = TokenId, macaroon = Macaroon}} = tokens:deserialize(Serialized),
    Caveats = macaroon:first_party_caveats(Macaroon),
    {ok, #document{value = #onedata_auth{secret = Secret}}} = onedata_auth:get(TokenId),
    TokenName = ?LEGACY_CLIENT_TOKEN_NAME(Counter),
    % Do not check results - it is possible that some of the records
    % already exist if the upgrade procedure was interrupted and repeated
    od_token:create(#document{key = TokenId, value = #od_token{
        name = TokenName,
        version = 1,
        subject = ?SUB(user, UserId),
        type = ?ACCESS_TOKEN,
        caveats = [caveats:deserialize(C) || C <- Caveats],
        metadata = token_metadata:build(?ACCESS_TOKEN, #{}, #{}),
        secret = Secret
    }}),
    token_names:register(?SUB(user, UserId), TokenName, TokenId),
    od_user:update(UserId, fun(User = #od_user{client_tokens = ClientTokens}) ->
        {ok, User#od_user{client_tokens = lists:delete(Serialized, ClientTokens)}}
    end),
    onedata_auth:delete(TokenId),
    Counter + 1.


%% @private
-spec migrate_provider_root_token(od_provider:id(), tokens:id()) -> ok.
migrate_provider_root_token(ProviderId, RootTokenId) ->
    {ok, Secret, _} = macaroon_auth:get(RootTokenId),
    TokenName = ?PROVIDER_ROOT_TOKEN_NAME,
    % Do not check results - it is possible that some of the records
    % already exist if the upgrade procedure was interrupted and repeated
    od_token:create(#document{key = RootTokenId, value = #od_token{
        name = TokenName,
        version = 1,
        subject = ?SUB(?ONEPROVIDER, ProviderId),
        type = ?ACCESS_TOKEN,
        caveats = [],
        metadata = token_metadata:build(?ACCESS_TOKEN, #{}, #{}),
        secret = Secret
    }}),
    token_names:register(?SUB(?ONEPROVIDER, ProviderId), TokenName, RootTokenId),
    macaroon_auth:delete(RootTokenId).
