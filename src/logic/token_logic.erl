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
-export([named_token_to_token/2]).
-export([create_user_named_token/6, create_user_named_token/4]).
-export([create_provider_named_token/6, create_provider_named_token/4]).
-export([create_user_temporary_token/4, create_user_temporary_token/3]).
-export([create_provider_temporary_token/4, create_provider_temporary_token/3]).
-export([create_gui_access_token/4]).
-export([list/1]).
-export([list_user_named_tokens/2, list_provider_named_tokens/2]).
-export([get_named_token_by_nonce/2]).
-export([get_user_named_token/3, get_provider_named_token/3]).
-export([exists/1]).
-export([update_named_token_metadata_by_nonce/3]).
-export([update_user_named_token_metadata/4, update_provider_named_token_metadata/4]).
-export([toggle_named_token_revoked_by_nonce/3]).
-export([toggle_user_named_token_revoked/4, toggle_provider_named_token_revoked/4]).
-export([delete_named_token_by_nonce/2]).
-export([delete_user_named_token/3, delete_provider_named_token/3]).
-export([delete_all_user_named_tokens/2, delete_all_provider_named_tokens/2]).
-export([migrate_deprecated_tokens/0]).

-define(GUI_TOKEN_TTL, oz_worker:get_env(gui_token_ttl, 600)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reconstructs a token based on given named token (od_token) record and nonce.
%% @end
%%--------------------------------------------------------------------
-spec named_token_to_token(tokens:nonce(), od_token:record()) -> tokens:token().
named_token_to_token(TokenNonce, NamedToken) ->
    #od_token{
        version = Version,
        subject = Subject,
        type = Type,
        secret = Secret,
        caveats = Caveats
    } = NamedToken,
    Prototype = #token{
        version = Version,
        onezone_domain = oz_worker:get_domain(),
        nonce = TokenNonce,
        subject = Subject,
        type = Type,
        persistent = true
    },
    tokens:construct(Prototype, Secret, Caveats).


-spec create_user_named_token(aai:auth(), od_user:id(), od_token:name(),
    tokens:type(), [caveats:caveat()], od_token:metadata()) ->
    {ok, tokens:token()} | errors:error().
create_user_named_token(Auth, UserId, TokenName, Type, Caveats, Metadata) ->
    create_user_named_token(Auth, UserId, TokenName, #{
        <<"type">> => Type,
        <<"caveats">> => Caveats,
        <<"metadata">> => Metadata
    }).


-spec create_user_named_token(aai:auth(), od_user:id(), od_token:name(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_user_named_token(Auth, UserId, TokenName, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {user_named_token, UserId}},
        data = Data
    })).


-spec create_provider_named_token(aai:auth(), od_provider:id(), od_token:name(),
    tokens:type(), [caveats:caveat()], od_token:metadata()) ->
    {ok, tokens:token()} | errors:error().
create_provider_named_token(Auth, ProviderId, TokenName, Type, Caveats, Metadata) ->
    create_provider_named_token(Auth, ProviderId, TokenName, #{
        <<"type">> => Type,
        <<"caveats">> => Caveats,
        <<"metadata">> => Metadata
    }).


-spec create_provider_named_token(aai:auth(), od_provider:id(), od_token:name(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_provider_named_token(Auth, ProviderId, TokenName, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {provider_named_token, ProviderId}},
        data = Data
    })).


-spec create_user_temporary_token(aai:auth(), od_user:id(), tokens:type(), [caveats:caveat()]) ->
    {ok, tokens:token()} | errors:error().
create_user_temporary_token(Auth, UserId, Type, Caveats) ->
    create_user_temporary_token(Auth, UserId, #{
        <<"type">> => Type,
        <<"caveats">> => Caveats
    }).


-spec create_user_temporary_token(aai:auth(), od_user:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_user_temporary_token(Auth, UserId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_temporary_token, UserId}},
        data = Data
    })).


-spec create_provider_temporary_token(aai:auth(), od_provider:id(), tokens:type(), [caveats:caveat()]) ->
    {ok, tokens:token()} | errors:error().
create_provider_temporary_token(Auth, ProviderId, Type, Caveats) ->
    create_provider_temporary_token(Auth, ProviderId, #{
        <<"type">> => Type,
        <<"caveats">> => Caveats
    }).


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


-spec list(aai:auth()) -> {ok, [tokens:nonce()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = list}
    }).


-spec list_user_named_tokens(aai:auth(), od_user:id()) ->
    {ok, [{od_token:name(), tokens:nonce()}]} | errors:error().
list_user_named_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_named_tokens, UserId}}
    }).


-spec list_provider_named_tokens(aai:auth(), od_provider:id()) ->
    {ok, [{od_token:name(), tokens:nonce()}]} | errors:error().
list_provider_named_tokens(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ProviderId}}
    }).


-spec get_named_token_by_nonce(aai:auth(), tokens:nonce()) ->
    {ok, od_token:record()} | errors:error().
get_named_token_by_nonce(Auth, TokenNonce) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenNonce, aspect = instance}
    }).


-spec get_user_named_token(aai:auth(), od_user:id(), od_token:name()) ->
    {ok, od_token:record()} | errors:error().
get_user_named_token(Auth, UserId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {user_named_token, UserId}}
    }).


-spec get_provider_named_token(aai:auth(), od_provider:id(), od_token:name()) ->
    {ok, od_token:record()} | errors:error().
get_provider_named_token(Auth, ProviderId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {provider_named_token, ProviderId}}
    }).


-spec exists(tokens:nonce()) -> boolean().
exists(TokenNonce) ->
    {ok, Exists} = od_token:exists(TokenNonce),
    Exists.


-spec update_named_token_metadata_by_nonce(aai:auth(), tokens:nonce(),
    entity_logic:data() | od_token:metadata()) -> ok | errors:error().
update_named_token_metadata_by_nonce(Auth, TokenNonce, Data = #{<<"metadata">> := _}) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenNonce, aspect = instance},
        data = Data
    });
update_named_token_metadata_by_nonce(Auth, TokenNonce, Metadata) ->
    update_named_token_metadata_by_nonce(Auth, TokenNonce, #{<<"metadata">> => Metadata}).


-spec update_user_named_token_metadata(aai:auth(), od_user:id(), od_token:name(),
    entity_logic:data() | od_token:metadata()) -> ok | errors:error().
update_user_named_token_metadata(Auth, UserId, TokenName, Data = #{<<"metadata">> := _}) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {user_named_token, UserId}},
        data = Data
    });
update_user_named_token_metadata(Auth, UserId, TokenName, Metadata) ->
    update_user_named_token_metadata(Auth, UserId, TokenName, #{<<"metadata">> => Metadata}).


-spec update_provider_named_token_metadata(aai:auth(), od_provider:id(), od_token:name(),
    entity_logic:data() | od_token:metadata()) -> ok | errors:error().
update_provider_named_token_metadata(Auth, ProviderId, TokenName, Data = #{<<"metadata">> := _}) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {provider_named_token, ProviderId}},
        data = Data
    });
update_provider_named_token_metadata(Auth, ProviderId, TokenName, Metadata) ->
    update_provider_named_token_metadata(Auth, ProviderId, TokenName, #{<<"metadata">> => Metadata}).


-spec toggle_named_token_revoked_by_nonce(aai:auth(), tokens:nonce(),
    entity_logic:data() | boolean()) -> ok | errors:error().
toggle_named_token_revoked_by_nonce(Auth, TokenNonce, Revoked) when is_boolean(Revoked) ->
    toggle_named_token_revoked_by_nonce(Auth, TokenNonce, #{<<"revoked">> => Revoked});
toggle_named_token_revoked_by_nonce(Auth, TokenNonce, Data) when is_map(Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenNonce, aspect = instance},
        data = Data
    }).


-spec toggle_user_named_token_revoked(aai:auth(), od_user:id(), od_token:name(),
    entity_logic:data() | boolean()) -> ok | errors:error().
toggle_user_named_token_revoked(Auth, UserId, TokenName, Revoked) when is_boolean(Revoked) ->
    toggle_user_named_token_revoked(Auth, UserId, TokenName, #{<<"revoked">> => Revoked});
toggle_user_named_token_revoked(Auth, UserId, TokenName, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {user_named_token, UserId}},
        data = Data
    }).


-spec toggle_provider_named_token_revoked(aai:auth(), od_provider:id(), od_token:name(),
    entity_logic:data() | boolean()) -> ok | errors:error().
toggle_provider_named_token_revoked(Auth, ProviderId, TokenName, Revoked) when is_boolean(Revoked) ->
    toggle_provider_named_token_revoked(Auth, ProviderId, TokenName, #{<<"revoked">> => Revoked});
toggle_provider_named_token_revoked(Auth, ProviderId, TokenName, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {provider_named_token, ProviderId}},
        data = Data
    }).


-spec delete_named_token_by_nonce(aai:auth(), tokens:nonce()) -> ok | errors:error().
delete_named_token_by_nonce(Auth, TokenNonce) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenNonce, aspect = instance}
    }).


-spec delete_user_named_token(aai:auth(), od_user:id(), od_token:name()) -> ok | errors:error().
delete_user_named_token(Auth, UserId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {user_named_token, UserId}}
    }).


-spec delete_provider_named_token(aai:auth(), od_provider:id(), od_token:name()) -> ok | errors:error().
delete_provider_named_token(Auth, ProviderId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {provider_named_token, ProviderId}}
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
    lists:foreach(fun(#document{key = ProviderId, value = #od_provider{root_token = RootTokenNonce}}) ->
        try
            migrate_provider_root_token(ProviderId, RootTokenNonce)
        catch Type:Message ->
            ?warning_stacktrace("Failed to migrate provider root token, ProviderId: ~s, Nonce: ~s - ~p:~p", [
                ProviderId, RootTokenNonce, Type, Message
            ])
        end
    end, ProviderDocs).


-spec migrate_user_token(od_user:id(), tokens:serialized(), Counter :: integer()) -> NewCounter :: integer().
migrate_user_token(UserId, Serialized, Counter) ->
    {ok, #token{nonce = Nonce, macaroon = Macaroon}} = tokens:deserialize(Serialized),
    Caveats = macaroon:first_party_caveats(Macaroon),
    {ok, #document{value = #onedata_auth{secret = Secret}}} = onedata_auth:get(Nonce),
    TokenName = ?LEGACY_CLIENT_TOKEN_NAME(Counter),
    % Do not check results - it is possible that some of the records
    % already exist if the upgrade procedure was interrupted and repeated
    od_token:create(#document{key = Nonce, value = #od_token{
        name = TokenName,
        version = 1,
        subject = ?SUB(user, UserId),
        type = ?ACCESS_TOKEN,
        caveats = Caveats,
        secret = Secret
    }}),
    named_tokens:add(?SUB(user, UserId), TokenName, Nonce),
    od_user:update(UserId, fun(User = #od_user{client_tokens = ClientTokens}) ->
        {ok, User#od_user{client_tokens = lists:delete(Serialized, ClientTokens)}}
    end),
    onedata_auth:delete(Nonce),
    Counter + 1.


%% @private
-spec migrate_provider_root_token(od_provider:id(), tokens:nonce()) -> ok.
migrate_provider_root_token(ProviderId, RootTokenNonce) ->
    {ok, Secret, _} = macaroon_auth:get(RootTokenNonce),
    TokenName = ?PROVIDER_ROOT_TOKEN_NAME,
    % Do not check results - it is possible that some of the records
    % already exist if the upgrade procedure was interrupted and repeated
    od_token:create(#document{key = RootTokenNonce, value = #od_token{
        name = TokenName,
        version = 1,
        subject = ?SUB(?ONEPROVIDER, ProviderId),
        type = ?ACCESS_TOKEN,
        caveats = [],
        secret = Secret
    }}),
    named_tokens:add(?SUB(?ONEPROVIDER, ProviderId), TokenName, RootTokenNonce),
    macaroon_auth:delete(RootTokenNonce).
