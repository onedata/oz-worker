%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating tokens of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_tokens).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([create/1, create/2, create/3, create/4]).
-export([try_create/3, try_create/4]).
-export([create_gui_access_token/3]).
-export([build_auth_ctx/1]).
-export([authenticate/1, authenticate/2]).
-export([verify_audience_token/2]).
-export([verify/1]).
-export([confine/2]).
-export([toggle_revoked/2]).
-export([revoke_all_temporary_tokens/1]).
-export([ensure_time_caveat/1]).
-export([ensure_serialized/1, ensure_deserialized/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(aai:subject()) -> tokens:token().
create(Subject) ->
    create(temporary, Subject).

-spec create(named | temporary, aai:subject()) -> tokens:token().
create(Persistence, Subject) ->
    create(Persistence, Subject, ?ACCESS_TOKEN).

-spec create(named | temporary, aai:subject(), tokens:type() | entity_logic:data()) ->
    tokens:token().
create(Persistence, Subject, Data) when is_map(Data) ->
    {ok, Token} = ?assertMatch({ok, _}, try_create(Persistence, Subject, Data)),
    Token;
create(Persistence, Subject, Type) ->
    create(Persistence, Subject, Type, []).

-spec create(named | temporary, aai:subject(), tokens:type(), [caveats:caveat()]) ->
    tokens:token().
create(Persistence, Subject, Type, Caveats) ->
    create(Persistence, Subject, #{<<"type">> => Type, <<"caveats">> => Caveats}).


-spec try_create(named | temporary, aai:subject(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
try_create(Persistence, Subject, Data) ->
    try_create(#auth{subject = Subject}, Persistence, Subject, Data).

-spec try_create(aai:auth(), named | temporary, aai:subject(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
try_create(Auth, named, ?SUB(user, UserId), Data) ->
    DataWithName = case maps:is_key(<<"name">>, Data) of
        true -> Data;
        false -> Data#{<<"name">> => <<"us-", UserId/binary, (str_utils:rand_hex(4))/binary>>}
    end,
    ozt:rpc(token_logic, create_user_named_token, [Auth, UserId, DataWithName]);
try_create(Auth, temporary, ?SUB(user, UserId), Data) ->
    ozt:rpc(token_logic, create_user_temporary_token, [Auth, UserId, Data#{
        <<"caveats">> => ensure_time_caveat(maps:get(<<"caveats">>, Data, []))
    }]);
try_create(Auth, named, ?SUB(?ONEPROVIDER, PrId), Data) ->
    DataWithName = case maps:is_key(<<"name">>, Data) of
        true -> Data;
        false -> Data#{<<"name">> => <<"pr-", PrId/binary, (str_utils:rand_hex(4))/binary>>}
    end,
    ozt:rpc(token_logic, create_provider_named_token, [Auth, PrId, DataWithName]);
try_create(Auth, temporary, ?SUB(?ONEPROVIDER, ProviderId), Data) ->
    ozt:rpc(token_logic, create_provider_temporary_token, [Auth, ProviderId, Data#{
        <<"caveats">> => ensure_time_caveat(maps:get(<<"caveats">>, Data, []))
    }]).


-spec create_gui_access_token(od_user:id(), session:id(), aai:audience()) -> tokens:token().
create_gui_access_token(UserId, SessionId, Audience) ->
    {ok, {GuiAccessToken, _}} = ?assertMatch({ok, _}, ozt:rpc(token_logic, create_gui_access_token, [
        ?USER(UserId), UserId, SessionId, Audience
    ])),
    GuiAccessToken.


% See token_auth:build_auth_ctx for args
-spec build_auth_ctx([term()]) -> aai:auth_ctx().
build_auth_ctx(Args) ->
    ozt:rpc(token_auth, build_auth_ctx, Args).


-spec authenticate(tokens:token() | tokens:serialized()) ->
    {true, aai:auth()} | errors:error().
authenticate(Token) ->
    authenticate(Token, build_auth_ctx([undefined])).

-spec authenticate(tokens:token() | tokens:serialized(), aai:auth_ctx()) ->
    {true, aai:auth()} | errors:error().
authenticate(Token, AuthCtx) ->
    ozt:rpc(token_auth, authenticate, [Token, AuthCtx]).


-spec verify_audience_token(tokens:token() | tokens:serialized(), aai:auth_ctx()) ->
    {ok, aai:audience()} | errors:error().
verify_audience_token(Token, AuthCtx) ->
    ozt:rpc(token_auth, verify_audience_token, [Token, AuthCtx]).


-spec verify(tokens:token()) -> {ok, entity_logic:data()} | errors:error().
verify(Token = #token{type = ?ACCESS_TOKEN}) ->
    ozt:rpc(token_logic, verify_access_token, [?NOBODY, #{<<"token">> => Token}]);
verify(Token = #token{type = ?GUI_ACCESS_TOKEN(_)}) ->
    ozt:rpc(token_logic, verify_access_token, [?NOBODY, #{<<"token">> => Token}]);
verify(Token = #token{type = ?INVITE_TOKEN(_, _)}) ->
    ozt:rpc(token_logic, verify_invite_token, [?NOBODY, #{<<"token">> => Token}]).


-spec confine(tokens:token() | tokens:serialized(), [caveats:caveat()]) ->
    {ok, tokens:token() | tokens:serialized()} | errors:error().
confine(Token, Caveats) ->
    ozt:rpc(token_logic, confine, [?ROOT, #{<<"token">> => Token, <<"caveats">> => Caveats}]).


-spec toggle_revoked(tokens:token(), boolean()) -> ok.
toggle_revoked(#token{id = TokenId}, Revoked) ->
    ?assertMatch(ok, ozt:rpc(token_logic, update_named_token, [?ROOT, TokenId, #{<<"revoked">> => Revoked}])).


-spec revoke_all_temporary_tokens(aai:subject()) -> ok.
revoke_all_temporary_tokens(?SUB(user, UserId)) ->
    ?assertMatch(ok, ozt:rpc(token_logic, revoke_all_user_temporary_tokens, [?USER(UserId), UserId]));
revoke_all_temporary_tokens(?SUB(?ONEPROVIDER, PrId)) ->
    ?assertMatch(ok, ozt:rpc(token_logic, revoke_all_provider_temporary_tokens, [?PROVIDER(PrId), PrId])).


-spec ensure_time_caveat([caveats:caveat()]) -> [caveats:caveat()].
ensure_time_caveat(Caveats) ->
    case caveats:find(cv_time, Caveats) of
        false -> [#cv_time{valid_until = ozt:cluster_time_seconds() + ?DEFAULT_TEMP_CAVEAT_TTL} | Caveats];
        {true, _} -> Caveats
    end.


-spec ensure_serialized(tokens:token() | tokens:serialized()) -> tokens:serialized().
ensure_serialized(Serialized) when is_binary(Serialized) ->
    Serialized;
ensure_serialized(Token) ->
    {ok, Serialized} = ?assertMatch({ok, _}, tokens:serialize(Token)),
    Serialized.


-spec ensure_deserialized(tokens:token() | tokens:serialized()) -> tokens:token().
ensure_deserialized(Serialized) when is_binary(Serialized) ->
    {ok, Token} = ?assertMatch({ok, _}, tokens:deserialize(Serialized)),
    Token;
ensure_deserialized(Token) ->
    Token.
