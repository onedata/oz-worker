%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This model holds information connected to state tokens. They are used to
%%% match together OIDC/SAML requests and responses and protect against replay
%%% attacks. The info contains for example to which IdP the client was
%%% redirected.
%%% @end
%%%-------------------------------------------------------------------
-module(state_token).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").

-define(STATE_TOKEN_TTL, oz_worker:get_env(state_token_ttl_secs, 300)).

%% API
-export([create/1]).
-export([lookup/1]).

%% model_behaviour callbacks
-export([init/0]).

-type id() :: binary().
-type record() :: #state_token{}.
-type doc() :: datastore_doc:doc(record()).
-type state_info() :: maps:map().
-export_type([doc/0, record/0, id/0, state_info/0]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new state token with current timestamp and saves state info
%% connected with it.
%% @end
%%--------------------------------------------------------------------
-spec create(state_info()) -> {ok, id()} | datastore:create_error().
create(StateInfo) ->
    {ok, #document{key = Token}} = datastore_model:create(?CTX, #document{
        value = #state_token{
            timestamp = time_utils:cluster_time_seconds(),
            state_info = StateInfo
        }
    }),
    {ok, Token}.

%%--------------------------------------------------------------------
%% @doc
%% Lookups state token in database. If existent and still valid, state info
%% is returned. error atom indicates that presented token is not valid (either
%% expired, already used or never issued by this zone).
%% The token (if present) is immediately consumed as it is a one-shot token.
%% @end
%%--------------------------------------------------------------------
-spec lookup(id()) -> {ok, state_info()} | error.
lookup(Token) ->
    case datastore_model:get(?CTX, Token) of
        {ok, #document{value = #state_token{timestamp = T, state_info = Info}}} ->
            % The token is consumed immediately
            datastore_model:delete(?CTX, Token),
            % Check if the token is still valid
            case time_utils:cluster_time_seconds() - T =< ?STATE_TOKEN_TTL of
                true -> {ok, Info};
                false -> error
            end;
        _ ->
            error
    end.

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes model.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | {error, term()}.
init() ->
    datastore_model:init(?CTX).