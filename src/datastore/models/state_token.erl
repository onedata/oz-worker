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
%%% attacks. Every state token has correlated info, for example to which IdP the
%%% client was redirected.
%%% @end
%%%-------------------------------------------------------------------
-module(state_token).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/4]).
-export([lookup/1]).
-export([ttl/0]).

%% model_behaviour callbacks
-export([init/0]).

-type state_token() :: binary(). % Used as id in the database
-type state_info() :: map().
-export_type([state_token/0, state_info/0]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new state token with current timestamp and associates some login
%% info, that can be later retrieved given the token.
%%  IdP:                id of the IdentityProvider to which the login process is related
%%  LinkAccount:        whether this is a linking account flow
%%                      (if so, includes UserId of the subject user)
%%  RedirectAfterLogin: the URL to which the user will be redirected after successful login
%%  TestMode:           specifies whether this is a test login flow
%% @end
%%--------------------------------------------------------------------
-spec create(auth_config:idp(), LinkAccount :: false | {true, od_user:id()},
    RedirectAfterLogin :: binary(), TestMode :: boolean()) ->
    {ok, state_token()}.
create(IdP, LinkAccount, RedirectAfterLogin, TestMode) ->
    StateInfo = #{
        idp => IdP,
        link_account => LinkAccount,
        redirect_after_login => RedirectAfterLogin,
        test_mode => TestMode
    },
    {ok, #document{key = StateToken}} = datastore_model:create(?CTX, #document{
        value = #state_token{
            timestamp = time_utils:cluster_time_seconds(),
            state_info = StateInfo
        }
    }),
    {ok, StateToken}.


%%--------------------------------------------------------------------
%% @doc
%% Lookups state token in database. If existent and still valid, state info
%% is returned. error atom indicates that presented token is not valid (either
%% expired, already used or never issued by this zone).
%% The token (if present) is immediately consumed as it is a one-shot token.
%% @end
%%--------------------------------------------------------------------
-spec lookup(state_token()) -> {ok, state_info()} | error.
lookup(StateToken) ->
    case datastore_model:get(?CTX, StateToken) of
        {ok, #document{value = #state_token{timestamp = T, state_info = Info}}} ->
            % The token is consumed immediately
            datastore_model:delete(?CTX, StateToken),
            % Check if the token is still valid
            case time_utils:cluster_time_seconds() - T =< ttl() of
                true -> {ok, Info};
                false -> error
            end;
        _ ->
            error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the state token Time To Live, as configured in app.config.
%% @end
%%--------------------------------------------------------------------
-spec ttl() -> integer().
ttl() ->
    oz_worker:get_env(state_token_ttl_secs, 300).


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