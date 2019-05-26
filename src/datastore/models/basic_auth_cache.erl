%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Cache user basic auth credentials to user basic properties
%%% @end
%%%-------------------------------------------------------------------
-module(basic_auth_cache).
-author("Bartosz Walkowicz").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get/2, save/3, delete/1]).

%% datastore_model callbacks
-export([get_ctx/0]).

-define(CTX, #{
    model => ?MODULE,
    routing => local,
    disc_driver => undefined
}).

-define(EXPIRATION_TIMEOUT, application:get_env(
    ?APP_NAME, basic_auth_cache_expiration_timeout, timer:seconds(5))
).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Returns user info from cache.
%% @end
%%--------------------------------------------------------------------
-spec get(Login :: binary(), Password :: binary()) ->
    {ok, maps:map()} | {error, term()}.
get(Login, Password) ->
    Now = time_utils:system_time_millis(),
    case datastore_model:get(?CTX, Login) of
        {ok, #document{value = #basic_auth_cache{
            password_hash = PasswordHash, expires = Expires, props = Props
        }}} when Now < Expires ->
            case onedata_passwords:verify(Password, PasswordHash) of
                true -> {ok, Props};
                false -> {error, not_found}
            end;
        {ok, #document{}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Saves user info.
%% @end
%%--------------------------------------------------------------------
-spec save(Login :: binary(), Password :: binary(), UserInfo :: maps:map()) ->
    ok | {error, term()}.
save(Login, Password, UserInfo) ->
    Doc = #document{key = Login, value = #basic_auth_cache{
        password_hash = onedata_passwords:create_hash(Password),
        expires = time_utils:system_time_millis() + ?EXPIRATION_TIMEOUT,
        props = UserInfo
    }},
    case datastore_model:save(?CTX, Doc) of
        {ok, _} -> ok;
        Value -> Value
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes user info from cache.
%% @end
%%--------------------------------------------------------------------
-spec delete(Login :: undefined | binary()) -> ok | {error, term()}.
delete(undefined) ->
    ok;
delete(Login) ->
    datastore_model:delete(?CTX, Login).


%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Returns model's context.
%% @end
%%--------------------------------------------------------------------
-spec get_ctx() -> datastore:ctx().
get_ctx() ->
    ?CTX.
