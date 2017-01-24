%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015-2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the data-space model used in Ember application.
%%% @end
%%%-------------------------------------------------------------------
-module(user_data_backend).
-behavior(data_backend_behaviour).
-author("Lukasz Opiola").


-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/0, terminate/0]).
-export([find_record/2, find_all/1, query/2, query_record/2]).
-export([create_record/2, update_record/3, delete_record/2]).
-export([user_record/1, push_user_record/1]).

%%%===================================================================
%%% data_backend_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback init/0.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback terminate/0.
%% @end
%%--------------------------------------------------------------------
-spec terminate() -> ok.
terminate() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_record/2.
%% @end
%%--------------------------------------------------------------------
-spec find_record(ResourceType :: binary(), Id :: binary()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find_record(<<"user">>, UserId) ->
    case gui_session:get_user_id() of
        UserId ->
            {ok, user_record(UserId)};
        _ ->
            gui_error:unauthorized()
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
-spec find_all(ResourceType :: binary()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
find_all(<<"user">>) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback query/2.
%% @end
%%--------------------------------------------------------------------
-spec query(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
query(<<"user">>, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback query_record/2.
%% @end
%%--------------------------------------------------------------------
-spec query_record(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
query_record(<<"user">>, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
-spec create_record(RsrcType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
create_record(<<"user">>, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
-spec update_record(RsrcType :: binary(), Id :: binary(),
    Data :: proplists:proplist()) -> ok | gui_error:error_result().
update_record(<<"user">>, UserId, [{<<"alias">>, NewAlias}]) ->
    case user_logic:set_alias(UserId, NewAlias) of
        ok ->
            ok;
        {error, disallowed_alias_prefix} ->
            gui_error:report_warning(
                <<"Alias cannot start with \"", ?NO_ALIAS_UUID_PREFIX, "\".">>);
        {error, invalid_alias} ->
            gui_error:report_warning(
                <<"Alias can contain only lowercase letters and digits, and "
                "must be at least 5 characters long.">>);
        {error, alias_occupied} ->
            gui_error:report_warning(
                <<"This alias is occupied by someone else. "
                "Please choose other alias.">>)
    end;
update_record(<<"user">>, UserId, Data) ->
    {DiffKey, DiffValue} = case Data of
        [{<<"defaultSpaceId">>, DefaultSpace}] ->
            {default_space, DefaultSpace};
        [{<<"defaultProviderId">>, DefaultProvider}] ->
            {default_provider, DefaultProvider}
    end,
    DiffValueOrUndefined = case DiffValue of
        null -> undefined;
        _ -> DiffValue
    end,
    {ok, _} = od_user:update(UserId, #{DiffKey => DiffValueOrUndefined}),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
-spec delete_record(RsrcType :: binary(), Id :: binary()) ->
    ok | gui_error:error_result().
delete_record(<<"user">>, _Id) ->
    gui_error:report_error(<<"Not implemented">>).

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant user record based on space id.
%% @end
%%--------------------------------------------------------------------
-spec user_record(UserId :: od_user:id()) -> proplists:proplist().
user_record(UserId) ->
    {ok, #document{value = #od_user{
        name = Name,
        alias = UserAlias,
        basic_auth_enabled = BasicAuthEnabled,
        connected_accounts = OAuthAccounts,
        client_tokens = ClientTokenIds,
        default_space = DefaultSpaceValue,
        default_provider = DefaultProviderValue
    }}} = od_user:get(UserId),
    Alias = alias_db_to_client(UserAlias),
    Authorizers = authorizers_db_to_client(OAuthAccounts),
    ClientTokens = client_tokens_db_to_client(ClientTokenIds),
    DefaultSpace = undefined_to_null(DefaultSpaceValue),
    DefaultProvider = undefined_to_null(DefaultProviderValue),
    {ok, [{effective_groups, Groups}]} = user_logic:get_effective_groups(UserId),
    {ok, UserSpaces} = user_logic:get_spaces(UserId),
    Spaces = proplists:get_value(spaces, UserSpaces),
    {ok, [{providers, Providers}]} = user_logic:get_providers(UserId),
    [
        {<<"id">>, UserId},
        {<<"name">>, Name},
        {<<"alias">>, Alias},
        {<<"basicAuthEnabled">>, BasicAuthEnabled},
        {<<"authorizers">>, Authorizers},
        {<<"clienttokens">>, ClientTokens},
        {<<"defaultSpaceId">>, DefaultSpace},
        {<<"defaultProviderId">>, DefaultProvider},
        {<<"groups">>, Groups},
        {<<"spaces">>, Spaces},
        {<<"providers">>, Providers}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Pushes newest user record to websocket client.
%% @end
%%--------------------------------------------------------------------
-spec push_user_record(UserId :: od_user:id()) -> ok.
push_user_record(UserId) ->
    gui_async:push_updated(<<"user">>, user_record(UserId)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts user alias in db format to client format.
%% @end
%%--------------------------------------------------------------------
-spec alias_db_to_client(UserAlias :: binary() | undefined) -> binary() | null.
alias_db_to_client(undefined) -> null;
alias_db_to_client(?EMPTY_ALIAS) -> null;
alias_db_to_client(Bin) when is_binary(Bin) -> Bin.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts oauth accounts in db format to client format.
%% @end
%%--------------------------------------------------------------------
-spec authorizers_db_to_client(OAuthAccounts :: [#oauth_account{}]) ->
    binary() | null.
authorizers_db_to_client(OAuthAccounts) ->
    lists:foldl(
        fun(OAuthAccount, Acc) ->
            #oauth_account{
                provider_id = Provider,
                email_list = Emails,
                user_id = SubId} = OAuthAccount,
            ProviderBin = str_utils:to_binary(Provider),
            SubIdBin = str_utils:to_binary(SubId),
            AccId = <<ProviderBin/binary, "#", SubIdBin/binary>>,
            Accounts = lists:map(
                fun(Email) ->
                    [
                        {<<"id">>, AccId},
                        {<<"type">>, ProviderBin},
                        {<<"email">>, Email}
                    ]
                end, Emails),
            Accounts ++ Acc
        end, [], OAuthAccounts).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts client tokens in db format to client format.
%% @end
%%--------------------------------------------------------------------
-spec client_tokens_db_to_client(ClientTokenIds :: [token:id()]) ->
    binary() | null.
client_tokens_db_to_client(ClientTokenIds) ->
    lists:map(
        fun(Id) ->
            [{<<"id">>, Id}]
        end, ClientTokenIds).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts 'undefined' atom to 'null' atom or does nothing otherwise.
%% @end
%%--------------------------------------------------------------------
-spec undefined_to_null(Value :: undefined | term()) -> null | term().
undefined_to_null(undefined) -> null;
undefined_to_null(Value) -> Value.


