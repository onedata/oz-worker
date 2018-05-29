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


-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([init/0, terminate/0]).
-export([find_record/2, find_all/1, query/2, query_record/2]).
-export([create_record/2, update_record/3, delete_record/2]).
-export([user_record/2]).
-export([push_user_record/1, push_user_record_when_synchronized/1]).

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
    {ok, user_record(?USER(gui_session:get_user_id()), UserId)}.


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
update_record(<<"user">>, UserId, [{<<"alias">>, NewLogin}]) ->
    Client = ?USER(gui_session:get_user_id()),
    case user_logic:update_alias(Client, UserId, NewLogin) of
        ok ->
            ok;
        ?ERROR_BAD_VALUE_ALIAS ->
            gui_error:report_warning(<<
                "Alias must be 3-15 characters long and composed of letters and digits, "
                "dashes and underscores are allowed (but not at the beginning or the end). "
                "Use null value to unset the login. "
            >>);
        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(_) ->
            gui_error:report_warning(
                <<"This alias is occupied by someone else. "
                "Please choose other alias.">>)
    end;
update_record(<<"user">>, UserId, Data) ->
    {ok, _} = od_user:update(UserId, fun(User = #od_user{}) ->
        case Data of
            [{<<"defaultSpaceId">>, null}] ->
                {ok, User#od_user{default_space = undefined}};
            [{<<"defaultSpaceId">>, DefaultSpace}] ->
                {ok, User#od_user{default_space = DefaultSpace}};
            [{<<"defaultProviderId">>, null}] ->
                {ok, User#od_user{default_provider = undefined}};
            [{<<"defaultProviderId">>, DefaultProvider}] ->
                {ok, User#od_user{default_provider = DefaultProvider}}
        end
    end),
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


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant user record based on user id.
%% @end
%%--------------------------------------------------------------------
-spec user_record(Client :: entity_logic:client(), UserId :: od_user:id()) ->
    proplists:proplist().
user_record(Client, UserId) ->
    {ok, #od_user{
        name = Name,
        alias = UserLogin,
        basic_auth_enabled = BasicAuthEnabled,
        linked_accounts = LinkedAccounts,
        client_tokens = ClientTokenIds,
        default_space = DefaultSpaceValue,
        default_provider = DefaultProviderValue,
        eff_groups = EffGroups,
        eff_spaces = EffSpaces,
        eff_providers = EffProviders
    }} = user_logic:get(Client, UserId),
    Login = login_db_to_client(UserLogin),
    Authorizers = authorizers_db_to_client(LinkedAccounts),
    ClientTokens = client_tokens_db_to_client(ClientTokenIds),
    DefaultSpace = undefined_to_null(DefaultSpaceValue),
    DefaultProvider = undefined_to_null(DefaultProviderValue),
    [
        {<<"id">>, UserId},
        {<<"name">>, Name},
        {<<"alias">>, Login},
        {<<"basicAuthEnabled">>, BasicAuthEnabled},
        {<<"authorizers">>, Authorizers},
        {<<"clienttokens">>, ClientTokens},
        {<<"defaultSpaceId">>, DefaultSpace},
        {<<"defaultProviderId">>, DefaultProvider},
        {<<"groups">>, maps:keys(EffGroups)},
        {<<"spaces">>, maps:keys(EffSpaces)},
        {<<"providers">>, maps:keys(EffProviders)}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Pushes newest user record to websocket client.
%% @end
%%--------------------------------------------------------------------
-spec push_user_record(UserId :: od_user:id()) -> ok.
push_user_record(UserId) ->
    gui_async:push_updated(<<"user">>, user_record(?USER(UserId), UserId)).


%%--------------------------------------------------------------------
%% @doc
%% Schedules an asynchronous push when user record has been synchronized.
%% @end
%%--------------------------------------------------------------------
-spec push_user_record_when_synchronized(UserId :: od_user:id()) -> ok.
push_user_record_when_synchronized(UserId) ->
    {ok, _} = gui_async:spawn(true, fun() ->
        entity_graph:ensure_up_to_date(),
        gui_async:push_updated(<<"user">>, user_record(?USER(UserId), UserId))
    end),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts user alias in db format to client format.
%% @end
%%--------------------------------------------------------------------
-spec login_db_to_client(UserLogin :: binary() | undefined) -> binary() | null.
login_db_to_client(undefined) -> null;
login_db_to_client(Bin) when is_binary(Bin) -> Bin.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts linked accounts in db format to client format.
%% @end
%%--------------------------------------------------------------------
-spec authorizers_db_to_client(LinkedAccounts :: [#linked_account{}]) ->
    proplists:proplist().
authorizers_db_to_client(LinkedAccounts) ->
    lists:foldl(
        fun(LinkedAccount, Acc) ->
            #linked_account{
                idp = Provider,
                email_list = Emails,
                subject_id = SubId} = LinkedAccount,
            ProviderBin = str_utils:to_binary(Provider),
            SubIdBin = str_utils:to_binary(SubId),
            AccId = <<ProviderBin/binary, "#", SubIdBin/binary>>,
            EmailsNotEmpty = case Emails of
                [] -> [<<"<no email address>">>];
                _ -> Emails
            end,
            Accounts = lists:map(
                fun(Email) ->
                    [
                        {<<"id">>, AccId},
                        {<<"type">>, ProviderBin},
                        {<<"email">>, Email}
                    ]
                end, EmailsNotEmpty),
            Accounts ++ Acc
        end, [], LinkedAccounts).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts client tokens in db format to client format.
%% @end
%%--------------------------------------------------------------------
-spec client_tokens_db_to_client(ClientTokenIds :: [token:id()]) ->
    [proplists:proplist()].
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
