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


-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/0, terminate/0]).
-export([find_record/2, find_all/1, query/2, query_record/2]).
-export([create_record/2, update_record/3, delete_record/2]).
-export([user_record/2, push_user_record_when_synchronized/1]).

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
update_record(<<"user">>, UserId, [{<<"alias">>, NewAlias}]) ->
    Client = ?USER(gui_session:get_user_id()),
    case n_user_logic:update_alias(Client, UserId, NewAlias) of
        ok ->
            ok;
        ?ERROR_BAD_VALUE_ALIAS_WRONG_PREFIX(_) ->
            gui_error:report_warning(
                <<"Alias cannot start with \"", ?NO_ALIAS_UUID_PREFIX, "\".">>);
        ?ERROR_BAD_VALUE_ALIAS(_) ->
            gui_error:report_warning(
                <<"Alias can contain only lowercase letters and digits, and "
                "must be at least 5 characters long.">>);
        ?ERROR_ALIAS_OCCUPIED ->
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


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant user record based on user id.
%% @end
%%--------------------------------------------------------------------
-spec user_record(Client :: n_entity_logic:client(), UserId :: od_user:id()) ->
    proplists:proplist().
user_record(Client, UserId) ->
    {ok, #od_user{
        name = Name,
        alias = UserAlias,
        basic_auth_enabled = BasicAuthEnabled,
        connected_accounts = OAuthAccounts,
        client_tokens = ClientTokenIds,
        default_space = DefaultSpaceValue,
        default_provider = DefaultProviderValue,
        eff_groups = EffGroups,
        eff_spaces = EffSpaces,
        eff_providers = EffProviders
    }} = n_user_logic:get(Client, UserId),
    Alias = case str_utils:to_binary(UserAlias) of
        <<"">> -> null;
        Bin -> Bin
    end,
    Authorizers = lists:foldl(
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
        end, [], OAuthAccounts),
    ClientTokens = lists:map(
        fun(Id) ->
            [{<<"id">>, Id}]
        end, ClientTokenIds),
    DefaultSpace = case DefaultSpaceValue of
        undefined -> null;
        _ -> DefaultSpaceValue
    end,
    DefaultProvider = case DefaultProviderValue of
        undefined -> null;
        _ -> DefaultProviderValue
    end,
    [
        {<<"id">>, UserId},
        {<<"name">>, Name},
        {<<"alias">>, Alias},
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