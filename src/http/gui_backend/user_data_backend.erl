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
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).
-export([user_record/1]).

-define(CURRENT_USER_ID, <<"0">>).

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
%% {@link data_backend_behaviour} callback find/2.
%% @end
%%--------------------------------------------------------------------
-spec find(ResourceType :: binary(), Id :: binary()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find(<<"user">>, ?CURRENT_USER_ID) ->
    UserId = gui_session:get_user_id(),
    {ok, user_record(UserId)};
find(<<"user">>, _) ->
    gui_error:unauthorized().


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
%% {@link data_backend_behaviour} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
-spec find_query(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find_query(<<"user">>, _Data) ->
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
    Data :: proplists:proplist()) ->
    ok | gui_error:error_result().
update_record(<<"user">>, ?CURRENT_USER_ID, [{<<"alias">>, NewAlias}]) ->
    UserId = gui_session:get_user_id(),
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
update_record(<<"user">>, ?CURRENT_USER_ID, Data) ->
    UserUpdateDiff = case Data of
        [{<<"defaultSpace">>, DefaultSpace}] ->
            #{default_space => DefaultSpace};
        [{<<"defaultProvider">>, DefaultProvider}] ->
            #{default_provider => DefaultProvider}
    end,
    {ok, _} = od_user:update(gui_session:get_user_id(), UserUpdateDiff),
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
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
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
        {<<"defaultSpace">>, DefaultSpace},
        {<<"defaultProvider">>, DefaultProvider},
        {<<"groups">>, Groups},
        {<<"spaces">>, Spaces},
        {<<"providers">>, Providers}
    ].
