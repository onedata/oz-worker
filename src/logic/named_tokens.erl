%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles storing named tokens in a links tree attached to
%%% od_user / od_provider documents.
%%% @end
%%%-------------------------------------------------------------------
-module(named_tokens).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").

-export([add/3, get/2, list_all/0, list_by_subject/1, remove/2]).

-define(NAMED_TOKENS_TREE, <<"named-tokens-tree">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds a new named token for given subject (user / provider).
%% @end
%%--------------------------------------------------------------------
-spec add(aai:subject(), od_token:name(), tokens:nonce()) ->
    ok | {error, term()}.
add(?SUB(SubjectType, SubjectId), TokenName, TokenNonce) ->
    Ctx = datastore_ctx(SubjectType),
    case datastore_model:add_links(Ctx, SubjectId, ?NAMED_TOKENS_TREE, {TokenName, TokenNonce}) of
        {ok, _} -> ok;
        {error, already_exists} -> ?ERROR_ALREADY_EXISTS
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a subject's named token by name.
%% @end
%%--------------------------------------------------------------------
-spec get(aai:subject(), od_token:name()) -> {ok, tokens:nonce()} | {error, term()}.
get(?SUB(SubjectType, SubjectId), TokenName) ->
    Ctx = datastore_ctx(SubjectType),
    case datastore_model:get_links(Ctx, SubjectId, ?NAMED_TOKENS_TREE, TokenName) of
        {ok, [#link{target = TokenNonce}]} -> {ok, TokenNonce};
        {error, _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns all named tokens in the system (of all users and providers).
%% @end
%%--------------------------------------------------------------------
-spec list_all() -> {ok, [tokens:nonce()]} | {error, term()}.
list_all() ->
    {ok, UserDocs} = od_user:list(),
    {ok, ProviderDocs} = od_provider:list(),
    AllSubjects = [?SUB(user, D#document.key) || D <- UserDocs] ++
        [?SUB(?ONEPROVIDER, D#document.key) || D <- ProviderDocs],

    {ok, lists:flatmap(fun(Subject) ->
        {ok, Tokens} = list_by_subject(Subject),
        {_Names, Ids} = lists:unzip(Tokens),
        Ids
    end, AllSubjects)}.

%%--------------------------------------------------------------------
%% @doc
%% Returns all named tokens of given subject (user / provider).
%% @end
%%--------------------------------------------------------------------
-spec list_by_subject(aai:subject()) -> {ok, [{od_token:name(), tokens:nonce()}]} | {error, term()}.
list_by_subject(?SUB(SubjectType, SubjectId)) ->
    Ctx = datastore_ctx(SubjectType),
    AccumulateTokens = fun(Link, Acc) -> {ok, [{Link#link.name, Link#link.target} | Acc]} end,
    case datastore_model:fold_links(Ctx, SubjectId, ?NAMED_TOKENS_TREE, AccumulateTokens, [], #{}) of
        {ok, Tokens} -> {ok, lists:reverse(Tokens)};
        {error, _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes a named token of given subject (user / provider).
%% @end
%%--------------------------------------------------------------------
-spec remove(aai:subject(), od_token:name()) -> ok | {error, term()}.
remove(?SUB(SubjectType, SubjectId), TokenName) ->
    Ctx = datastore_ctx(SubjectType),
    datastore_model:delete_links(Ctx, SubjectId, ?NAMED_TOKENS_TREE, TokenName).


%% @private
-spec datastore_ctx(aai:subject_type()) -> datastore:ctx().
datastore_ctx(user) -> od_user:get_ctx();
datastore_ctx(?ONEPROVIDER) -> od_provider:get_ctx().