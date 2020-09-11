%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles storing token names mapping (token name -> token id)
%%% in a links tree attached to od_user / od_provider documents.
%%% @end
%%%-------------------------------------------------------------------
-module(token_names).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").

-export([register/3, lookup/2, update/4, unregister/2]).
-export([list_all_ids/0, list_by_subject/1]).

-define(TOKEN_NAMES_TREE, <<"token-names-tree">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Registers a new token name mapping for given subject (user / provider).
%% @end
%%--------------------------------------------------------------------
-spec register(aai:subject(), od_token:name(), tokens:id()) ->
    ok | {error, term()}.
register(?SUB(SubjectType, SubjectId), TokenName, TokenId) ->
    Ctx = datastore_ctx(SubjectType),
    case datastore_model:add_links(Ctx, SubjectId, ?TOKEN_NAMES_TREE, {TokenName, TokenId}) of
        {ok, _} -> ok;
        {error, already_exists} -> ?ERROR_ALREADY_EXISTS
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves token id by name registered for given subject.
%% @end
%%--------------------------------------------------------------------
-spec lookup(aai:subject(), od_token:name()) -> {ok, tokens:id()} | {error, term()}.
lookup(?SUB(SubjectType, SubjectId), TokenName) ->
    Ctx = datastore_ctx(SubjectType),
    case datastore_model:get_links(Ctx, SubjectId, ?TOKEN_NAMES_TREE, TokenName) of
        {ok, [#link{target = TokenId}]} -> {ok, TokenId};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates a token name mapping for given subject (user / provider) -
%% unregisters the old mapping and registers the new one.
%% @end
%%--------------------------------------------------------------------
-spec update(aai:subject(), OldName :: od_token:name(), tokens:id(), NewName :: od_token:name()) ->
    ok | {error, term()}.
update(Subject, OldName, TokenId, NewName) ->
    case register(Subject, NewName, TokenId) of
        ok ->
            unregister(Subject, OldName);
        ?ERROR_ALREADY_EXISTS ->
            % If the mapping exists, check if it already points to the desired token id.
            % This check covers:
            %   - updating to the same name as the old one
            %   - possible desynchronizations with the token name in od_token record
            case lookup(Subject, NewName) of
                {ok, TokenId} -> ok;
                _ -> ?ERROR_ALREADY_EXISTS
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Unregisters a token name mapping for given subject (user / provider).
%% @end
%%--------------------------------------------------------------------
-spec unregister(aai:subject(), od_token:name()) -> ok | {error, term()}.
unregister(?SUB(SubjectType, SubjectId), TokenName) ->
    Ctx = datastore_ctx(SubjectType),
    datastore_model:delete_links(Ctx, SubjectId, ?TOKEN_NAMES_TREE, TokenName).


%%--------------------------------------------------------------------
%% @doc
%% Returns all ids of tokens registered by name (of all users and providers).
%% @end
%%--------------------------------------------------------------------
-spec list_all_ids() -> {ok, [tokens:id()]} | {error, term()}.
list_all_ids() ->
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
%% Returns all registered token names of given subject (user / provider)
%% as pairs {TokenName, TokenId}.
%% @end
%%--------------------------------------------------------------------
-spec list_by_subject(aai:subject()) -> {ok, [{od_token:name(), tokens:id()}]} | {error, term()}.
list_by_subject(?SUB(SubjectType, SubjectId)) ->
    Ctx = datastore_ctx(SubjectType),
    AccumulateTokens = fun(Link, Acc) -> {ok, [{Link#link.name, Link#link.target} | Acc]} end,
    case datastore_model:fold_links(Ctx, SubjectId, ?TOKEN_NAMES_TREE, AccumulateTokens, [], #{}) of
        {ok, Tokens} -> {ok, lists:reverse(Tokens)};
        {error, _} = Error -> Error
    end.


%% @private
-spec datastore_ctx(aai:subject_type()) -> datastore:ctx().
datastore_ctx(user) -> od_user:get_ctx();
datastore_ctx(?ONEPROVIDER) -> od_provider:get_ctx().