%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for records that store a shared token secret for all temporary tokens
%%% belonging to given subject (user or provider).
%%% The secret can be regenerated, in such case all existing
%%% temporary tokens of the subject become invalid.
%%% @end
%%%-------------------------------------------------------------------
-module(temporary_token_secret).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get/1, regenerate/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-define(CTX, #{
    model => ?MODULE,
    memory_copies => all
}).

-define(KEY(Subject), case Subject of
    ?SUB(user, UserId) -> datastore_key:build_adjacent(<<"user">>, UserId);
    ?SUB(?ONEPROVIDER, PrId) -> datastore_key:build_adjacent(<<"provider">>, PrId)
end).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the shared token secret for given subject.
%% @end
%%--------------------------------------------------------------------
-spec get(aai:subject()) -> tokens:secret().
get(Subject) ->
    Key = ?KEY(Subject),
    {ok, Doc} = case datastore_model:get(?CTX, Key) of
        {error, not_found} ->
            init_secret_for_subject(Subject),
            datastore_model:get(?CTX, Key);
        OtherResult ->
            OtherResult
    end,
    Doc#document.value#temporary_token_secret.secret.


%%--------------------------------------------------------------------
%% @doc
%% Regenerates the shared temporary token secret for given subject.
%% Causes all existing temporary tokens of given subject to be invalidated.
%% @end
%%--------------------------------------------------------------------
-spec regenerate(aai:subject()) -> ok.
regenerate(Subject) ->
    datastore_model:save(?CTX, #document{
        key = ?KEY(Subject),
        value = #temporary_token_secret{secret = tokens:generate_secret()}
    }),
    ?debug(
        "Generated a new share temporary token secret for ~ts. "
        "All existing temporary tokens of the subject have been invalidated.",
        [aai:subject_to_printable(Subject)]
    ),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes a new secret for given subject. Uses a critical section to avoid
%% race conditions and generating more than one secret.
%% @end
%%--------------------------------------------------------------------
-spec init_secret_for_subject(aai:subject()) -> ok.
init_secret_for_subject(Subject) ->
    critical_section:run({create_secret, Subject}, fun() ->
        % Make sure the secret wasn't initialized by another process
        case datastore_model:get(?CTX, ?KEY(Subject)) of
            {error, not_found} -> regenerate(Subject);
            {ok, _} -> ok
        end
    end).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {secret, string}
    ]}.
