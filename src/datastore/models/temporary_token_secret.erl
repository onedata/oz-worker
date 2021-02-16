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
%%%
%%% Apart from above, this module implements entity logic plugin behaviour and
%%% handles entity logic operations corresponding to temporary_token_secret
%%% model - fetching the temporary token generation and subscribing for changes.
%%% @end
%%%-------------------------------------------------------------------
-module(temporary_token_secret).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-define(CTX, #{
    model => ?MODULE,
    sync_enabled => true,
    memory_copies => all
}).

%% API
-export([get_for_subject/1, regenerate_for_subject/1, delete_for_subject/1, key_to_subject/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).

%% entity_logic_plugin_behaviour
-export([entity_logic_plugin/0]).

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the shared token secret for given subject.
%% @end
%%--------------------------------------------------------------------
-spec get_for_subject(aai:subject()) ->
    {ok, {tokens:secret(), tokens:temporary_token_generation()}} | ?ERROR_NOT_FOUND.
get_for_subject(Subject) ->
    case fetch_entity(Subject) of
        ?ERROR_NOT_FOUND ->
            ?ERROR_NOT_FOUND;
        {true, {#temporary_token_secret{secret = Secret, generation = Generation}, _}} ->
            {ok, {Secret, Generation}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Regenerates the shared temporary token secret for given subject.
%% Causes all existing temporary tokens of given subject to be invalidated.
%% @end
%%--------------------------------------------------------------------
-spec regenerate_for_subject(aai:subject()) -> ok.
regenerate_for_subject(Subject) ->
    Diff = fun(#temporary_token_secret{generation = Generation}) ->
        {ok, #temporary_token_secret{
            secret = tokens:generate_secret(),
            generation = Generation + 1
        }}
    end,
    Default = #temporary_token_secret{
        secret = tokens:generate_secret(),
        generation = 1
    },
    datastore_model:update(?CTX, subject_to_key(Subject), Diff, Default),
    ?debug(
        "Generated a new share temporary token secret for ~ts. "
        "All existing temporary tokens of the subject have been invalidated.",
        [aai:subject_to_printable(Subject)]
    ).


%%--------------------------------------------------------------------
%% @doc
%% Completely deletes the shared temporary token secret for given subject.
%% Intended for cleanup when the subject is deleted from the system.
%% @end
%%--------------------------------------------------------------------
-spec delete_for_subject(aai:subject()) -> ok.
delete_for_subject(Subject) ->
    datastore_model:delete(?CTX, subject_to_key(Subject)).


-spec key_to_subject(datastore_key:key()) -> aai:subject().
key_to_subject(<<"usr", UserId/binary>>) -> ?SUB(user, UserId);
key_to_subject(<<"prv", PrId/binary>>) -> ?SUB(?ONEPROVIDER, PrId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec subject_to_key(aai:subject()) -> datastore_key:key().
subject_to_key(?SUB(user, UserId)) -> <<"usr", UserId/binary>>;
subject_to_key(?SUB(?ONEPROVIDER, PrId)) -> <<"prv", PrId/binary>>.


%% @private
-spec init_secret_for_subject(aai:subject()) -> ok.
init_secret_for_subject(Subject) ->
    critical_section:run({create_secret, Subject}, fun() ->
        % Make sure the secret wasn't initialized by another process
        case datastore_model:get(?CTX, subject_to_key(Subject)) of
            {error, not_found} -> regenerate_for_subject(Subject);
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
        {secret, string},
        {generation, integer}
    ]}.

%%%===================================================================
%%% entity logic plugin behaviour
%%%===================================================================

-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    ?MODULE.


-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(get, user, shared) -> true;
operation_supported(get, provider, shared) -> true;
operation_supported(_, _, _) -> false.


-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(user, shared) -> true;
is_subscribable(provider, shared) -> true;
is_subscribable(_, _) -> false.


-spec fetch_entity(gri:gri() | aai:subject()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = SubjectId, aspect = user, scope = shared}) ->
    fetch_entity(?SUB(user, SubjectId));
fetch_entity(#gri{id = SubjectId, aspect = provider, scope = shared}) ->
    fetch_entity(?SUB(?ONEPROVIDER, SubjectId));
fetch_entity(Subject) ->
    Key = subject_to_key(Subject),
    FetchResult = case datastore_model:get(?CTX, Key) of
        {error, not_found} ->
            init_secret_for_subject(Subject),
            datastore_model:get(?CTX, Key);
        OtherResult ->
            OtherResult
    end,
    case FetchResult of
        {ok, #document{value = Record, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Record, Revision}};
        _ ->
            ?ERROR_NOT_FOUND
    end.


-spec create(entity_logic:req()) -> errors:error().
create(_) ->
    ?ERROR_NOT_SUPPORTED.


-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{scope = shared}}, Record) ->
    {ok, Record#temporary_token_secret.generation}.


-spec update(entity_logic:req()) -> errors:error().
update(_) ->
    ?ERROR_NOT_SUPPORTED.


-spec delete(entity_logic:req()) -> errors:error().
delete(_) ->
    ?ERROR_NOT_SUPPORTED.


-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(_, _) ->
    true. % Checked in fetch_entity


-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(#el_req{auth = Auth, operation = get, gri = #gri{id = UserId, aspect = user, scope = shared}}, _) ->
    case Auth of
        ?PROVIDER(ProviderId) -> user_logic:has_eff_provider(UserId, ProviderId);
        ?USER(UserId) -> true;
        _ -> false
    end;
authorize(#el_req{auth = Auth, operation = get, gri = #gri{id = ProviderId, aspect = provider, scope = shared}}, _) ->
    case Auth of
        ?PROVIDER(ProviderId) -> true;
        ?USER(UserId) -> cluster_logic:has_eff_user(ProviderId, UserId);
        _ -> false
    end.


-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(_) ->
    [?OZ_TOKENS_MANAGE].


-spec validate(entity_logic:req()) -> errors:error().
validate(_) ->
    ?ERROR_NOT_SUPPORTED.
