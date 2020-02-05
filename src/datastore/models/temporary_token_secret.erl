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
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-define(CTX, #{
    model => ?MODULE,
    sync_enabled => true,
    memory_copies => all
}).

%% API
-export([get/1, regenerate/1, clear/1, key_to_subject/1]).

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
-spec get(aai:subject()) -> {tokens:secret(), tokens:temporary_token_generation()}.
get(Subject) ->
    Key = subject_to_key(Subject),
    {ok, #document{value = Record}} = case datastore_model:get(?CTX, Key) of
        {error, not_found} ->
            init_secret_for_subject(Subject),
            datastore_model:get(?CTX, Key);
        OtherResult ->
            OtherResult
    end,
    {Record#temporary_token_secret.secret, Record#temporary_token_secret.generation}.


%%--------------------------------------------------------------------
%% @doc
%% Regenerates the shared temporary token secret for given subject.
%% Causes all existing temporary tokens of given subject to be invalidated.
%% @end
%%--------------------------------------------------------------------
-spec regenerate(aai:subject()) -> ok.
regenerate(Subject) ->
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

-spec clear(aai:subject()) -> ok.
clear(Subject) ->
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


-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = SubjectId, aspect = Aspect, scope = shared}) ->
    Subject = case Aspect of
        user -> ?SUB(user, SubjectId);
        provider -> ?SUB(?ONEPROVIDER, SubjectId)
    end,
    try
        {_, Generation} = ?MODULE:get(Subject),
        {true, {Generation, Generation}}
    catch _:_ ->
        ?ERROR_NOT_FOUND
    end.


-spec create(entity_logic:req()) -> errors:error().
create(_) ->
    ?ERROR_NOT_SUPPORTED.


-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{scope = shared}}, Generation) ->
    {ok, Generation}.


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
