%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles entity
%%% logic operations corresponding to named (od_token) and temporary tokens.
%%% @end
%%%-------------------------------------------------------------------
-module(token_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-type is_authorized_to_invite_fun() :: fun((aai:auth(), tokens:invite_token_type(), gri:entity_id()) -> boolean()).
-type consume_fun() :: fun((gri:entity_id()) -> Result :: term() | errors:error()).

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).
-export([consume_invite_token/5]).

-define(MAX_TEMPORARY_TOKEN_TTL, oz_worker:get_env(max_temporary_token_ttl, 604800)). % 1 week

% Internal token metadata is used to keep track of multi-use invite tokens and
% is not available to update by the token owner (but can be read).
-define(INTERNAL_METADATA_KEY, <<"$$$_internal_$$$">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
%%  * {true, gri:gri(), entity_logic:versioned_entity()}
%%      if the fetch was successful and new GRI was resolved
%%  * false
%%      if fetch is not applicable for this operation
%%  * {error, _}
%%      if there was an error, such as ?ERROR_NOT_FOUND
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{aspect = {user_named_token, _}}) ->
    false;
fetch_entity(#gri{aspect = {provider_named_token, _}}) ->
    false;
fetch_entity(#gri{id = TokenNonce}) ->
    case od_token:get(TokenNonce) of
        {ok, #document{value = Token, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_utils:parse_rev(DbRev),
            {true, {Token, Revision}};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(create, {user_named_token, _}, private) -> true;
operation_supported(create, {provider_named_token, _}, private) -> true;
operation_supported(create, {user_temporary_token, _}, private) -> true;
operation_supported(create, {provider_temporary_token, _}, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, {user_named_tokens, _}, private) -> true;
operation_supported(get, {provider_named_tokens, _}, private) -> true;
operation_supported(get, instance, private) -> true;
operation_supported(get, {user_named_token, _}, private) -> true;
operation_supported(get, {provider_named_token, _}, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {user_named_token, _}, private) -> true;
operation_supported(update, {provider_named_token, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {user_named_token, _}, private) -> true;
operation_supported(delete, {provider_named_token, _}, private) -> true;
operation_supported(delete, {user_named_tokens, _}, private) -> true;
operation_supported(delete, {provider_named_tokens, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
create(#el_req{gri = #gri{id = TokenName, aspect = {user_named_token, UserId}}, data = Data}) ->
    create_named_token(?SUB(user, UserId), TokenName, Data);

create(#el_req{gri = #gri{id = TokenName, aspect = {provider_named_token, ProviderId}}, data = Data}) ->
    create_named_token(?SUB(?ONEPROVIDER, ProviderId), TokenName, Data);

create(#el_req{gri = #gri{id = undefined, aspect = {user_temporary_token, UserId}}, data = Data}) ->
    create_temporary_token(?SUB(user, UserId), Data);

create(#el_req{gri = #gri{id = undefined, aspect = {provider_temporary_token, ProviderId}}, data = Data}) ->
    create_temporary_token(?SUB(?ONEPROVIDER, ProviderId), Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    named_tokens:list_all();

get(#el_req{gri = #gri{aspect = {user_named_tokens, UserId}}}, _) ->
    named_tokens:list_by_subject(?SUB(user, UserId));

get(#el_req{gri = #gri{aspect = {provider_named_tokens, ProviderId}}}, _) ->
    named_tokens:list_by_subject(?SUB(?ONEPROVIDER, ProviderId));

get(#el_req{gri = #gri{aspect = instance}}, Token) ->
    {ok, Token};

get(#el_req{gri = #gri{id = TokenName, aspect = {user_named_token, UserId}}}, _) ->
    {ok, lookup_token(?SUB(user, UserId), TokenName)};

get(#el_req{gri = #gri{id = TokenName, aspect = {provider_named_token, PrId}}}, _) ->
    {ok, lookup_token(?SUB(?ONEPROVIDER, PrId), TokenName)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = TokenNonce, aspect = instance}, data = Data}) ->
    {ok, _} = od_token:update(TokenNonce, fun(Token = #od_token{metadata = OldMetadata, revoked = OldRevoked}) ->
        NewMetadata = maps:get(<<"metadata">>, Data, OldMetadata),
        {ok, Token#od_token{
            revoked = maps:get(<<"revoked">>, Data, OldRevoked),
            % Internal metadata cannot be overwritten
            metadata = case maps:find(?INTERNAL_METADATA_KEY, OldMetadata) of
                error -> maps:remove(?INTERNAL_METADATA_KEY, NewMetadata);
                {ok, Internal} -> NewMetadata#{?INTERNAL_METADATA_KEY => Internal}
            end
        }}
    end),
    ok;

update(Req = #el_req{gri = GRI = #gri{id = TokenName, aspect = {user_named_token, UserId}}}) ->
    update(Req#el_req{gri = GRI#gri{id = lookup_nonce(?SUB(user, UserId), TokenName), aspect = instance}});

update(Req = #el_req{gri = GRI = #gri{id = TokenName, aspect = {provider_named_token, PrId}}}) ->
    update(Req#el_req{gri = GRI#gri{id = lookup_nonce(?SUB(?ONEPROVIDER, PrId), TokenName), aspect = instance}}).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = TokenNonce, aspect = instance}}) ->
    fun(NamedToken) ->
        delete_named(TokenNonce, NamedToken)
    end;

delete(#el_req{gri = #gri{id = TokenName, aspect = {user_named_token, UserId}}}) ->
    delete_named(lookup_nonce(?SUB(user, UserId), TokenName));

delete(#el_req{gri = #gri{id = TokenName, aspect = {provider_named_token, PrId}}}) ->
    delete_named(lookup_nonce(?SUB(?ONEPROVIDER, PrId), TokenName));

delete(#el_req{gri = #gri{id = undefined, aspect = {user_named_tokens, UserId}}}) ->
    {ok, UserTokens} = named_tokens:list_by_subject(?SUB(user, UserId)),
    lists:foreach(fun({TokenName, TokenNonce}) ->
        delete_named(?SUB(user, UserId), TokenNonce, TokenName)
    end, UserTokens);

delete(#el_req{gri = #gri{id = undefined, aspect = {provider_named_tokens, ProviderId}}}) ->
    {ok, ProviderTokens} = named_tokens:list_by_subject(?SUB(?ONEPROVIDER, ProviderId)),
    lists:foreach(fun({TokenName, TokenNonce}) ->
        delete_named(?SUB(?ONEPROVIDER, ProviderId), TokenNonce, TokenName)
    end, ProviderTokens).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(_, _) ->
    % Instance's existence depends on fetch_entity, and for requests by name,
    % it is checked later in get / delete.
    true.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
% Users and providers are authorized to perform all operations on their tokens
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {user_named_tokens, UserId}}}, _) ->
    true;
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {user_named_token, UserId}}}, _) ->
    true;
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {user_temporary_token, UserId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {provider_named_tokens, PrId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {provider_named_token, PrId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {provider_temporary_token, PrId}}}, _) ->
    true;

% Provider's admin with CLUSTER_UPDATE is allowed to manage its tokens
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {provider_named_tokens, PrId}}}, _) ->
    cluster_logic:has_eff_privilege(PrId, UserId, ?CLUSTER_UPDATE);

authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {provider_named_token, PrId}}}, _) ->
    cluster_logic:has_eff_privilege(PrId, UserId, ?CLUSTER_UPDATE);

authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {provider_temporary_token, PrId}}}, _) ->
    cluster_logic:has_eff_privilege(PrId, UserId, ?CLUSTER_UPDATE);

% When a token is referenced by its id, check if token's subject matches the auth
authorize(#el_req{auth = Auth, gri = #gri{aspect = instance}}, #od_token{subject = TokenSubject}) ->
    case {Auth, TokenSubject} of
        {#auth{subject = ?SUB(SubType, SubId)}, ?SUB(SubType, SubId)} ->
            true;
        {?USER(UserId), ?SUB(?ONEPROVIDER, ProviderId)} ->
            cluster_logic:has_eff_privilege(ProviderId, UserId, ?CLUSTER_UPDATE);
        _ ->
            false
    end;

authorize(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = create, data = #{<<"type">> := ?INVITE_TOKEN(?PROVIDER_REGISTRATION_TOKEN, _)}}) ->
    [?OZ_PROVIDERS_INVITE];
required_admin_privileges(_) ->
    % This privilege allows to perform all operations related to tokens
    [?OZ_MANAGE_TOKENS].


-define(NAMED_TOKEN_VALIDATOR(Subject), #{
    required => #{
        {id, <<"name">>} => {binary, name}
    },
    optional => #{
        <<"type">> => {token_type, fun(Type) -> validate_type(Subject, named, Type) end},
        <<"caveats">> => {caveats, fun(Caveat) -> validate_caveat(Subject, Caveat) end},
        <<"metadata">> => {json, fun validate_metadata/1}
    }
}).
-define(TEMPORARY_TOKEN_VALIDATOR(Subject), #{
    optional => #{
        <<"type">> => {token_type, fun(Type) -> validate_type(Subject, temporary, Type) end},
        <<"caveats">> => {caveats, fun(Caveat) -> validate_caveat(Subject, Caveat) end}
    }
}).

%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = create, gri = #gri{aspect = {user_named_token, UserId}}}) ->
    ?NAMED_TOKEN_VALIDATOR(?SUB(user, UserId));
validate(#el_req{operation = create, gri = #gri{aspect = {provider_named_token, ProviderId}}}) ->
    ?NAMED_TOKEN_VALIDATOR(?SUB(?ONEPROVIDER, ProviderId));

validate(#el_req{operation = create, gri = #gri{aspect = {user_temporary_token, UserId}}}) ->
    ?TEMPORARY_TOKEN_VALIDATOR(?SUB(user, UserId));
validate(#el_req{operation = create, gri = #gri{aspect = {provider_temporary_token, ProviderId}}}) ->
    ?TEMPORARY_TOKEN_VALIDATOR(?SUB(?ONEPROVIDER, ProviderId));

validate(#el_req{operation = update, gri = #gri{aspect = {user_named_token, _}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = instance}});
validate(#el_req{operation = update, gri = #gri{aspect = {provider_named_token, _}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = instance}});
validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"metadata">> => {json, fun validate_metadata/1},
        <<"revoked">> => {boolean, any}
    }
}.


%%--------------------------------------------------------------------
%% @doc
%% Unified procedure to be used from other logic plugin modules when consuming
%% an invite token.
%% @end
%%--------------------------------------------------------------------
-spec consume_invite_token(aai:auth(), tokens:token(), tokens:invite_token_type(),
    is_authorized_to_invite_fun(), consume_fun()) -> term() | no_return().
consume_invite_token(Auth, Token, ExpectedType, IsAuthorizedToInviteFun, ConsumeFun) ->
    case token_auth:verify_invite_token(Token, ExpectedType, Auth#auth.peer_ip, aai:auth_to_audience(Auth)) of
        {error, _} = Error ->
            Error;
        {ok, IssuerAuth} ->
            #token{type = ?INVITE_TOKEN(_, EntityId)} = Token,
            case IsAuthorizedToInviteFun(IssuerAuth, ExpectedType, EntityId) of
                false ->
                    ?ERROR_INVITE_TOKEN_ISSUER_NOT_AUTHORIZED;
                true ->
                    critical_section:run({invite_token, Token#token.nonce}, fun() ->
                        Result = ConsumeFun(EntityId),
                        ok = delete_named(Token#token.nonce),
                        Result
                    end)
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec lookup_nonce(aai:subject(), od_token:name()) -> tokens:nonce() | no_return().
lookup_nonce(Subject, TokenName) ->
    case named_tokens:get(Subject, TokenName) of
        {ok, TokenNonce} -> TokenNonce;
        _ -> throw(?ERROR_NOT_FOUND)
    end.


%% @private
-spec lookup_token(tokens:nonce()) -> od_token:record() | no_return().
lookup_token(TokenNonce) ->
    case od_token:get(TokenNonce) of
        {ok, #document{value = Token}} -> Token;
        _ -> throw(?ERROR_NOT_FOUND)
    end.


%% @private
-spec lookup_token(aai:subject(), od_token:name()) -> od_token:record() | no_return().
lookup_token(Subject, TokenName) ->
    lookup_token(lookup_nonce(Subject, TokenName)).


%% @private
-spec delete_named(tokens:nonce()) -> ok | no_return().
delete_named(TokenNonce) ->
    delete_named(TokenNonce, lookup_token(TokenNonce)).


%% @private
-spec delete_named(tokens:nonce(), od_token:record()) -> ok | no_return().
delete_named(TokenNonce, #od_token{name = TokenName, subject = Subject}) ->
    delete_named(Subject, TokenNonce, TokenName).


%% @private
-spec delete_named(aai:subject(), tokens:nonce(), od_token:name()) -> ok | no_return().
delete_named(Subject, TokenNonce, TokenName) ->
    ok = named_tokens:remove(Subject, TokenName),
    ok = od_token:delete(TokenNonce).


%% @private
-spec validate_caveat(aai:subject(), caveats:caveat()) -> boolean().
validate_caveat(Subject, #cv_audience{whitelist = AllowedAudiences}) ->
    lists:foreach(fun(Audience) ->
        token_auth:is_audience_allowed(Subject, Audience) orelse
            throw(?ERROR_TOKEN_AUDIENCE_FORBIDDEN(Audience))
    end, AllowedAudiences),
    true;
validate_caveat(_, _) ->
    true.


%% @private
-spec validate_type(aai:subject(), named | temporary, tokens:type()) -> boolean().
validate_type(?SUB(user), _, ?ACCESS_TOKEN) ->
    true;
validate_type(?SUB(?ONEPROVIDER), _, ?ACCESS_TOKEN) ->
    true;

validate_type(?SUB(user), temporary, ?GUI_ACCESS_TOKEN(_)) ->
    true;

% Providers are allowed to create invite tokens for clusters
validate_type(?SUB(?ONEPROVIDER), _, ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, _)) ->
    true;
validate_type(?SUB(?ONEPROVIDER), _, ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, _)) ->
    true;
validate_type(?SUB(user), _, ?INVITE_TOKEN(_, _)) ->
    % Users are allowed to create all invite tokens
    %% @TODO VFS-5727 checks will be implemented when invite token creation is migrated here
    true;
validate_type(_, _, _) ->
    false.


%% @private
-spec validate_metadata(json_utils:json_term()) -> boolean().
validate_metadata(Metadata) ->
    % Modifying the internal metadata is not allowed
    not maps:is_key(?INTERNAL_METADATA_KEY, Metadata).


%% @private
-spec create_named_token(aai:subject(), od_token:name(), entity_logic:data()) ->
    entity_logic:create_result().
create_named_token(Subject, TokenName, Data) ->
    TokenNonce = datastore_utils:gen_key(),
    case named_tokens:add(Subject, TokenName, TokenNonce) of
        ok -> ok;
        ?ERROR_ALREADY_EXISTS -> throw(?ERROR_ALREADY_EXISTS)
    end,
    Type = maps:get(<<"type">>, Data, ?ACCESS_TOKEN),
    Caveats = maps:get(<<"caveats">>, Data, []),
    Metadata = maps:get(<<"metadata">>, Data, #{}),
    Secret = tokens:generate_secret(),
    Token = construct(Subject, TokenNonce, Type, true, Secret, Caveats),
    TokenRecord = #od_token{
        name = TokenName,
        subject = Subject,
        type = Type,
        secret = Secret,
        caveats = [caveats:serialize(C) || C <- Caveats],
        metadata = Metadata
    },
    {ok, _} = od_token:create(#document{key = TokenNonce, value = TokenRecord}),
    {ok, value, Token}.


%% @private
-spec create_temporary_token(aai:subject(), entity_logic:data()) ->
    entity_logic:create_result().
create_temporary_token(Subject, Data) ->
    Type = maps:get(<<"type">>, Data, ?ACCESS_TOKEN),
    Caveats = maps:get(<<"caveats">>, Data, []),

    Now = time_utils:cluster_time_seconds(),
    MaxTtl = ?MAX_TEMPORARY_TOKEN_TTL,
    IsTtlAllowed = lists:any(fun
        (#cv_time{valid_until = ?INFINITY}) -> false;
        (#cv_time{valid_until = ValidUntil}) -> ValidUntil < Now + MaxTtl
    end, caveats:filter([cv_time], Caveats)),
    IsTtlAllowed orelse throw(?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)),

    TokenNonce = datastore_utils:gen_key(),
    Secret = shared_token_secret:get(),
    Token = construct(Subject, TokenNonce, Type, false, Secret, Caveats),
    {ok, value, Token}.


%% @private
-spec construct(aai:subject(), tokens:nonce(), tokens:type(), tokens:persistent(),
    tokens:secret(), [caveats:caveat()]) -> tokens:token().
construct(Subject, Nonce, Type, Persistent, Secret, Caveats) ->
    Prototype = #token{
        onezone_domain = oz_worker:get_domain(),
        subject = Subject,
        nonce = Nonce,
        type = Type,
        persistent = Persistent
    },
    tokens:construct(Prototype, Secret, Caveats).