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

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

-define(MAX_TEMPORARY_TOKEN_TTL, oz_worker:get_env(max_temporary_token_ttl, 604800)). % 1 week
-define(OFFLINE_ACCESS_TOKEN_TTL, min(
    ?MAX_TEMPORARY_TOKEN_TTL,
    oz_worker:get_env(offline_access_token_ttl, 604800) % 1 week
)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
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
fetch_entity(#gri{id = TokenId}) ->
    case od_token:get(TokenId) of
        {ok, #document{value = NamedToken, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {NamedToken, Revision}};
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
operation_supported(create, examine, public) -> true;
operation_supported(create, confine, public) -> true;
operation_supported(create, infer_access_token_scope, public) -> true;
operation_supported(create, verify_access_token, public) -> true;
operation_supported(create, verify_identity_token, public) -> true;
operation_supported(create, verify_invite_token, public) -> true;

operation_supported(create, {user_named_token, _}, private) -> true;
operation_supported(create, {provider_named_token, _}, private) -> true;
operation_supported(create, {user_temporary_token, _}, private) -> true;
operation_supported(create, {provider_temporary_token, _}, private) -> true;
operation_supported(create, {offline_user_access_token, _}, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, {user_named_tokens, _}, private) -> true;
operation_supported(get, {provider_named_tokens, _}, private) -> true;
operation_supported(get, instance, private) -> true;
operation_supported(get, instance, shared) -> true;
operation_supported(get, {user_named_token, _}, private) -> true;
operation_supported(get, {provider_named_token, _}, private) -> true;

operation_supported(update, instance, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {user_named_tokens, _}, private) -> true;
operation_supported(delete, {provider_named_tokens, _}, private) -> true;
operation_supported(delete, {user_temporary_tokens, _}, private) -> true;
operation_supported(delete, {provider_temporary_tokens, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, private) -> true;
is_subscribable(instance, shared) -> true;
is_subscribable({user_named_token, _}, private) -> true;
is_subscribable({user_named_tokens, _}, private) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
create(#el_req{gri = #gri{id = undefined, aspect = examine}, data = Data}) ->
    Token = maps:get(<<"token">>, Data),
    #token{
        onezone_domain = OnezoneDomain, id = Id, persistence = Persistence,
        subject = Subject, type = Type
    } = Token,
    {ok, value, #{
        <<"onezoneDomain">> => OnezoneDomain,
        <<"id">> => Id,
        <<"persistence">> => case Persistence of
            named -> <<"named">>;
            {temporary, _} -> <<"temporary">>
        end,
        <<"subject">> => Subject,
        <<"type">> => Type,
        <<"caveats">> => tokens:get_caveats(Token)
    }};

create(#el_req{gri = #gri{id = undefined, aspect = confine}, data = Data}) ->
    Token = #token{type = Type, subject = Subject} = maps:get(<<"token">>, Data),
    Caveats = maps:get(<<"caveats">>, Data),
    validate_subject_and_service(Type, Subject, Caveats),
    {ok, value, tokens:confine(Token, Caveats)};

create(ElReq = #el_req{gri = #gri{id = undefined, aspect = infer_access_token_scope}, data = Data}) ->
    Token = maps:get(<<"token">>, Data),
    AuthCtxWithoutConsumer = #auth_ctx{
        ip = ElReq#el_req.auth#auth.peer_ip,
        ignored_caveats = [cv_service, cv_interface],
        data_access_caveats_policy = allow_data_access_caveats,
        session_id = any
    },
    Consumer = case maps:find(<<"consumerToken">>, Data) of
        error -> Token#token.subject;
        {ok, null} -> undefined;
        {ok, ConsumerToken} -> ?check(token_auth:verify_consumer_token(ConsumerToken, AuthCtxWithoutConsumer))
    end,
    Auth = ?check(token_auth:verify_access_token(Token, AuthCtxWithoutConsumer#auth_ctx{consumer = Consumer})),
    {ok, value, infer_access_token_scope(Auth, Token)};

create(#el_req{auth = Auth, gri = #gri{id = undefined, aspect = verify_access_token}, data = Data}) ->
    Token = maps:get(<<"token">>, Data),
    AuthCtx = build_token_verification_auth_ctx(Auth, Data),
    case token_auth:verify_access_token(Token, AuthCtx) of
        {ok, #auth{subject = Subject, caveats = Caveats}} ->
            {ok, value, #{
                <<"subject">> => Subject,
                <<"ttl">> => caveats:infer_ttl(Caveats)
            }};
        {error, _} = Error ->
            Error
    end;

create(#el_req{auth = Auth, gri = #gri{id = undefined, aspect = verify_identity_token}, data = Data}) ->
    Token = maps:get(<<"token">>, Data),
    AuthCtx = build_token_verification_auth_ctx(Auth, Data),
    case token_auth:verify_identity_token(Token, AuthCtx) of
        {ok, {Subject, Caveats}} ->
            {ok, value, #{
                <<"subject">> => Subject,
                <<"ttl">> => caveats:infer_ttl(Caveats)
            }};
        {error, _} = Error ->
            Error
    end;

create(#el_req{auth = Auth, gri = #gri{id = undefined, aspect = verify_invite_token}, data = Data}) ->
    Token = maps:get(<<"token">>, Data),
    ExpType = maps:get(<<"expectedInviteType">>, Data, any),
    AuthCtx = build_token_verification_auth_ctx(Auth, Data),
    case token_auth:verify_invite_token(Token, ExpType, AuthCtx) of
        {ok, #auth{subject = Subject, caveats = Caveats}} ->
            {ok, value, #{
                <<"subject">> => Subject,
                <<"ttl">> => caveats:infer_ttl(Caveats)
            }};
        {error, _} = Error ->
            Error
    end;

create(#el_req{gri = #gri{id = undefined, aspect = {user_named_token, UserId}}, data = Data}) ->
    create_named_token(?SUB(user, UserId), Data);

create(#el_req{gri = #gri{id = undefined, aspect = {provider_named_token, ProviderId}}, data = Data}) ->
    create_named_token(?SUB(?ONEPROVIDER, ProviderId), Data);

create(#el_req{gri = #gri{id = undefined, aspect = {user_temporary_token, UserId}}, data = Data}) ->
    create_temporary_token(?SUB(user, UserId), Data);

create(#el_req{gri = #gri{id = undefined, aspect = {provider_temporary_token, ProviderId}}, data = Data}) ->
    create_temporary_token(?SUB(?ONEPROVIDER, ProviderId), Data);

create(#el_req{auth = ?PROVIDER(PrId) = Auth, gri = #gri{id = undefined, aspect = {offline_user_access_token, UserId}}, data = Data}) ->
    Token = maps:get(<<"token">>, Data),
    AuthCtx = build_token_verification_auth_ctx(Auth, Data),
    case token_auth:verify_access_token(Token, AuthCtx) of
        {ok, #auth{subject = ?SUB(user, UserId), caveats = OriginalCaveats}} ->
            create_temporary_token(?SUB(user, UserId), #{
                <<"type">> => ?ACCESS_TOKEN,
                <<"caveats">> => provider_offline_access:build_token_caveats(
                    OriginalCaveats, PrId, ?OFFLINE_ACCESS_TOKEN_TTL
                )
            });
        {ok, #auth{}} ->
            ?ERROR_TOKEN_SUBJECT_INVALID;
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    token_names:list_all_ids();

get(#el_req{gri = #gri{aspect = {user_named_tokens, UserId}}}, _) ->
    {ok, Tokens} = token_names:list_by_subject(?SUB(user, UserId)),
    {_Names, Ids} = lists:unzip(Tokens),
    {ok, Ids};

get(#el_req{gri = #gri{aspect = {provider_named_tokens, ProviderId}}}, _) ->
    {ok, Tokens} = token_names:list_by_subject(?SUB(?ONEPROVIDER, ProviderId)),
    {_Names, Ids} = lists:unzip(Tokens),
    {ok, Ids};

get(#el_req{gri = #gri{aspect = instance, id = TokenId, scope = private}}, NamedToken) ->
    {ok, to_token_data(TokenId, NamedToken)};

get(#el_req{gri = #gri{aspect = instance, scope = shared}}, NamedToken) ->
    {ok, #{<<"revoked">> => NamedToken#od_token.revoked}};

get(#el_req{gri = #gri{aspect = {user_named_token, UserId}, id = TokenName}}, _) ->
    case token_names:lookup(?SUB(user, UserId), TokenName) of
        {ok, TokenId} -> {ok, to_token_data(TokenId)};
        {error, _} -> ?ERROR_NOT_FOUND
    end;

get(#el_req{gri = #gri{aspect = {provider_named_token, ProviderId}, id = TokenName}}, _) ->
    case token_names:lookup(?SUB(?ONEPROVIDER, ProviderId), TokenName) of
        {ok, TokenId} -> {ok, to_token_data(TokenId)};
        {error, _} -> ?ERROR_NOT_FOUND
    end.

%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = TokenId, aspect = instance}, data = Data}) ->
    % If name change was requested, try to update the name mapping first and
    % (upon success) proceed to od_token record update.
    case maps:find(<<"name">>, Data) of
        {ok, NewName} ->
            {ok, #document{value = #od_token{name = OldName, subject = Subject}}} = od_token:get(TokenId),
            case token_names:update(Subject, OldName, TokenId, NewName) of
                ok -> ok;
                ?ERROR_ALREADY_EXISTS -> throw(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"name">>))
            end;
        error ->
            ok
    end,

    {ok, _} = od_token:update(TokenId, fun(Token = #od_token{metadata = OldMetadata}) ->
        {ok, Token#od_token{
            name = maps:get(<<"name">>, Data, Token#od_token.name),
            revoked = maps:get(<<"revoked">>, Data, Token#od_token.revoked),
            metadata = case maps:find(<<"customMetadata">>, Data) of
                {ok, CustomMetadata} -> token_metadata:update_custom_metadata(OldMetadata, CustomMetadata);
                error -> OldMetadata
            end
        }}
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = TokenId, aspect = instance}}) ->
    fun(#od_token{name = TokenName, subject = Subject}) ->
        delete_named_token(Subject, TokenName, TokenId)
    end;

delete(#el_req{gri = #gri{id = undefined, aspect = {user_named_tokens, UserId}}}) ->
    {ok, UserTokens} = token_names:list_by_subject(?SUB(user, UserId)),
    lists:foreach(fun({TokenName, TokenId}) ->
        ok = delete_named_token(?SUB(user, UserId), TokenName, TokenId)
    end, UserTokens);

delete(#el_req{gri = #gri{id = undefined, aspect = {provider_named_tokens, ProviderId}}}) ->
    {ok, ProviderTokens} = token_names:list_by_subject(?SUB(?ONEPROVIDER, ProviderId)),
    lists:foreach(fun({TokenName, TokenId}) ->
        ok = delete_named_token(?SUB(?ONEPROVIDER, ProviderId), TokenName, TokenId)
    end, ProviderTokens);

delete(#el_req{gri = #gri{aspect = {user_temporary_tokens, UserId}}}) ->
    temporary_token_secret:regenerate_for_subject(?SUB(user, UserId));

delete(#el_req{gri = #gri{aspect = {provider_temporary_tokens, PrId}}}) ->
    temporary_token_secret:regenerate_for_subject(?SUB(?ONEPROVIDER, PrId)).


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
% Publicly available operations
authorize(#el_req{operation = create, gri = #gri{aspect = examine}}, _) ->
    true;
authorize(#el_req{operation = create, gri = #gri{aspect = confine}}, _) ->
    true;
authorize(#el_req{operation = create, gri = #gri{aspect = infer_access_token_scope}}, _) ->
    true;
authorize(#el_req{operation = create, gri = #gri{aspect = verify_access_token}}, _) ->
    true;
authorize(#el_req{operation = create, gri = #gri{aspect = verify_identity_token}}, _) ->
    true;
authorize(#el_req{operation = create, gri = #gri{aspect = verify_invite_token}}, _) ->
    true;
% Users and providers are authorized to perform all operations on their tokens.
% NOTE: Additional authorization for creating different types of
% invite tokens is checked in validate_type/3.
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {user_named_tokens, UserId}}}, _) ->
    true;
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {user_named_token, UserId}}}, _) ->
    true;
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {user_temporary_token, UserId}}}, _) ->
    true;
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {user_temporary_tokens, UserId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {provider_named_tokens, PrId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {provider_named_token, PrId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {provider_temporary_token, PrId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {provider_temporary_tokens, PrId}}}, _) ->
    true;
authorize(#el_req{auth = ?PROVIDER(PrId), gri = #gri{aspect = {offline_user_access_token, UserId}}}, _) ->
    provider_logic:has_eff_user(PrId, UserId);

% Provider's admin with CLUSTER_UPDATE is allowed to manage its tokens
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {provider_named_tokens, PrId}}}, _) ->
    cluster_logic:has_eff_privilege(PrId, UserId, ?CLUSTER_UPDATE);
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {provider_named_token, PrId}}}, _) ->
    cluster_logic:has_eff_privilege(PrId, UserId, ?CLUSTER_UPDATE);
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {provider_temporary_token, PrId}}}, _) ->
    cluster_logic:has_eff_privilege(PrId, UserId, ?CLUSTER_UPDATE);
authorize(#el_req{auth = ?USER(UserId), gri = #gri{aspect = {provider_temporary_tokens, PrId}}}, _) ->
    cluster_logic:has_eff_privilege(PrId, UserId, ?CLUSTER_UPDATE);

% When a token is referenced by its id, check if token's subject matches the auth
authorize(#el_req{auth = Auth, gri = #gri{aspect = instance, scope = private}}, #od_token{subject = TokenSubject}) ->
    case {Auth, TokenSubject} of
        {#auth{subject = ?SUB(SubType, SubId)}, ?SUB(SubType, SubId)} ->
            true;
        {?USER(UserId), ?SUB(?ONEPROVIDER, ProviderId)} ->
            cluster_logic:has_eff_privilege(ProviderId, UserId, ?CLUSTER_UPDATE);
        _ ->
            false
    end;

authorize(#el_req{auth = Auth, gri = #gri{aspect = instance, scope = shared}}, #od_token{subject = TokenSubject}) ->
    case {Auth, TokenSubject} of
        {?PROVIDER(ProviderId), ?SUB(user, UserId)} ->
            user_logic:has_eff_provider(UserId, ProviderId);
        {?USER(UserId), ?SUB(user, UserId)} ->
            true;
        {?PROVIDER(ProviderId), ?SUB(?ONEPROVIDER, ProviderId)} ->
            true;
        {?USER(UserId), ?SUB(?ONEPROVIDER, ProviderId)} ->
            cluster_logic:has_eff_user(ProviderId, UserId);
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
required_admin_privileges(_) ->
    % NOTE: Additional admin privileges for creating different types of
    % invite tokens is checked in validate_type/3.
    [?OZ_TOKENS_MANAGE].


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(entity_logic:req()) -> entity_logic_sanitizer:sanitizer_spec().
validate(#el_req{operation = create, gri = #gri{aspect = examine}}) -> #{
    required => #{
        <<"token">> => {token, any}
    }
};
validate(#el_req{operation = create, gri = #gri{aspect = confine}}) ->
    #{
        required => #{
            <<"token">> => {token, any},
            <<"caveats">> => {caveats, any}
        }
    };
validate(#el_req{operation = create, gri = #gri{aspect = infer_access_token_scope}}) -> #{
    required => #{
        <<"token">> => {token, any}
    },
    optional => #{
        <<"consumerToken">> => {token, any}
    }
};
validate(#el_req{operation = create, gri = #gri{aspect = verify_access_token}}) ->
    validate_verify_operation(#{
        <<"interface">> => {atom, cv_interface:valid_interfaces()},
        <<"serviceToken">> => {token, any},
        <<"allowDataAccessCaveats">> => {boolean, any}
    });
validate(#el_req{operation = create, gri = #gri{aspect = verify_identity_token}}) ->
    validate_verify_operation(#{
        <<"interface">> => {atom, cv_interface:valid_interfaces()}
    });
validate(#el_req{operation = create, gri = #gri{aspect = verify_invite_token}}) ->
    validate_verify_operation(#{
        <<"expectedInviteType">> => {invite_type, any}
    });

validate(#el_req{operation = create, gri = #gri{aspect = {user_named_token, UserId}}, data = Data}) ->
    validate_create_operation(named, ?SUB(user, UserId), Data);
validate(#el_req{operation = create, gri = #gri{aspect = {provider_named_token, ProviderId}}, data = Data}) ->
    validate_create_operation(named, ?SUB(?ONEPROVIDER, ProviderId), Data);

validate(#el_req{operation = create, gri = #gri{aspect = {user_temporary_token, UserId}}, data = Data}) ->
    validate_create_operation(temporary, ?SUB(user, UserId), Data);
validate(#el_req{operation = create, gri = #gri{aspect = {provider_temporary_token, ProviderId}}, data = Data}) ->
    validate_create_operation(temporary, ?SUB(?ONEPROVIDER, ProviderId), Data);

validate(Req = #el_req{operation = create, gri = #gri{aspect = {offline_user_access_token, _}}}) ->
    % the data spec is the same for these two operations
    validate(Req#el_req{gri = #gri{aspect = verify_access_token}});

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"customMetadata">> => {json, any},
        <<"revoked">> => {boolean, any}
    }
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec validate_verify_operation(OptionalParams :: map()) -> entity_logic_sanitizer:sanitizer_spec().
validate_verify_operation(OptionalParams) -> #{
    required => #{
        <<"token">> => {token, any}
    },
    optional => OptionalParams#{
        <<"peerIp">> => {ipv4_address, any},
        <<"consumerToken">> => {token, any}
    }
}.


%% @private
-spec validate_create_operation(named | temporary, aai:subject(), entity_logic:data()) ->
    entity_logic_sanitizer:sanitizer_spec().
validate_create_operation(named, Subject, Data) -> #{
    required => #{
        <<"name">> => {binary, name}
    },
    optional => maps:merge(
        optional_invite_token_parameter_specs(Data),
        #{
            <<"type">> => {token_type, fun(Type) -> validate_type(Subject, named, Type, Data) end},
            <<"caveats">> => {caveats, any},
            <<"customMetadata">> => {json, any},
            <<"revoked">> => {boolean, any}
        }
    )
};
validate_create_operation(temporary, Subject, Data) -> #{
    optional => #{
        <<"type">> => {token_type, fun(Type) -> validate_type(Subject, temporary, Type, Data) end},
        <<"caveats">> => {caveats, any}
    }
}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns optional parameters for creating a named invite token in case such
%% token type is specified in the data parameters.
%% @end
%%--------------------------------------------------------------------
-spec optional_invite_token_parameter_specs(entity_logic:data()) -> entity_logic_sanitizer:parameter_specs().
optional_invite_token_parameter_specs(#{<<"type">> := TokenTypeJSON} = Data) when is_map(TokenTypeJSON) ->
    case token_type:sanitize(TokenTypeJSON) of
        {true, SanitizedType} ->
            optional_invite_token_parameter_specs(Data#{<<"type">> := SanitizedType});
        false ->
            throw(?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>))
    end;
optional_invite_token_parameter_specs(#{<<"type">> := ?INVITE_TOKEN(InviteType, _)}) ->
    token_metadata:optional_invite_token_parameter_specs(InviteType);
optional_invite_token_parameter_specs(_) ->
    #{}.


%% @private
-spec validate_type(aai:subject(), named | temporary, tokens:type(), entity_logic:data()) -> boolean().
validate_type(?SUB(user), _, ?ACCESS_TOKEN, _Data) ->
    true;
validate_type(?SUB(?ONEPROVIDER), _, ?ACCESS_TOKEN, _Data) ->
    true;

validate_type(?SUB(user), _, ?IDENTITY_TOKEN, _Data) ->
    true;
validate_type(?SUB(?ONEPROVIDER), _, ?IDENTITY_TOKEN, _Data) ->
    true;

validate_type(Subject, Persistence, ?INVITE_TOKEN(InviteType, EntityId), Data) ->
    PrivilegesProfile = case Persistence of
        temporary -> default_privileges;
        named -> token_metadata:inspect_requested_privileges(InviteType, Data)
    end,
    invite_tokens:ensure_valid_invitation(Subject, InviteType, EntityId, PrivilegesProfile);

validate_type(_, _, _, _) ->
    false.


%% @private
-spec build_token_verification_auth_ctx(aai:auth(), entity_logic:data()) -> aai:auth_ctx().
build_token_verification_auth_ctx(Auth, Data) ->
    PeerIp = case maps:find(<<"peerIp">>, Data) of
        {ok, P} -> utils:null_to_undefined(P);
        error -> Auth#auth.peer_ip
    end,
    Interface = utils:null_to_undefined(maps:get(<<"interface">>, Data, undefined)),
    Service = case utils:null_to_undefined(maps:get(<<"serviceToken">>, Data, undefined)) of
        undefined ->
            undefined;
        ServiceToken ->
            case token_auth:verify_service_token(ServiceToken, #auth_ctx{ip = PeerIp, interface = Interface}) of
                {ok, Srv} -> Srv;
                {error, _} = Error1 -> throw(Error1)
            end
    end,
    Consumer = case maps:find(<<"consumerToken">>, Data) of
        error ->
            Auth#auth.subject;
        {ok, null} ->
            undefined;
        {ok, ConsumerToken} ->
            ?check(token_auth:verify_consumer_token(ConsumerToken, #auth_ctx{ip = PeerIp, interface = Interface}))
    end,
    DataAccessCaveatsPolicy = case maps:get(<<"allowDataAccessCaveats">>, Data, false) of
        true -> allow_data_access_caveats;
        false -> disallow_data_access_caveats
    end,
    #auth_ctx{
        ip = PeerIp, interface = Interface, service = Service,
        consumer = Consumer, data_access_caveats_policy = DataAccessCaveatsPolicy,
        % allow any session when verifying tokens, the session is checked only if the token
        % is used by a client to authenticate (then, a matching session cookie must be provided)
        session_id = any
    }.


%% @private
-spec create_named_token(aai:subject(), entity_logic:data()) ->
    entity_logic:create_result().
create_named_token(Subject, Data) ->
    TokenName = maps:get(<<"name">>, Data),
    TokenId = datastore_key:new(),
    Type = maps:get(<<"type">>, Data, ?ACCESS_TOKEN),
    Caveats = maps:get(<<"caveats">>, Data, []),
    validate_subject_and_service(Type, Subject, Caveats),
    case token_names:register(Subject, TokenName, TokenId) of
        ok -> ok;
        ?ERROR_ALREADY_EXISTS -> throw(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"name">>))
    end,

    CustomMetadata = maps:get(<<"customMetadata">>, Data, #{}),
    Secret = tokens:generate_secret(),
    TokenRecord = #od_token{
        name = TokenName,
        subject = Subject,
        type = Type,
        secret = Secret,
        caveats = Caveats,
        metadata = token_metadata:build(Type, CustomMetadata, Data),
        revoked = maps:get(<<"revoked">>, Data, false)
    },
    {ok, _} = od_token:create(#document{key = TokenId, value = TokenRecord}),
    NewGRI = #gri{type = od_token, id = TokenId, aspect = instance},
    {true, {NamedToken, Rev}} = fetch_entity(NewGRI),
    TokenData = to_token_data(TokenId, NamedToken),
    {ok, resource, {NewGRI, {TokenData, Rev}}}.


%% @private
-spec create_temporary_token(aai:subject(), entity_logic:data()) ->
    entity_logic:create_result().
create_temporary_token(Subject, Data) ->
    Type = maps:get(<<"type">>, Data, ?ACCESS_TOKEN),
    Caveats = maps:get(<<"caveats">>, Data, []),
    validate_subject_and_service(Type, Subject, Caveats),

    MaxTTL = ?MAX_TEMPORARY_TOKEN_TTL,
    IsTtlAllowed = case caveats:infer_ttl(Caveats) of
        undefined -> false;
        TTL -> TTL =< MaxTTL
    end,
    IsTtlAllowed orelse throw(?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTTL)),

    {ok, {Secret, Generation}} = temporary_token_secret:get_for_subject(Subject),
    Prototype = #token{
        onezone_domain = oz_worker:get_domain(),
        id = datastore_key:new(),
        subject = Subject,
        type = Type,
        persistence = {temporary, Generation}
    },
    Token = tokens:construct(Prototype, Secret, Caveats),
    {ok, value, Token}.


%% @private
-spec validate_subject_and_service(tokens:type(), aai:subject(), [caveats:caveat()]) ->
    ok | no_return().
validate_subject_and_service(Type, Subject, Caveats) ->
    ServicesToCheck = case caveats:filter([cv_service], Caveats) of
        [] ->
            [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)];
        ServiceCaveats ->
            lists:flatten([Whitelist || #cv_service{whitelist = Whitelist} <- ServiceCaveats])
    end,
    lists:foreach(fun(Service) ->
        case token_auth:validate_subject_and_service(Type, Subject, Service) of
            ok -> ok;
            {error, _} = Error -> throw(Error)
        end
    end, ServicesToCheck).


%% @private
-spec delete_named_token(aai:subject(), od_token:name(), tokens:id()) -> ok | no_return().
delete_named_token(Subject, TokenName, TokenId) ->
    ok = token_names:unregister(Subject, TokenName),
    ok = od_token:delete(TokenId).


%% @private
-spec to_token_data(tokens:id()) -> entity_logic:data().
to_token_data(TokenId) ->
    {ok, #document{value = NamedToken}} = od_token:get(TokenId),
    to_token_data(TokenId, NamedToken).


%% @private
-spec to_token_data(tokens:id(), od_token:record()) -> entity_logic:data().
to_token_data(TokenId, NamedToken) ->
    #{
        <<"id">> => TokenId,
        <<"name">> => NamedToken#od_token.name,
        <<"subject">> => NamedToken#od_token.subject,
        <<"type">> => NamedToken#od_token.type,
        <<"caveats">> => NamedToken#od_token.caveats,
        <<"metadata">> => NamedToken#od_token.metadata,
        <<"revoked">> => NamedToken#od_token.revoked,
        <<"token">> => od_token:named_token_to_token(TokenId, NamedToken)
    }.


%% @private
-spec infer_access_token_scope(aai:auth(), tokens:token()) -> json_utils:json_map().
infer_access_token_scope(Auth, Token) ->
    Caveats = tokens:get_caveats(Token),
    #{
        <<"validUntil">> => utils:undefined_to_null(caveats:infer_expiration_time(Caveats)),
        <<"dataAccessScope">> => case Auth of
            ?USER(UserId) ->
                EffSpaces = ?check(user_logic:get_eff_spaces(?ROOT, UserId)),
                AvailableSpaces = data_access_caveats:match_available_spaces(
                    data_access_caveats:filter(Caveats), EffSpaces
                ),
                build_data_access_scope_info(AvailableSpaces, Caveats);
            _ ->
                null
        end
        % NOTE: this object structure is designed to allow extensions;
        %       e.g. "apiAccessScope" may be added in the future if needed
    }.


%% @private
-spec build_data_access_scope_info([od_space:id()], [caveats:caveat()]) -> json_utils:json_map().
build_data_access_scope_info(SpaceIds, Caveats) ->
    SpacesById = maps_utils:generate_from_list(fun(S) -> ?check(od_space:get_record(S)) end, SpaceIds),

    StorageIds = lists:usort(maps:fold(fun(_, #od_space{storages = Storages}, Acc) ->
        maps:keys(Storages) ++ Acc
    end, [], SpacesById)),
    StoragesById = maps_utils:generate_from_list(fun(S) -> ?check(od_storage:get_record(S)) end, StorageIds),

    AllProviderIds = lists:usort(maps:fold(fun(_, #od_storage{provider = P}, Acc) -> [P | Acc] end, [], StoragesById)),
    AllowedProviderIds = service_caveats:match_allowed_service_ids(
        service_caveats:filter(Caveats), ?OP_WORKER, AllProviderIds
    ),
    AllowedProvidersById = maps_utils:generate_from_list(fun(P) ->
        ?check(od_provider:get_record(P))
    end, AllowedProviderIds),

    #{
        <<"readonly">> => lists:member(#cv_data_readonly{}, Caveats),
        <<"spaces">> => maps:map(fun(_SpaceId, #od_space{name = Name, storages = Storages}) ->
            #{
                <<"name">> => Name,
                <<"supports">> => maps:fold(fun(StorageId, _, Acc) ->
                    #od_storage{provider = ProviderId, readonly = Readonly} = maps:get(StorageId, StoragesById),
                    case lists:member(ProviderId, AllowedProviderIds) of
                        true ->
                            Acc#{ProviderId => #{
                                % the support of a provider is marked as readonly when
                                % all its supporting storages are readonly
                                <<"readonly">> => Readonly andalso kv_utils:get([ProviderId, <<"readonly">>], Acc, true)
                            }};
                        false ->
                            Acc
                    end
                end, #{}, Storages)
            }
        end, SpacesById),
        <<"providers">> => maps:map(fun(ProviderId, #od_provider{name = Name, domain = Domain}) ->
            #{
                <<"name">> => Name,
                <<"domain">> => Domain,
                <<"version">> => ?check(cluster_logic:get_worker_release_version(?ROOT, ProviderId)),
                <<"online">> => provider_connections:is_online(ProviderId)
            }
        end, AllowedProvidersById)
    }.
