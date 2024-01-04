%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all token logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(token_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

%% API
-export([examine/2]).
-export([confine/2]).
-export([verify_access_token/2]).
-export([verify_identity_token/2]).
-export([verify_invite_token/2]).
-export([infer_access_token_scope/2]).
-export([create_user_named_token/3, create_provider_named_token/3]).
-export([create_user_temporary_token/3, create_provider_temporary_token/3]).
-export([create_access_token_for_gui/4]).
-export([create_offline_user_access_token/3]).
-export([list/1]).
-export([list_user_named_tokens/2, list_provider_named_tokens/2]).
-export([get_named_token/2]).
-export([get_named_token_status/2]).
-export([get_user_named_token_by_name/3]).
-export([get_provider_named_token_by_name/3]).
-export([get_user_temporary_token_generation/2, get_provider_temporary_token_generation/2]).
-export([exists/1]).
-export([update_named_token/3]).
-export([delete_named_token/2]).
-export([delete_all_user_named_tokens/2, delete_all_provider_named_tokens/2]).
-export([revoke_all_user_temporary_tokens/2, revoke_all_provider_temporary_tokens/2]).
-export([create_legacy_invite_token/2, create_legacy_client_token/1]).


-define(GUI_TOKEN_TTL, oz_worker:get_env(gui_token_ttl, 600)).

%%%===================================================================
%%% API
%%%===================================================================

-spec examine(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
examine(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = examine, scope = public},
        data = Data
    })).


-spec confine(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
confine(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = confine, scope = public},
        data = Data
    })).


-spec verify_access_token(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
verify_access_token(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = verify_access_token, scope = public},
        data = Data
    })).


-spec verify_identity_token(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
verify_identity_token(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = verify_identity_token, scope = public},
        data = Data
    })).


-spec verify_invite_token(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
verify_invite_token(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = verify_invite_token, scope = public},
        data = Data
    })).


-spec infer_access_token_scope(aai:auth(), entity_logic:data()) ->
    {ok, entity_logic:data()} | errors:error().
infer_access_token_scope(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = infer_access_token_scope, scope = public},
        data = Data
    })).


-spec create_user_named_token(aai:auth(), od_user:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_user_named_token(Auth, UserId, Data) ->
    Result = entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_named_token, UserId}},
        data = Data
    }),
    case Result of
        {ok, resource, {_, {#{<<"token">> := Token}, _}}} -> {ok, Token};
        {error, _} = Error -> Error
    end.


-spec create_provider_named_token(aai:auth(), od_provider:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_provider_named_token(Auth, ProviderId, Data) ->
    Result = entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_named_token, ProviderId}},
        data = Data
    }),
    case Result of
        {ok, resource, {_, {#{<<"token">> := Token}, _}}} -> {ok, Token};
        {error, _} = Error -> Error
    end.


-spec create_user_temporary_token(aai:auth(), od_user:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_user_temporary_token(Auth, UserId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_temporary_token, UserId}},
        data = Data
    })).


-spec create_provider_temporary_token(aai:auth(), od_provider:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_provider_temporary_token(Auth, ProviderId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_temporary_token, ProviderId}},
        data = Data
    })).


-spec create_access_token_for_gui(aai:auth(), od_user:id(), session:id(), aai:service_spec()) ->
    {ok, {tokens:token(), time:seconds()}} | errors:error().
create_access_token_for_gui(Auth, UserId, SessionId, Service) ->
    Ttl = ?GUI_TOKEN_TTL,
    Result = create_user_temporary_token(Auth, UserId, #{
        <<"type">> => ?ACCESS_TOKEN(SessionId),
        <<"caveats">> => [
            #cv_time{valid_until = global_clock:timestamp_seconds() + Ttl},
            #cv_service{whitelist = [Service]}
            % @TODO VFS-5913 Add interface caveat when it is fully supported by Onepanel
            % note that there are some problems with that:
            % * the interface caveat will break harvester GUI uploads and file uploads in Oneprovider
            % * due to the above, either the caveat must be added depending on the service version,
            %   or it must be introduced gradually over 2 major releases to ensure backward compatibility
        ]
    }),
    case Result of
        {ok, Token} -> {ok, {Token, Ttl}};
        {error, _} = Error -> Error
    end.


-spec create_offline_user_access_token(aai:auth(), od_user:id(), entity_logic:data()) ->
    {ok, tokens:token()} | errors:error().
create_offline_user_access_token(Auth, UserId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {offline_user_access_token, UserId}},
        data = Data
    })).


-spec list(aai:auth()) -> {ok, [tokens:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = list}
    }).


-spec list_user_named_tokens(aai:auth(), od_user:id()) ->
    {ok, [tokens:id()]} | errors:error().
list_user_named_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_named_tokens, UserId}}
    }).


-spec list_provider_named_tokens(aai:auth(), od_provider:id()) ->
    {ok, [tokens:id()]} | errors:error().
list_provider_named_tokens(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ProviderId}}
    }).


-spec get_named_token(aai:auth(), tokens:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_named_token(Auth, TokenId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenId, aspect = instance}
    }).


%% @doc Returns if the token is currently revoked
-spec get_named_token_status(aai:auth(), tokens:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_named_token_status(Auth, TokenId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenId, aspect = instance, scope = shared}
    }).


-spec get_user_named_token_by_name(aai:auth(), od_user:id(), od_token:name()) ->
    {ok, entity_logic:data()} | errors:error().
get_user_named_token_by_name(Auth, UserId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {user_named_token, UserId}}
    }).


-spec get_provider_named_token_by_name(aai:auth(), od_provider:id(), od_token:name()) ->
    {ok, entity_logic:data()} | errors:error().
get_provider_named_token_by_name(Auth, ProviderId, TokenName) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenName, aspect = {provider_named_token, ProviderId}}
    }).


-spec get_user_temporary_token_generation(aai:auth(), od_user:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_user_temporary_token_generation(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = temporary_token_secret, id = UserId, aspect = user, scope = shared}
    }).


-spec get_provider_temporary_token_generation(aai:auth(), od_provider:id()) ->
    {ok, entity_logic:data()} | errors:error().
get_provider_temporary_token_generation(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = temporary_token_secret, id = ProviderId, aspect = provider, scope = shared}
    }).


-spec exists(tokens:id()) -> boolean().
exists(TokenId) ->
    {ok, Exists} = od_token:exists(TokenId),
    Exists.


-spec update_named_token(aai:auth(), tokens:id(), entity_logic:data()) ->
    ok | errors:error().
update_named_token(Auth, TokenId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenId, aspect = instance},
        data = Data
    }).


-spec delete_named_token(aai:auth(), tokens:id()) -> ok | errors:error().
delete_named_token(Auth, TokenId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = TokenId, aspect = instance}
    }).


-spec delete_all_user_named_tokens(aai:auth(), od_user:id()) -> ok | errors:error().
delete_all_user_named_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_named_tokens, UserId}}
    }).


-spec delete_all_provider_named_tokens(aai:auth(), od_provider:id()) -> ok | errors:error().
delete_all_provider_named_tokens(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_named_tokens, ProviderId}}
    }).


-spec revoke_all_user_temporary_tokens(aai:auth(), od_user:id()) -> ok | errors:error().
revoke_all_user_temporary_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {user_temporary_tokens, UserId}}
    }).


-spec revoke_all_provider_temporary_tokens(aai:auth(), od_provider:id()) -> ok | errors:error().
revoke_all_provider_temporary_tokens(Auth, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_token, id = undefined, aspect = {provider_temporary_tokens, ProviderId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 20.02.*
%% Function used from *_logic_plugin modules to create legacy invite tokens.
%% Token names are autogenerated and usage is limited to 1.
%% @end
%%--------------------------------------------------------------------
-spec create_legacy_invite_token(aai:auth(), tokens:type()) ->
    entity_logic:create_result().
create_legacy_invite_token(Auth, Type = ?INVITE_TOKEN(InviteType, TargetEntityId)) ->
    Data = #{
        <<"name">> => gen_invite_token_name(InviteType, TargetEntityId),
        <<"type">> => Type,
        <<"usageLimit">> => 1
    },
    Result = case Auth#auth.subject of
        ?SUB(user, UserId) -> create_user_named_token(Auth, UserId, Data);
        ?SUB(?ONEPROVIDER, PrId) -> create_provider_named_token(Auth, PrId, Data)
    end,
    case Result of
        {ok, Token} -> {ok, value, Token};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% @TODO VFS-5846 old client tokens API kept for backward compatibility
%% Creates an access token with the parameters of a legacy client token.
%% @end
%%--------------------------------------------------------------------
-spec create_legacy_client_token(aai:auth()) -> {ok, tokens:token()} | errors:error().
create_legacy_client_token(Auth = ?USER(UserId)) ->
    Data = #{
        <<"name">> => <<
            "access token ",
            (binary:replace(
                time:seconds_to_iso8601(global_clock:timestamp_seconds()),
                <<$:>>, <<$.>>, [global]
            ))/binary, " ",
            (str_utils:rand_hex(3))/binary
        >>,
        <<"type">> => ?ACCESS_TOKEN,
        <<"caveats">> => [
            #cv_time{valid_until = global_clock:timestamp_seconds() + 31536000}  % 1 year
        ]
    },
    create_user_named_token(Auth, UserId, Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec gen_invite_token_name(token_type:invite_type(), gri:entity_id()) -> binary().
gen_invite_token_name(?USER_JOIN_GROUP, GroupId) ->
    {ok, Name} = group_logic:get_name(?ROOT, GroupId),
    format_invite_token_name(<<"user invite to group">>, Name);
gen_invite_token_name(?GROUP_JOIN_GROUP, GroupId) ->
    {ok, Name} = group_logic:get_name(?ROOT, GroupId),
    format_invite_token_name(<<"group invite to group">>, Name);
gen_invite_token_name(?USER_JOIN_SPACE, SpaceId) ->
    {ok, Name} = space_logic:get_name(?ROOT, SpaceId),
    format_invite_token_name(<<"user invite to space">>, Name);
gen_invite_token_name(?GROUP_JOIN_SPACE, SpaceId) ->
    {ok, Name} = space_logic:get_name(?ROOT, SpaceId),
    format_invite_token_name(<<"group invite to space">>, Name);
gen_invite_token_name(?SUPPORT_SPACE, SpaceId) ->
    {ok, Name} = space_logic:get_name(?ROOT, SpaceId),
    format_invite_token_name(<<"support request for space">>, Name);
gen_invite_token_name(?REGISTER_ONEPROVIDER, AdminUserId) ->
    {ok, Name} = user_logic:get_full_name(?ROOT, AdminUserId),
    format_invite_token_name(<<"Oneprovider rgstr. token for">>, Name);
gen_invite_token_name(?USER_JOIN_CLUSTER, ClusterId) ->
    Name = case ClusterId of
        ?ONEZONE_CLUSTER_ID -> <<"Onezone">>;
        _ -> element(2, {ok, _} = provider_logic:get_name(?ROOT, ClusterId))
    end,
    format_invite_token_name(<<"user invite to cluster">>, Name);
gen_invite_token_name(?GROUP_JOIN_CLUSTER, ClusterId) ->
    Name = case ClusterId of
        ?ONEZONE_CLUSTER_ID -> <<"Onezone">>;
        _ -> element(2, {ok, _} = provider_logic:get_name(?ROOT, ClusterId))
    end,
    format_invite_token_name(<<"group invite to cluster">>, Name);
gen_invite_token_name(?USER_JOIN_HARVESTER, HarvesterId) ->
    {ok, Name} = harvester_logic:get_name(?ROOT, HarvesterId),
    format_invite_token_name(<<"user invite to harvester">>, Name);
gen_invite_token_name(?GROUP_JOIN_HARVESTER, HarvesterId) ->
    {ok, Name} = harvester_logic:get_name(?ROOT, HarvesterId),
    format_invite_token_name(<<"group invite to harvester">>, Name);
gen_invite_token_name(?SPACE_JOIN_HARVESTER, HarvesterId) ->
    {ok, Name} = harvester_logic:get_name(?ROOT, HarvesterId),
    format_invite_token_name(<<"space invite to harvester">>, Name);
gen_invite_token_name(?USER_JOIN_ATM_INVENTORY, AtmInventoryId) ->
    {ok, Name} = atm_inventory_logic:get_name(?ROOT, AtmInventoryId),
    format_invite_token_name(<<"user invite to atm inventory">>, Name);
gen_invite_token_name(?GROUP_JOIN_ATM_INVENTORY, AtmInventoryId) ->
    {ok, Name} = atm_inventory_logic:get_name(?ROOT, AtmInventoryId),
    format_invite_token_name(<<"group invite to atm inventory">>, Name).


%% @private
-spec format_invite_token_name(binary(), binary()) -> binary().
format_invite_token_name(Description, EntityName) ->
    TokenName = str_utils:format_bin("~s - ~ts ~s", [Description, EntityName, str_utils:rand_hex(3)]),
    TokenNameSize = size(TokenName),
    case TokenNameSize =< 50 of
        true ->
            TokenName;
        false ->
            EntityNameSize = size(EntityName),
            % Trim 3 extra characters for the ellipsis "..."
            TrimmedName = binary:part(EntityName, 0, EntityNameSize - (TokenNameSize - 47)),
            format_invite_token_name(Description, <<TrimmedName/binary, "...">>)
    end.
