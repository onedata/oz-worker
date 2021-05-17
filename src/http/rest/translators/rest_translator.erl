%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of request results to REST
%%% responses, designed to match entity logic API.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_translator).
-author("Lukasz Opiola").

-include("http/rest.hrl").
-include("registered_names.hrl").

-export([response/2]).

%% Convenience functions for rest translators
-export([
    ok_body_reply/1,
    ok_no_content_reply/0,
    created_reply_with_body/1,
    created_reply_with_location/1,
    created_reply_with_location_and_body/2,
    updated_reply/0,
    deleted_reply/0,
    ok_encoded_intermediaries_reply/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec response(undefined | entity_logic:req(), entity_logic:result()) ->
    rest_handler:rest_resp().
response(_, {error, _} = Error) ->
    #rest_resp{
        code = errors:to_http_code(Error),
        body = #{<<"error">> => errors:to_json(Error)}
    };
response(#el_req{operation = create}, ok) ->
    % No need for translation, 'ok' means success with no response data
    rest_translator:ok_no_content_reply();
response(#el_req{operation = create} = ElReq, {ok, DataFormat, Result}) ->
    #el_req{gri = GRI = #gri{type = Model}, auth_hint = AuthHint} = ElReq,
    Translator = entity_type_to_translator(Model),
    Translator:create_response(GRI, AuthHint, DataFormat, Result);
response(#el_req{operation = get} = ElReq, {ok, Data}) ->
    #el_req{gri = GRI = #gri{type = EntityType}} = ElReq,
    Translator = entity_type_to_translator(EntityType),
    Translator:get_response(GRI, Data);
response(#el_req{operation = update}, ok) ->
    updated_reply();
response(#el_req{operation = delete}, ok) ->
    deleted_reply().


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST operations that send
%% a body in response.
%% @end
%%--------------------------------------------------------------------
-spec ok_body_reply(json_utils:json_term() | {binary, binary()}) -> rest_handler:rest_resp().
ok_body_reply(Body) ->
    #rest_resp{code = ?HTTP_200_OK, body = Body}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST operations that do not
%% send any body in response.
%% @end
%%--------------------------------------------------------------------
-spec ok_no_content_reply() -> rest_handler:rest_resp().
ok_no_content_reply() ->
    #rest_resp{code = ?HTTP_204_NO_CONTENT}.


%%--------------------------------------------------------------------
%% @doc
%% Returns 201 CREATED with a response body.
%% @end
%%--------------------------------------------------------------------
-spec created_reply_with_body(json_utils:json_term()) -> rest_handler:rest_resp().
created_reply_with_body(Body) ->
    #rest_resp{code = ?HTTP_201_CREATED, body = Body}.


%%--------------------------------------------------------------------
%% @doc
%% Returns 201 CREATED with proper location headers.
%% @end
%%--------------------------------------------------------------------
-spec created_reply_with_location(PathTokens :: [binary()]) -> rest_handler:rest_resp().
created_reply_with_location(PathTokens) ->
    LocationHeader = #{<<"Location">> => oz_worker:get_rest_uri(filename:join(["/" | PathTokens]))},
    #rest_resp{code = ?HTTP_201_CREATED, headers = LocationHeader}.


%%--------------------------------------------------------------------
%% @doc
%% Returns 201 CREATED with proper location headers and a response body.
%% @end
%%--------------------------------------------------------------------
-spec created_reply_with_location_and_body(PathTokens :: [binary()], json_utils:json_term()) ->
    rest_handler:rest_resp().
created_reply_with_location_and_body(PathTokens, Body) ->
    CreatedReply = created_reply_with_location(PathTokens),
    CreatedReply#rest_resp{body = Body}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST updates.
%% @end
%%--------------------------------------------------------------------
-spec updated_reply() -> rest_handler:rest_resp().
updated_reply() ->
    #rest_resp{code = ?HTTP_204_NO_CONTENT}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST deletions.
%% @end
%%--------------------------------------------------------------------
-spec deleted_reply() -> rest_handler:rest_resp().
deleted_reply() ->
    #rest_resp{code = ?HTTP_204_NO_CONTENT}.


%%--------------------------------------------------------------------
%% @doc
%% REST reply that should be used for successful REST operations returning
%% membership intermediaries.
%% @end
%%--------------------------------------------------------------------
-spec ok_encoded_intermediaries_reply(entity_graph:intermediaries()) -> rest_handler:rest_resp().
ok_encoded_intermediaries_reply(Intermediaries) ->
    ok_body_reply(#{<<"intermediaries">> => lists:map(fun({Type, Id}) ->
        #{<<"type">> => gri:serialize_type(Type), <<"id">> => Id}
    end, Intermediaries)}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns rest translator for given entity type.
%% @end
%%--------------------------------------------------------------------
-spec entity_type_to_translator(atom()) -> module().
entity_type_to_translator(od_user) -> user_rest_translator;
entity_type_to_translator(od_group) -> group_rest_translator;
entity_type_to_translator(od_space) -> space_rest_translator;
entity_type_to_translator(od_share) -> share_rest_translator;
entity_type_to_translator(od_provider) -> provider_rest_translator;
entity_type_to_translator(od_handle_service) -> handle_service_rest_translator;
entity_type_to_translator(od_handle) -> handle_rest_translator;
entity_type_to_translator(od_harvester) -> harvester_rest_translator;
entity_type_to_translator(od_cluster) -> cluster_rest_translator;
entity_type_to_translator(od_token) -> token_rest_translator;
entity_type_to_translator(temporary_token_secret) -> token_rest_translator;
entity_type_to_translator(od_atm_inventory) -> atm_inventory_rest_translator;
entity_type_to_translator(od_atm_lambda) -> atm_lambda_rest_translator;
entity_type_to_translator(oz_worker) -> zone_rest_translator.
