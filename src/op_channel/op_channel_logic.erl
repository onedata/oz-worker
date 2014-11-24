%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides logic for communication with oneproviders.
%% @end
%% ===================================================================
-module(op_channel_logic).

-include("dao/dao_types.hrl").
-include("registered_names.hrl").

%% API
-export([push/2]).

%% push/2
%% ====================================================================
%% @doc Pushes message to given providers or all providers that support
%% given Space.
%% @end
-spec push({provider, ProviderId | [ProviderId]} | {space, SpaceId}, Msg :: term()) -> ok when
    ProviderId :: provider_id(),
    SpaceId :: space_id().
%% ====================================================================
push({provider, ProviderId}, Msg) when is_binary(ProviderId) ->
    gen_server:cast(?OpChannel, {push, [ProviderId], Msg});

push({provider, ProviderIds}, Msg) when is_list(ProviderIds) ->
    gen_server:cast(?OpChannel, {push, ProviderIds, Msg});

push({space, SpaceId}, Msg) ->
    #db_document{record = #space{providers = ProviderIds}} = dao_adapter:space_doc(SpaceId),
    push({provider, ProviderIds}, Msg).