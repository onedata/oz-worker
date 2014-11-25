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
-include_lib("ctool/include/logging.hrl").
-include("gr_messages_pb.hrl").
-include("gr_communication_protocol_pb.hrl").

-define(MESSAGE_TYPE, "gr_messages").
-define(PROTOCOL_VERSION, 1).

%% API
-export([push/4]).
-export([space_modified/3, space_removed/2]).
-export([user_modified/3, user_removed/2]).
-export([group_modified/3, group_removed/2]).

%% push/2
%% ====================================================================
%% @doc Pushes message to given providers or all providers that support
%% given Space.
%% @end
-spec push(ProviderIds :: [provider_id()], ProtocolVersion :: integer(), MsgType :: string(), Msg :: term()) -> ok.
%% ====================================================================
push(ProviderIds, ProtocolVersion, MsgType, Msg) ->
    ?dump(ProviderIds),
    ?dump(ProtocolVersion),
    ?dump(MsgType),
    ?dump(Msg),
    {ok, EncodedInput} = pb:encode(MsgType, Msg),
    ?dump(EncodedInput),
    {ok, EncodedMsg} = pb:encode("gr_communication_protocol", #message{
        protocol_version = ProtocolVersion,
        message_type = MsgType,
        message_decoder_name = atom_to_list(element(1, Msg)),
        input = EncodedInput
    }),
    ?dump(EncodedMsg),
    gen_server:cast(?OpChannel, {push, ProviderIds, EncodedMsg}).


%% space_modified/3
%% ====================================================================
%% @doc Notifies given providers about space modification.
%% @end
-spec space_modified(ProviderIds :: [provider_id()], SpaceId :: space_id(), Space :: space_info()) -> ok.
%% ====================================================================
space_modified(ProviderIds, SpaceId, #space{name = Name, users = Users, groups = Groups, providers = Providers}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?MESSAGE_TYPE, #spacemodified{
        id = SpaceId,
        name = Name,
        size = [],
        users = lists:map(fun({UserId, _}) -> UserId end, Users),
        groups = Groups,
        providers = Providers
    }).


%% space_removed/2
%% ====================================================================
%% @doc Notifies given providers about space removal.
%% @end
-spec space_removed(ProviderIds :: [provider_id()], SpaceId :: space_id()) -> ok.
%% ====================================================================
space_removed(ProviderIds, SpaceId) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?MESSAGE_TYPE, #spaceremoved{id = SpaceId}).


%% user_modified/3
%% ====================================================================
%% @doc Notifies given providers about user modification.
%% @end
-spec user_modified(ProviderIds :: [provider_id()], UserId :: user_id(), User :: user_info()) -> ok.
%% ====================================================================
user_modified(ProviderIds, UserId, #user{default_space = undefined, spaces = Spaces, groups = Groups}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?MESSAGE_TYPE, #usermodified{id = UserId, spaces = Spaces, groups = Groups});

user_modified(ProviderIds, UserId, #user{default_space = DefaultSpace, spaces = Spaces, groups = Groups}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?MESSAGE_TYPE,
        #usermodified{id = UserId, spaces = [DefaultSpace | lists:delete(DefaultSpace, Spaces)], groups = Groups}).


%% user_removed/2
%% ====================================================================
%% @doc Notifies given providers about user removal.
%% @end
-spec user_removed(ProviderIds :: [provider_id()], UserId :: user_id()) -> ok.
%% ====================================================================
user_removed(ProviderIds, UserId) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?MESSAGE_TYPE, #userremoved{id = UserId}).


%% group_modified/3
%% ====================================================================
%% @doc Notifies given providers about group modification.
%% @end
-spec group_modified(ProviderIds :: [provider_id()], GroupId :: group_id(), Group :: group_info()) -> ok.
%% ====================================================================
group_modified(ProviderIds, GroupId, #user_group{name = Name}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?MESSAGE_TYPE, #groupmodified{id = GroupId, name = Name}).


%% group_removed/2
%% ====================================================================
%% @doc Notifies given providers about group removal.
%% @end
-spec group_removed(ProviderIds :: [provider_id()], GroupId :: group_id()) -> ok.
%% ====================================================================
group_removed(ProviderIds, GroupId) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?MESSAGE_TYPE, #groupremoved{id = GroupId}).