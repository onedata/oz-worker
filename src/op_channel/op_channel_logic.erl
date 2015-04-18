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
-include_lib("prproto/include/gr_messages.hrl").
-include_lib("prproto/include/gr_communication_protocol.hrl").

-define(ENCODING_MODULE, gr_messages).
-define(PROTOCOL_VERSION, 1).

%% API
-export([push/2, push/4]).
-export([space_modified/3, space_removed/2]).
-export([user_modified/3, user_removed/2]).
-export([group_modified/3, group_removed/2]).

%% push/2
%% ====================================================================
%% @doc Pushes message to providers.
%% @end
-spec push(ProviderIds :: [binary()], Msg :: iolist() | binary()) -> ok.
%% ====================================================================
push(ProviderIds, Msg) ->
    gen_server:cast(?OpChannel, {push, ProviderIds, Msg}).

%% push/4
%% ====================================================================
%% @doc Encodes message into protocol buffers frame and pushes it
%% to providers.
%% @end
-spec push(ProviderIds :: [binary()], ProtocolVersion :: integer(),
    EncodingModule :: module(), Msg :: term()) -> ok.
%% ====================================================================
push(ProviderIds, ProtocolVersion, EncodingModule, Msg) ->
    {ok, EncodedInput} = pb:encode(EncodingModule, Msg),
    {ok, EncodedMsg} = pb:encode(gr_communication_protocol, #'Message'{
        protocol_version = ProtocolVersion,
        message_type = atom_to_list(element(1, Msg)),
        message_decoder_name = atom_to_list(EncodingModule),
        input = EncodedInput
    }),
    push(ProviderIds, EncodedMsg).

%% space_modified/3
%% ====================================================================
%% @doc Notifies given providers about space modification.
%% @end
-spec space_modified(ProviderIds :: [binary()], SpaceId :: binary(),
    Space :: space_info()) -> ok.
%% ====================================================================
space_modified(ProviderIds, SpaceId, #space{name = Name, size = Size,
    users = Users, groups = Groups, providers = Providers}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?ENCODING_MODULE, #'SpaceModified'{
        id = SpaceId,
        name = Name,
        size = lists:map(fun({ProviderId, SupportedSize}) ->
            #'SpaceModified.Size'{provider = ProviderId, size = SupportedSize}
        end, Size),
        users = lists:map(fun({UserId, _}) -> UserId end, Users),
        groups = lists:map(fun({GroupId, _}) -> GroupId end, Groups),
        providers = Providers
    }).

%% space_removed/2
%% ====================================================================
%% @doc Notifies given providers about space removal.
%% @end
-spec space_removed(ProviderIds :: [binary()], SpaceId :: binary()) -> ok.
%% ====================================================================
space_removed(ProviderIds, SpaceId) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?ENCODING_MODULE, #'SpaceRemoved'{
        id = SpaceId
    }).

%% user_modified/3
%% ====================================================================
%% @doc Notifies given providers about user modification.
%% @end
-spec user_modified(ProviderIds :: [binary()], UserId :: binary(),
    User :: user_info()) -> ok.
%% ====================================================================
user_modified(ProviderIds, UserId, #user{default_space = undefined,
    spaces = Spaces, groups = Groups}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?ENCODING_MODULE, #'UserModified'{
        id = UserId, spaces = Spaces, groups = Groups
    });

user_modified(ProviderIds, UserId, #user{default_space = DefaultSpace,
    spaces = Spaces, groups = Groups}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?ENCODING_MODULE,
        #'UserModified'{id = UserId, spaces = [DefaultSpace |
            lists:delete(DefaultSpace, Spaces)], groups = Groups
        }
    ).

%% user_removed/2
%% ====================================================================
%% @doc Notifies given providers about user removal.
%% @end
-spec user_removed(ProviderIds :: [binary()], UserId :: binary()) -> ok.
%% ====================================================================
user_removed(ProviderIds, UserId) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?ENCODING_MODULE, #'UserRemoved'{
        id = UserId
    }).

%% group_modified/3
%% ====================================================================
%% @doc Notifies given providers about group modification.
%% @end
-spec group_modified(ProviderIds :: [binary()], GroupId :: binary(),
    Group :: group_info()) -> ok.
%% ====================================================================
group_modified(ProviderIds, GroupId, #user_group{name = Name}) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?ENCODING_MODULE, #'GroupModified'{
        id = GroupId, name = Name
    }).

%% group_removed/2
%% ====================================================================
%% @doc Notifies given providers about group removal.
%% @end
-spec group_removed(ProviderIds :: [binary()], GroupId :: binary()) -> ok.
%% ====================================================================
group_removed(ProviderIds, GroupId) ->
    push(ProviderIds, ?PROTOCOL_VERSION, ?ENCODING_MODULE, #'GroupRemoved'{
        id = GroupId
    }).