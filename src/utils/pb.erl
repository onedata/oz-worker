%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module provides common encoding and decoding functions for
%%% protobuffs' messages.
%%% @end
%%%-------------------------------------------------------------------
-module(pb).

-include_lib("ctool/include/logging.hrl").

%% API
-export([encode/2, decode/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Encodes record using encoding module.
%% @end
%%--------------------------------------------------------------------
-spec encode(EncodingModule :: module(), Record :: tuple()) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.
encode(EncodingModule, Record) ->
    try
        {ok, EncodingModule:encode_msg(Record)}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot encode message due to: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Decodes data using decoding module and record name.
%% @end
%%--------------------------------------------------------------------
-spec decode(DecodingModule :: binary() | module(), RecordName :: binary() | atom(),
    Data :: binary()) -> {ok, Record :: tuple} | {error, Reason :: term()}.
decode(DecodingModule, RecordName, Data) when is_binary(DecodingModule) ->
    try
        decode(binary_to_existing_atom(DecodingModule, utf8), RecordName, Data)
    catch
        _:Reason ->
            ?debug_stacktrace("Cannot decode message due to: ~p", [Reason]),
            {error, <<"Unsupported decoder.">>}
    end;
decode(DecodingModule, RecordName, Data) when is_binary(RecordName) ->
    try
        decode(DecodingModule, binary_to_existing_atom(RecordName, utf8), Data)
    catch
        _:Reason ->
            ?debug_stacktrace("Cannot decode message due to: ~p", [Reason]),
            {error, <<"Unsupported record.">>}
    end;
decode(DecodingModule, RecordName, Data) ->
    try
        {ok, DecodingModule:decode_msg(Data, RecordName)}
    catch
        _:Reason ->
            ?debug_stacktrace("Cannot decode message due to: ~p", [Reason]),
            {error, <<"Invalid data.">>}
    end.
