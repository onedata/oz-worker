%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc This module provides common encoding and decoding functions for
%% protobuffs' messages.
%% @end
%% ===================================================================
-module(pb).

%% API
-export([encode/2, decode/3]).

%% ====================================================================
%% API
%% ====================================================================

%% encode/2
%% ====================================================================
%% @doc Encodes record using encoding module.
%% @end
-spec encode(EncodingModule :: module(), Record :: tuple()) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.
%% ====================================================================
encode(EncodingModule, Record) ->
    try
        {ok, EncodingModule:encode_msg(Record)}
    catch
        _:undef -> {error, unsupported_encoder_or_record};
        _:_ -> {error, invalid_record}
    end.

%% decode/3
%% ====================================================================
%% @doc Decodes data using decoding module and function.
%% @end
-spec decode(DecodingModule :: module(), RecordName :: atom(), Data :: binary()) ->
    {ok, Record :: tuple} | {error, Reason :: term()}.
%% ====================================================================
decode(DecodingModule, RecordName, Data) ->
    try
        {ok, DecodingModule:decode_msg(Data, RecordName)}
    catch
        _:undef -> {error, unsupported_decoder};
        _:_ -> {error, invalid_data}
    end.
