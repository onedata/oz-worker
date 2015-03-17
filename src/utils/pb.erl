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
-spec encode(EncodingModule, Record) -> {ok, Data} | {error, Reason} when
  EncodingModule :: string() | atom(),
  Record :: tuple(),
  Data :: iolist(),
  Reason :: term().
%% ====================================================================
encode(EncodingModule, Record) when is_list(EncodingModule) ->
  try
    case lists:suffix("_pb", EncodingModule) of
      true -> encode(list_to_existing_atom(EncodingModule), Record);
      _ -> encode(list_to_existing_atom(EncodingModule ++ "_pb"), Record)
    end
  catch
    _:_ -> {error, unsupported_encoder}
  end;

encode(EncodingModule, Record) when is_atom(EncodingModule) ->
  try
    RecordName = element(1, Record),
    EncodingFunction = list_to_existing_atom("encode_" ++ atom_to_list(RecordName)),
    {ok, EncodingModule:EncodingFunction(Record)}
  catch
    _:undef -> {error, unsupported_encoder_or_record};
    _:_ -> {error, invalid_record}
  end;

encode(_, _) ->
  {error, unsupported_encoder}.


%% decode/3
%% ====================================================================
%% @doc Decodes data using decoding module and function.
%% @end
-spec decode(DecodingModule, DecodingFunction, Data) -> {ok, Record} | {error, Reason} when
  DecodingModule :: string() | atom(),
  DecodingFunction :: string() | atom(),
  Data :: iolist(),
  Record :: tuple(),
  Reason :: term().
%% ====================================================================
decode(DecodingModule, DecodingFunction, Data) when is_list(DecodingModule) ->
  try
    case lists:suffix("_pb", DecodingModule) of
      true -> decode(list_to_existing_atom(DecodingModule), DecodingFunction, Data);
      _ -> decode(list_to_existing_atom(DecodingModule ++ "_pb"), DecodingFunction, Data)
    end
  catch
    _:_ -> {error, unsupported_decoder}
  end;

decode(DecodingModule, DecodingFunction, Data) when is_list(DecodingFunction) ->
  try
    case lists:prefix("decode_", DecodingFunction) of
      true -> decode(DecodingModule, list_to_existing_atom(DecodingFunction), Data);
      _ -> decode(DecodingModule, list_to_existing_atom("decode_" ++ DecodingFunction), Data)
    end
  catch
    _:_ -> {error, unsupported_decoder}
  end;

decode(DecodingModule, DecodingFunction, Data) when is_list(Data) ->
  try
    decode(DecodingModule, DecodingFunction, iolist_to_binary(Data))
  catch
    _:_ -> {error, invalid_data}
  end;

decode(DecodingModule, DecodingFunction, Data) when is_atom(DecodingModule), is_atom(DecodingFunction), is_binary(Data) ->
  try
    {ok, DecodingModule:DecodingFunction(Data)}
  catch
    _:undef -> {error, unsupported_decoder};
    _:_ -> {error, invalid_data}
  end;

decode(_, _, _) ->
  {error, unsupported_decoder_or_invalid_data}.