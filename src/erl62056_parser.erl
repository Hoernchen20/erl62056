%%-------------------------------------------------------------------------------------------------
%% @doc erl62056 parser
%% @end
%%-------------------------------------------------------------------------------------------------
-module(erl62056_parser).

-include("erl62056.hrl").

-export([
    encode/1,
    decode/1
]).

%-define(EUNIT, ok).
-ifdef(EUNIT).
-export([
    calculate_bcc/1,
    check_bcc/2,
    decode_data_block/1,
    decode_data_set/2
]).
-endif.

%% Constants
-define(START_CHAR, 16#2f).
-define(END_CHAR, 16#21).
-define(CR, 16#0d).
-define(LF, 16#0a).
-define(ACK, 16#06).
-define(STX, 16#02).
-define(ETX, 16#03).
-define(EOT, 16#04).
-define(REQ_CMD, 16#3f).

%% Types

%%-------------------------------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------------------------------
-spec encode(Msg) -> Binary when
    Msg :: request_message() | ack_option_select_message(),
    Binary :: binary().
encode({request_message, DeviceAddress}) ->
    DeviceAddress1 = cut_binary(DeviceAddress, 32),
    <<?START_CHAR, ?REQ_CMD, DeviceAddress1/binary, ?END_CHAR, ?CR, ?LF>>;
encode({ack_option_select_message, ProtocolMode, Baudrate, ControlMode}) ->
    ProtocolModeByte = encode_protocol_mode(ProtocolMode),
    BaudrateByte = encode_baudrate(Baudrate),
    ControlModeByte = encode_control_mode(ControlMode),
    <<?ACK, ProtocolModeByte, BaudrateByte, ControlModeByte, ?CR, ?LF>>.

-spec decode(Binary) -> {ok, Message, Rest} | {error, Reason, Rest} when
    Binary :: binary(),
    Message :: identification_message() | data_message(),
    Rest :: binary(),
    Reason :: incomplete | bcc_check_failed.
decode(
    <<?START_CHAR, ManufacturerId:3/binary, BaudrateByte:1/binary, DeviceAddress:16/binary, ?CR,
        ?LF, Rest/binary>>
) ->
    Baudrate = decode_baudrate(BaudrateByte),
    {ok, {identification_message, ManufacturerId, Baudrate, DeviceAddress}, Rest};
decode(<<?STX, Rest/binary>>) when size(Rest) >= 6 ->
    decode_data_message(Rest);
decode(Binary) ->
    {error, incomplete, Binary}.

%%-------------------------------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------------------------------
% TODO add spec
cut_binary(Binary, Size) when byte_size(Binary) >= Size ->
    binary:part(Binary, 0, Size);
cut_binary(Binary, _Size) ->
    Binary.

% TODO add spec
check_bcc(Data, BCC) ->
    CalculatedBCC = calculate_bcc(Data),
    equal(CalculatedBCC, BCC).

% TODO add spec
calculate_bcc(Data) ->
    lists:foldl(fun(Byte, Acc) -> Byte bxor Acc end, 0, binary_to_list(Data)).

% TODO add spec
equal(A, A) ->
    ok;
equal(_A, _B) ->
    error.

-spec encode_protocol_mode(ProtocolMode) -> EncodedValue when
    ProtocolMode :: protocol_mode(),
    EncodedValue :: 0.
encode_protocol_mode(normal) ->
    0.

-spec encode_baudrate(Baudrate) -> EncodedValue when
    Baudrate :: baudrate(),
    EncodedValue :: 0..6.
encode_baudrate(300) ->
    0;
encode_baudrate(600) ->
    1;
encode_baudrate(1200) ->
    2;
encode_baudrate(2400) ->
    3;
encode_baudrate(4800) ->
    4;
encode_baudrate(9600) ->
    5;
encode_baudrate(19200) ->
    6.

-spec encode_control_mode(ControlMode) -> EncodedValue when
    ControlMode :: control_mode(),
    EncodedValue :: 0..1.
encode_control_mode(data_readout) ->
    0;
encode_control_mode(programming_mode) ->
    1.

-spec decode_baudrate(EncodedValue) -> Baudrate when
    EncodedValue :: <<_:8>>,
    Baudrate :: baudrate().
decode_baudrate(<<"0">>) ->
    300;
decode_baudrate(<<"1">>) ->
    600;
decode_baudrate(<<"2">>) ->
    1200;
decode_baudrate(<<"3">>) ->
    2400;
decode_baudrate(<<"4">>) ->
    4800;
decode_baudrate(<<"5">>) ->
    9600;
decode_baudrate(<<"6">>) ->
    19200.

-spec decode_data_block(DataBlock) -> DataSets when
    DataBlock :: binary(),
    DataSets :: [data_set()].
decode_data_block(DataBlock) ->
    DataLines = re:split(DataBlock, "\r\n", [trim]),
    decode_data_set(DataLines, []).

-spec decode_data_set(DataLines, DecodedDataSets) -> DecodedDataSets when
    DataLines :: string(),
    DecodedDataSets :: [data_set()].
decode_data_set([], DecodedDataSets) ->
    lists:reverse(DecodedDataSets);
decode_data_set([DataLine | Rest], DecodedDataSets) ->
    case re:split(DataLine, "[(*)]", [trim]) of
        [Identification, Value, Unit] ->
            NewDecodedDataSets = [{Identification, Value, Unit} | DecodedDataSets];
        [Identification, Value] ->
            NewDecodedDataSets = [{Identification, Value, undefined} | DecodedDataSets]
    end,
    decode_data_set(Rest, NewDecodedDataSets).

decode_data_message(Binary) ->
    case string:split(Binary, <<?END_CHAR, ?CR, ?LF, ?ETX>>) of
        [DataBlock, Rest] ->
            decode_data_message(DataBlock, Rest);
        [Rest] ->
            decode_prog_data_message(Rest)
    end.

decode_data_message(DataBlock, <<BCC, Rest/binary>>) ->
    case check_bcc(<<DataBlock/binary, ?END_CHAR, ?CR, ?LF, ?ETX>>, BCC) of
        ok ->
            DataSets = decode_data_block(DataBlock),
            {ok, {data_message, DataSets}, Rest};
        error ->
            {error, bcc_check_failed, Rest}
    end.

decode_prog_data_message(_Binary) ->
    {error, not_implemented}.
