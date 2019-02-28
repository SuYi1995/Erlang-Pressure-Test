%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 协议解析相关函数
%%% @end
%%% Created : 10. 十二月 2018 16:59
%%%-------------------------------------------------------------------
-module(lib_proto).
-author("suyang").

%% API
-export([pack/2,
    pack_test/2,
    unmask/2,
    read_bin/1,
    decode_data/1,
    build_frame/1]).

-include("common.hrl").
-include("conn.hrl").

%% @spec pack(Cmd, Data) -> {ok, Bin} | {error, Reason}
%% Cmd = integer()
%% Data = tuple()
%% Bin = binary()
%% Reason = bitstring()
%% @doc 协议打包
pack(Cmd, Data) ->
    case mapping:module(game_server, Cmd) of
        {ok, _Auth, _Caller, Proto, _ModName} ->
            NewData = Proto:encode_msg(Data),
            Len = byte_size(NewData),
            L = ?IF(Len =:= 0, 2, Len),
            Bin = <<L:?u32, Cmd:?u16, NewData/binary>>,
            BinData = build_frame(Bin),
            {ok, BinData};
        {error, _Code} ->
            ?ERROR_MSG("mapping err[~w]:~w", [Cmd, Data]),
            {error, pack_data_abnormal}
    end.

%% @spec pack(Cmd, Data) -> {ok, Bin} | {error, Reason}
%% Cmd = integer()
%% Data = tuple()
%% Bin = binary()
%% Reason = bitstring()
%% @doc 协议打包
pack_test(Cmd, Data) ->
    case mapping:module(game_server, Cmd) of
        {ok, _Auth, _Caller, Proto, _ModName} ->
            NewData = Proto:encode_msg(Data),
            Len = byte_size(NewData),
            Bin = <<Len:?u32, Cmd:?u16, NewData/binary>>,
            {ok, Bin};
        {error, _Code} ->
            ?ERROR_MSG("mapping err[~w]:~w", [Cmd, Data]),
            {error, pack_data_abnormal}
    end.

decode_data(<<>>) -> {incomplete};
decode_data(Data) -> decode_new_data(Data).

decode_new_data(Data) when is_list(Data) -> decode_new_data(list_to_binary(Data));
decode_new_data(Data) when is_binary(Data) ->
    <<_Fin:1, _Rsv:3, Opcode:4, Mask:1, Len:7, Rest/binary>> = Data,
    {Length, RestPacket} = case Len of
                               126 ->
                                   <<ALen:16, ARest/binary>> = Rest,
                                   {ALen, ARest};
                               127 ->
                                   <<ALen:64, ARest/binary>> = Rest,
                                   {ALen, ARest};
                               _ ->
                                   {Len, Rest}
                           end,
    {MaskN, PayLoad} = case Mask of
                           1 ->
                               <<MaskData:32, PLoad/binary>> = RestPacket,
                               {<<MaskData:32>>, PLoad};
                           _ ->
                               {<<0:32>>, RestPacket}
                       end,
    {UnmaskedData, Extra} =
        case Length > size(PayLoad) of
            true -> {<<>>, Data};
            _ ->
                <<APayLoad:Length/binary, EExtra/binary>> = PayLoad,
                {unmask(APayLoad, MaskN), EExtra}
        end,
    case Opcode of
        _ when Length > size(PayLoad) -> {incomplete};
        0 -> {continuation, binary_to_list(UnmaskedData), Extra};
        2 -> {binary, UnmaskedData, Extra};
        _ -> {unknown, Opcode, UnmaskedData, Extra}
    end.

unmask(PayLoad, 0) -> PayLoad;
unmask(PayLoad, Mask) -> unmask(PayLoad, Mask, <<>>).
unmask(<<Frame:32, Rest/binary>>, <<Mask:32>> = MaskB, Acc) ->
    unmask(Rest, MaskB, <<Acc/binary, (Frame bxor Mask):32>>);
unmask(<<Frame:24>>, <<Mask:24, _/binary>>, Acc) ->
    <<Acc/binary, (Frame bxor Mask):24>>;
unmask(<<Frame:16>>, <<Mask:16, _/binary>>, Acc) ->
    <<Acc/binary, (Frame bxor Mask):16>>;
unmask(<<Frame:8>>, <<Mask:8, _/binary>>, Acc) ->
    <<Acc/binary, (Frame bxor Mask):8>>;
unmask(<<>>, _, Acc) -> Acc.

%% @doc 协议数据解包
read_bin(Bin) ->
    case Bin of
        <<Len:?u32, Cmd:?u16, Rest/binary>> -> {Len, Cmd, Rest};
        _R -> {-1, -1, <<>>}
    end.

%%封装二进制版本的websocket数据
build_frame(Content) when erlang:is_binary(Content) ->
%%    DataLength = byte_size(Content),
    Len = iolist_size(Content),
    BinLen = payload_length_to_binary(Len),
    MaskingKeyBin = crypto:strong_rand_bytes(4),
    << MaskingKey:32 >> = MaskingKeyBin,
    build_frame_binary(BinLen, Content, MaskingKey, MaskingKeyBin).

%%封装文本的websocket数据
%%build_frame(Content) when erlang:is_list(Content) ->
%%    Bin = unicode:characters_to_binary(Content), %转换为二进制
%%    DataLength = byte_size(Bin),
%%    build_frame_text(DataLength, Bin).

build_frame_binary(DataLength, Bin, MaskingKey, MaskingKeyBin) ->
    Header = << 1:1, 0:3, 2:4, 1:1, DataLength/bits, MaskingKeyBin/bits >>,
    MaskedPayload = mask_payload(MaskingKey, Bin),
    << Header/binary, MaskedPayload/binary >>.
%%    <<1:1, 0:3, 2:4, 1:1, DataLength:7, MaskingKey/binary, Bin/binary>>;
%%build_frame_binary(DataLength, Bin, MaskingKey) when DataLength >= 125, DataLength =< 65535 ->
%%    <<1:1, 0:3, 2:4, 1:1, 126:7, DataLength:16, MaskingKey/binary, Bin/binary>>;
%%build_frame_binary(DataLength, Bin, MaskingKey) when DataLength > 65535 ->
%%    <<1:1, 0:3, 2:4, 1:1, 127:7, DataLength:64, MaskingKey/binary, Bin/binary>>.
%%build_frame_text(DataLength, Bin) when DataLength =< 125 ->
%%    <<1:1, 0:3, 1:4, 1:1, DataLength:7, Bin/binary>>;
%%build_frame_text(DataLength, Bin) when DataLength >= 125, DataLength =< 65535 ->
%%    <<1:1, 0:3, 1:4, 1:1, 126:7, DataLength:16, Bin/binary>>;
%%build_frame_text(DataLength, Bin) when DataLength > 65535 ->
%%    <<1:1, 0:3, 1:4, 1:1, 127:7, DataLength:64, Bin/binary>>.

mask_payload(MaskingKey, Payload) ->
    mask_payload(MaskingKey, Payload, <<>>).
mask_payload(_, <<>>, Acc) ->
    Acc;
mask_payload(MaskingKey, << D:32, Rest/bits >>, Acc) ->
    T = D bxor MaskingKey,
    mask_payload(MaskingKey, Rest, << Acc/binary, T:32 >>);
mask_payload(MaskingKey, << D:24 >>, Acc) ->
    << MaskingKeyPart:24, _:8 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:24 >>;
mask_payload(MaskingKey, << D:16 >>, Acc) ->
    << MaskingKeyPart:16, _:16 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:16 >>;
mask_payload(MaskingKey, << D:8 >>, Acc) ->
    << MaskingKeyPart:8, _:24 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:8 >>.

%% @doc Encode the payload length as binary in a variable number of bits.
%% See RFC Doc for more details
payload_length_to_binary(Len) when Len =<125 ->
    << Len:7 >>;
payload_length_to_binary(Len) when Len =< 16#ffff ->
    << 126:7, Len:16 >>;
payload_length_to_binary(Len) when Len =< 16#7fffffffffffffff ->
    << 127:7, Len:64 >>.