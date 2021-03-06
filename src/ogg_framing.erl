-module(ogg_framing).
-export([chop_packet/1, dump_page/5]).

-import(util, [bitreflect8/1]).

booltobit(false) -> 0;
booltobit(true) -> 1.

oggcrc(Data) when not is_binary(Data) ->
	oggcrc(iolist_to_binary(Data));
oggcrc(Data) ->
	bitreflect8(<<(erlang:crc32(16#ffffffff, bitreflect8(Data)) bxor 16#ffffffff):32>>).

chop_packet(Packet) ->
	chop_packet(Packet, []).
chop_packet(<<Seg:255/bytes, Rest/bits>>, Chopped) ->
	chop_packet(Rest, [Seg|Chopped]);
chop_packet(Seg, Chopped) ->
	PaddedSize = byte_size(Seg) * 8,
	Size = bit_size(Seg),
	LastSeg = if
		PaddedSize > Size -> <<Seg/bits, 0:(PaddedSize-Size)>>;
		true -> Seg
	end,

	lists:reverse([LastSeg|Chopped]).

dump_page(Flags, GranulePos, SerialNum, PageCounter, Segments) ->
	<<BFlags>> = <<0:5,
		(booltobit(proplists:get_bool(eos, Flags))):1,
		(booltobit(proplists:get_bool(bos, Flags))):1,
		(booltobit(proplists:get_bool(continued, Flags))):1>>,
	BeforeCRC = <<"OggS", 0, BFlags:8, GranulePos:64/little, SerialNum:32/little, PageCounter:32/little>>,
	AfterCRC = [<<(length(Segments)):8>>, [byte_size(S) || S <- Segments], Segments],
	CRC = oggcrc([BeforeCRC, 0, 0, 0, 0, AfterCRC]),
	[BeforeCRC, CRC, AfterCRC].
