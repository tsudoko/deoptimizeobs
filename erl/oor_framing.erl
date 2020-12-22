-module(oor_framing).
-export([flaglist/1, is_bos/1, is_eos/1]).
-export([print_frame_header/1]).
-export([print_frame/1, read_frame/1]).

-include("oor_records.hrl").

is_bos(#frame_header{flags = <<_:4, Bos:1, _:1>>}) -> Bos.
is_eos(#frame_header{flags = <<_:5, Eos:1>>}) -> Eos.

flaglist(<<Rest:5/bits, 0:1>>) -> flaglist_([], Rest);
flaglist(<<Rest:5/bits, 1:1>>) -> flaglist_([eos], Rest).
flaglist_(L, <<Rest:4/bits, 0:1>>) -> flaglist_(L, Rest);
flaglist_(L, <<Rest:4/bits, 1:1>>) -> flaglist_([bos|L], Rest);
flaglist_(L, <<Rest:3/bits, 0:1>>) -> flaglist_(L, Rest);
flaglist_(L, <<Rest:3/bits, 1:1>>) -> flaglist_([partial|L], Rest);
flaglist_(L, <<_:2/bits, 0:1>>) -> L;
flaglist_(L, <<_:2/bits, 1:1>>) -> [continued|L].

print_frame_header(FH) ->
	io:format("headersize ~p flags ~p granulepos ~p npkt ~p basepktsize ~p vlenbits ~p~n", [
		FH#frame_header.headersize,
		FH#frame_header.flags,
		FH#frame_header.granulepos,
		FH#frame_header.npkt,
		FH#frame_header.basepktsize,
		FH#frame_header.vlenbits]).

read_frame_header(<<0:2, Rest/bits>>) ->
	read_frame_header_(#frame_header{headersize=0}, <<0:2, Rest/bits>>);
read_frame_header(<<Ext:2, Frest:4, _:2, GranulePos:64, _:6, Rest/bits>>) ->
	read_frame_header_(#frame_header{granulepos=GranulePos, headersize=72}, <<Ext:2, Frest:4, Rest/bits>>).
read_frame_header_(H, <<Ext:2, Flags0:2, Bos:1, Flags1:1, _:7, BasePktSizeSel:2, _/bits>>) when Bos =:= 1 ->
	Flags = <<Ext:2, Flags0:2, Bos:1, Flags1:1>>,
	H#frame_header{
		vlenbits = 0,
		npkt = 1,
		basepktsize =
			if BasePktSizeSel == 3 -> 3; true -> 2 end +
			if Ext > 0 -> 10; true -> 0 end,
		headersize = H#frame_header.headersize + 6,
		flags = Flags};
read_frame_header_(H, <<Ext:2, Flags0:4, VlenBits:4, _:1, Npkt:8, BasePktSizeSel:2, Rest/bits>>) ->
	PktSizeSize = case BasePktSizeSel of
		0 -> 0;
		1 -> 8;
		2 -> 11
		% 3 is undefined
	end,
	<<BasePktSize:PktSizeSize, _/bits>> = Rest,
	H#frame_header{vlenbits=VlenBits, npkt=Npkt, basepktsize=BasePktSize, headersize=H#frame_header.headersize+21+PktSizeSize, flags= <<Ext:2, Flags0:4>>}.

read_packet_lengths(H, Data) ->
	read_packet_lengths(H, Data, H#frame_header.npkt, []).
read_packet_lengths(_, Rest, 0, Lengths) ->
	{lists:reverse(Lengths), Rest};
read_packet_lengths(H, Data, Npkt, Lengths) ->
	Bits = H#frame_header.vlenbits,
	<<Len:Bits, Rest/bits>> = Data,
	read_packet_lengths(H, Rest, Npkt-1, [H#frame_header.basepktsize+Len | Lengths]).

print_frame({H, Pktlens, Body}) ->
	io:format("{~p,~w,~W}~n", [H, Pktlens, Body, 5]).

read_frame(Data) ->
	H = read_frame_header(Data),
	print_frame_header(H),

	HSize = H#frame_header.headersize,
	<<_:(HSize), Data_/bits>> = Data,
	{Pktlens, _} = read_packet_lengths(H, Data_),
	BodyLen = lists:sum(Pktlens),

	VlenTotalSize = H#frame_header.vlenbits*H#frame_header.npkt,
	% TODO: figure out where the 7 comes from, ideally avoid doing two
	%       different splits altogether if possible
	BodyOffset = ((HSize + 7 + VlenTotalSize) div 8) * 8,
	%<<_:(BodyOffset+BodyLen*8), NextFrame/bits>> = Data,
	<<_:BodyOffset, Body:(BodyLen*8)/bits, NextFrame/bits>> = Data,

	% body offset from original code: (vlenbits*npkt + 7 + headersize) / 8
	% body len: all packet sizes summed up (Skip here)

	{{H, Pktlens, Body}, NextFrame}.
