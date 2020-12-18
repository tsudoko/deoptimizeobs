-module(oor_framing).
-export([is_bos/1, is_eos/1]).
-export([print_frame_header/1]).
-export([print_frame/1, read_frame/1]).

-include("oor_records.hrl").

% flags: ??? (unseen), ??? (unseen), ??? (unseen), ??? (used for something), bos, eos

is_bos(#frame_header{flags = <<_:4, Bos:1, _:1>>}) -> Bos.
is_eos(#frame_header{flags = <<_:5, Eos:1>>}) -> Eos.

print_frame_header(FH) ->
	io:format("headersize ~p flags ~p npkt ~p basepktsize ~p vlenbits ~p~n", [
		FH#frame_header.headersize,
		FH#frame_header.flags,
		FH#frame_header.npkt,
		FH#frame_header.basepktsize,
		FH#frame_header.vlenbits]).

read_frame_header(<<Flags0:4, Bos:1, Flags1:1, _:7, BasePktSizeSel:2, _/bits>>) when Bos =:= 1 ->
	Flags = <<Flags0:4, Bos:1, Flags1:1>>,
	#frame_header{
		vlenbits = 0,
		npkt = 1,
		basepktsize = if BasePktSizeSel == 3 -> 3; true -> 2 end,
		headersize = 6,
		flags = Flags};
read_frame_header(<<Flags:6, VlenBits:4, _:1, Npkt:8, BasePktSizeSel:2, Rest/bits>>) ->
	HeaderSize = case BasePktSizeSel of
		0 -> 21;
		1 -> 29;
		2 -> 32
		% 3 is undefined
	end,
	<<BasePktSize:(HeaderSize-21), _/bits>> = Rest,
	#frame_header{vlenbits=VlenBits, npkt=Npkt, basepktsize=BasePktSize, headersize=HeaderSize, flags= <<Flags:6>>}.

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

	io:format("~W~n", [NextFrame, 5]),
	% TODO: include actual packets?
	{{H, Pktlens, Body}, NextFrame}.
