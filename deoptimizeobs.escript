#!/usr/bin/env escript
-record(frame_header, {headersize, flags, npkt, basepktsize, vlenbits}).

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

parse_info({H, _, <<Ver:2, Channels:3, RateSel:2, Rest/bits>>}) ->
	case is_bos(H) of
		1 -> ok;
		0 -> error({badinfo, notbos})
	end,
	case Ver of
		0 -> ok;
		_ -> error({badinfo, {badver, Ver}})
	end,
	{Rate, Blocksizes, End} = if
		RateSel < 3 ->
			<<Bs1:4, Bs2:4, E:1>> = Rest,
			{11025 bsl RateSel, {Bs1, Bs2}, E};
		true ->
			<<RateSel2:8, Rest2/bits>> = Rest,
			<<Bs1:4, Bs2:4, E:1>> = Rest2,
			R = case RateSel2 of
				3 -> 32000;
				4 -> 48000;
				5 -> 96000
			end,
			{R, {Bs1, Bs2}, E}
	end,
	case End of
		1 -> ok;
		0 -> error({badinfo, endzero})
	end,
	io:format("info header: ~p~n", [{Ver, Channels, Rate, Blocksizes, End}]),
	{Ver, Channels, Rate, Blocksizes}.

read_oor(Data) ->
	read_oor(Data, []).
read_oor(<<>>, Frames) ->
	Rframes = lists:reverse(Frames),
	[Info|Rest] = Rframes,
	[Setup|Sound] = Rest,
	{{parse_info(Info), Info}, Setup, Sound};
read_oor(Data, Frames) ->
	{F, NextData} = read_frame(Data),
	print_frame(F),
	read_oor(NextData, [F|Frames]).

main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	read_oor(Data).
