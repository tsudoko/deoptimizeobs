-module(oor_framing).
-export([flaglist/1, is_bos/1, is_eos/1]).
-export([format_page_header/1, format_page_flags/1]).
-export([format_page/1, read_page/1]).

-include("oor_records.hrl").

is_bos(#page_header{flags = <<_:4, Bos:1, _:1>>}) -> Bos.
is_eos(#page_header{flags = <<_:5, Eos:1>>}) -> Eos.

flaglist(<<Rest:5/bits, 0:1>>) -> flaglist_([], Rest);
flaglist(<<Rest:5/bits, 1:1>>) -> flaglist_([eos], Rest).
flaglist_(L, <<Rest:4/bits, 0:1>>) -> flaglist_(L, Rest);
flaglist_(L, <<Rest:4/bits, 1:1>>) -> flaglist_([bos|L], Rest);
flaglist_(L, <<Rest:3/bits, 0:1>>) -> flaglist_(L, Rest);
flaglist_(L, <<Rest:3/bits, 1:1>>) -> flaglist_([partial|L], Rest);
flaglist_(L, <<Rest:2/bits, 0:1>>) -> flaglist_(L, Rest);
flaglist_(L, <<Rest:2/bits, 1:1>>) -> flaglist_([continued|L], Rest);
flaglist_(L, <<Ver:2>>) -> [{version, Ver}|L].

format_page_flags(<<Ver:2, Continued:1, Partial:1, Bos:1, Eos:1>>) ->
	io_lib:format("v~w ~c~c~c~c", [
		Ver,
		case Continued of 1 -> $c; _ -> $- end,
		case Partial   of 1 -> $p; _ -> $- end,
		case Bos       of 1 -> $b; _ -> $- end,
		case Eos       of 1 -> $e; _ -> $- end]).

format_page_header(FH) ->
	io_lib:format("headersize ~p ~s granulepos ~p npkt ~p basepktsize ~p vlenbits ~p", [
		FH#page_header.headersize,
		format_page_flags(FH#page_header.flags),
		FH#page_header.granulepos,
		FH#page_header.npkt,
		FH#page_header.basepktsize,
		FH#page_header.vlenbits]).

read_page_header(<<0:2, Rest/bits>>) ->
	read_page_header_(#page_header{headersize=0}, <<0:2, Rest/bits>>);
read_page_header(<<Ver:2, Flags0:4, _:2, GranulePos:64, _:6, Rest/bits>>) ->
	read_page_header_(#page_header{granulepos=GranulePos, headersize=72}, <<Ver:2, Flags0:4, Rest/bits>>).
read_page_header_(H, <<Ver:2, Flags0:2, Bos:1, Flags1:1, _:7, BasePktSizeSel:2, _/bits>>) when Bos =:= 1 ->
	Flags = <<Ver:2, Flags0:2, Bos:1, Flags1:1>>,
	H#page_header{
		vlenbits = 0,
		npkt = 1,
		basepktsize =
			if BasePktSizeSel == 3 -> 3; true -> 2 end +
			if Ver > 0 -> 10; true -> 0 end,
		headersize = H#page_header.headersize + 6,
		flags = Flags};
read_page_header_(H, <<Ver:2, Flags0:4, VlenBits:4, _:1, Npkt:8, BasePktSizeSel:2, Rest/bits>>) ->
	PktSizeSize = case BasePktSizeSel of
		0 -> 0;
		1 -> 8;
		2 -> 11
		% 3 is undefined
	end,
	<<BasePktSize:PktSizeSize, _/bits>> = Rest,
	H#page_header{vlenbits=VlenBits, npkt=Npkt, basepktsize=BasePktSize, headersize=H#page_header.headersize+21+PktSizeSize, flags= <<Ver:2, Flags0:4>>}.

read_packet_lengths(H, Data) ->
	read_packet_lengths(H, Data, H#page_header.npkt, []).
read_packet_lengths(_, Rest, 0, Lengths) ->
	{lists:reverse(Lengths), Rest};
read_packet_lengths(H, Data, Npkt, Lengths) ->
	Bits = H#page_header.vlenbits,
	<<Len:Bits, Rest/bits>> = Data,
	read_packet_lengths(H, Rest, Npkt-1, [H#page_header.basepktsize+Len | Lengths]).

format_page({H, Pktlens, Body}) ->
	io_lib:format("{~s,~w,~W}", [format_page_header(H), Pktlens, Body, 5]).

read_page(Data) ->
	H = read_page_header(Data),

	HSize = H#page_header.headersize,
	<<_:(HSize), Data_/bits>> = Data,
	{Pktlens, _} = read_packet_lengths(H, Data_),
	BodyLen = lists:sum(Pktlens),

	VlenTotalSize = H#page_header.vlenbits*H#page_header.npkt,
	% TODO: figure out where the 7 comes from, ideally avoid doing two
	%       different splits altogether if possible
	BodyOffset = ((HSize + 7 + VlenTotalSize) div 8) * 8,
	%<<_:(BodyOffset+BodyLen*8), NextPage/bits>> = Data,
	<<_:BodyOffset, Body:(BodyLen*8)/bits, NextPage/bits>> = Data,

	% body offset from original code: (vlenbits*npkt + 7 + headersize) / 8
	% body len: all packet sizes summed up (Skip here)

	{{H, Pktlens, Body}, NextPage}.
