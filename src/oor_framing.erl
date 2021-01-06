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
	io_lib:format("~s granulepos ~p npkt ~p basepktsize ~p vlenbits ~p", [
		format_page_flags(FH#page_header.flags),
		FH#page_header.granulepos,
		FH#page_header.npkt,
		FH#page_header.basepktsize,
		FH#page_header.vlenbits]).

read_page_header(<<0:2, Flags0:4, Rest/bits>>) ->
	read_page_header_(#page_header{flags= <<0:2, Flags0:4>>}, Rest);
read_page_header(<<Ver:2, Flags0:4, _:2, GranulePos:64, _:6, Rest/bits>>) ->
	read_page_header_(#page_header{flags= <<Ver:2, Flags0:4>>, granulepos=GranulePos}, Rest).
read_page_header_(H = #page_header{flags = <<Ver:2, _:2, Bos:1, _:1>>}, Rest) when Bos =:= 1 ->
	% info header, special case, size needs to be inferred from the packet contents
	<<_:7, RateSel:2, _/bits>> = Rest,
	{H#page_header{
		vlenbits = 0,
		npkt = 1,
		basepktsize =
			2 +
			% extended sample rate
			if RateSel >= 3 -> 1; true -> 0 end +
			% final granulepos, some unknowns
			if Ver > 0 -> 10; true -> 0 end}, Rest};
read_page_header_(H, <<VlenBits:4, _:1, Npkt:8, BasePktSizeSel:2, Stuff/bits>>) ->
	PktSizeSize = case BasePktSizeSel of
		0 -> 0;
		1 -> 8;
		2 -> 11
		% 3 is undefined
	end,
	<<BasePktSize:PktSizeSize, Rest/bits>> = Stuff,
	{H#page_header{vlenbits=VlenBits, npkt=Npkt, basepktsize=BasePktSize}, Rest}.

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
	{H, RawPktlens} = read_page_header(Data),
	{Pktlens, RawStuff} = read_packet_lengths(H, RawPktlens),
	BodyLen = lists:sum(Pktlens),
	<<0:(bit_size(RawStuff) rem 8), Body:BodyLen/bytes, NextPage/bytes>> = RawStuff,
	{{H, Pktlens, Body}, NextPage}.
