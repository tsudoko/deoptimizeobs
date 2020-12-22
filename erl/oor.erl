-module(oor).
-export([read_oor/1, to_ogg/2]).

-include("oor_records.hrl").

read_oor(Data) ->
	read_oor(Data, []).
read_oor(<<>>, Frames) ->
	Rframes = lists:reverse(Frames),
	[Info|Rest] = Rframes,
	[Setup|Sound] = Rest,
	{{oor_headers:parse_info(Info), Info}, {oor_headers:parse_setup(Setup), Setup}, Sound};
read_oor(Data, Frames) ->
	{F, NextData} = oor_framing:read_frame(Data),
	oor_framing:print_frame(F),
	read_oor(NextData, [F|Frames]).

segment_pktlens(Pktlens) ->
	segment_pktlens(Pktlens, []).
segment_pktlens([Len|Rest], Done) when Len > 255; Len =:= 255, Rest =/= [] ->
	segment_pktlens([Len-255|Rest], [255|Done]);
segment_pktlens([Len|Rest], Done) ->
	segment_pktlens(Rest, [Len|Done]);
segment_pktlens([], Done) ->
	lists:reverse(Done).

chop_binary(Lens, Bin) ->
	chop_binary(Lens, Bin, []).
chop_binary([L|Lens], Bin, Done) ->
	<<B:L/bytes, Rest/bytes>> = Bin,
	chop_binary(Lens, Rest, [B|Done]);
chop_binary([], <<>>, Done) ->
	lists:reverse(Done).

undefinedtozero(undefined) -> 0;
undefinedtozero(X) -> X.

to_ogg(Device, {{Info, {IH, _, _}}, {Setup, {SH, _, _}}, Sound}) ->
	VI = ogg_framing:chop_packet(vorbis_headers:dump_info(Info)),
	VC = ogg_framing:chop_packet(vorbis_headers:dump_comment("deoptimizeobs-erl-20201222; original encoder unknown", [])),
	VS = ogg_framing:chop_packet(<<5, "vorbis", Setup/bytes>>),
	IFlags = oor_framing:flaglist(IH#frame_header.flags),
	IGranulePos = undefinedtozero(IH#frame_header.granulepos),
	SFlags = oor_framing:flaglist(SH#frame_header.flags),
	SGranulePos = undefinedtozero(SH#frame_header.granulepos),
	ok = file:write(Device, ogg_framing:dump_page(IFlags, IGranulePos, 0, 0, VI)),
	ok = file:write(Device, ogg_framing:dump_page(SFlags, SGranulePos, 0, 1, lists:flatten([VC, VS]))),
	to_ogg_(Device, Sound, 2).
to_ogg_(Device, [{H, Pktlens, Body}|Rest], PageNum) ->
	Flags = oor_framing:flaglist(H#frame_header.flags),
	GranulePos = undefinedtozero(H#frame_header.granulepos),
	Seglens = segment_pktlens(Pktlens),
	ok = file:write(Device, ogg_framing:dump_page(Flags, GranulePos, 0, PageNum, chop_binary(Seglens, Body))),
	to_ogg_(Device, Rest, PageNum+1);
to_ogg_(_, [], _) ->
	ok.
