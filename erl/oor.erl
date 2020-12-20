-module(oor).
-export([read_oor/1, to_ogg/2]).

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

chop_binary(Lens, Bin) ->
	chop_binary(Lens, Bin, []).
chop_binary([L|Lens], Bin, Done) ->
	<<B:L/bytes, Rest/bytes>> = Bin,
	chop_binary(Lens, Rest, [B|Done]);
chop_binary([], <<>>, Done) ->
	lists:reverse(Done).

to_ogg(Device, {{Info, _}, {Setup, _}, Sound}) ->
	VI = ogg_framing:chop_packet(vorbis_headers:dump_info(Info)),
	VC = ogg_framing:chop_packet(vorbis_headers:dump_comment("deoptimizeobs-erl-20201220; original encoder unknown", [])),
	VS = ogg_framing:chop_packet(<<5, "vorbis", Setup/bytes, 1>>),
	ok = file:write(Device, ogg_framing:dump_page(bos, 0, 0, 0, VI)),
	ok = file:write(Device, ogg_framing:dump_page(none, 0, 0, 1, lists:flatten([VC, VS]))),
	to_ogg_(Device, Sound, 2).
to_ogg_(Device, [{_, Pktlens, Body}|Rest], PageNum) ->
	% TODO: continued
	Flags = case Rest of
		[] -> eos;
		_ -> none
	end,
	% TODO: handle packets with length≥255
	ok = file:write(Device, ogg_framing:dump_page(Flags, 0, 0, PageNum, chop_binary(Pktlens, Body))),
	to_ogg_(Device, Rest, PageNum+1);
to_ogg_(_, [], _) ->
	ok.
