-module(oor).
-export([read_oor/1, to_ogg/2]).

-include("oor_records.hrl").
-import(util, [undefined_default/2, ilog2/1]).

read_oor(Data) ->
	read_oor(Data, []).
read_oor(<<>>, Frames) ->
	Rframes = lists:reverse(Frames),
	[Info|Rest] = Rframes,
	[Setup|Sound] = Rest,
	ParsedInfo = oor_headers:parse_info(Info),
	io:format("info header ~w~n", [ParsedInfo]),
	io:format("setup header ~s~n", [oor_headers:format_setup(Setup)]),
	{{ParsedInfo, Info}, {oor_headers:parse_setup(Setup), Setup}, Sound};
read_oor(Data, Frames) ->
	{F, NextData} = oor_framing:read_frame(Data),
	io:format("~s~n", [oor_framing:format_frame(F)]),
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

get_granulepos({#frame_header{granulepos = undefined, flags = RawFlags}, Pktlens, Body}, {ModeInfo, PrevPos, PrevCount, PartialPkt}) ->
	Flags = oor_framing:flaglist(RawFlags),
	[FLen|Restlens] = Pktlens,
	{NewPos, NewCount, NewPartialPkt} = ogg_vorbis:granulepos(proplists:get_bool(partial, Flags), PrevPos, PrevCount, ModeInfo, [byte_size(PartialPkt)+FLen|Restlens], <<PartialPkt/bytes, Body/bytes>>),
	io:format("calculated granulepos: ~w~n", [NewPos]),
	{NewPos, {ModeInfo, NewPos, NewCount, NewPartialPkt}};
% this whole case is just to keep GranulePosState up to date in case the stream
% contains pages with granulepos and pages without granulepos (haven't actually
% encountered such a stream in practice so I'm not sure if there's a point in
% doing this)
get_granulepos({#frame_header{granulepos = GranulePos, flags = RawFlags}, Pktlens, Body}, {ModeInfo, _, _, _}) ->
	Flags = oor_framing:flaglist(RawFlags),
	[LastLen|Lens] = lists:reverse(Pktlens),
	{Count, PartialPkt} = case proplists:get_bool(partial, Flags) of
		true ->
			[LastFullLen|Lens_] = Lens,
			Skip = lists:sum(Lens_),
			<<_:Skip/bytes, LastPkt:LastFullLen/bytes, Partial:LastLen/bytes>> = Body,
			{vorbis_headers:audio_sample_count(ModeInfo, LastPkt), Partial};
		false ->
			Skip = lists:sum(Lens),
			<<_:Skip/bytes, LastPkt:LastLen/bytes>> = Body,
			{vorbis_headers:audio_sample_count(ModeInfo, LastPkt), <<>>}
	end,
	{GranulePos, {ModeInfo, GranulePos, Count, PartialPkt}}.

% TODO: (optionally?) process one page at a time (read from iodevice etc)
to_ogg(Device, {{Info, {IH, _, _}}, {Setup, {SH, _, _}}, Sound}) ->
	VI = ogg_framing:chop_packet(vorbis_headers:dump_info(Info)),
	VC = ogg_framing:chop_packet(vorbis_headers:dump_comment("deoptimizeobs-erl-20201223; original encoder unknown", [])),
	VS = ogg_framing:chop_packet(<<5, "vorbis", Setup/bytes>>),
	IFlags = oor_framing:flaglist(IH#frame_header.flags),
	IGranulePos = undefined_default(IH#frame_header.granulepos, fun() -> 0 end),
	SFlags = oor_framing:flaglist(SH#frame_header.flags),
	SGranulePos = undefined_default(SH#frame_header.granulepos, fun() -> 0 end),
	ok = file:write(Device, ogg_framing:dump_page(IFlags, IGranulePos, 0, 0, VI)),
	ok = file:write(Device, ogg_framing:dump_page(SFlags, SGranulePos, 0, 1, lists:flatten([VC, VS]))),
	{_, _, _, {Bs1, Bs2}, _} = Info, % TODO: consider converting to record
	ModeSizes = vorbis_headers:setup_mode_blocksizes(Setup),
	to_ogg_(Device, Sound, {{{1 bsl Bs1, 1 bsl Bs2}, ModeSizes, ilog2(length(ModeSizes))-1}, SGranulePos, none, <<>>}, 2).
to_ogg_(Device, [{H, Pktlens, Body}|Rest], GranulePosState, PageNum) ->
	Flags = oor_framing:flaglist(H#frame_header.flags),
	{GranulePos, NewGranulePosState} = get_granulepos({H, Pktlens, Body}, GranulePosState),
	Seglens = segment_pktlens(Pktlens),
	ok = file:write(Device, ogg_framing:dump_page(Flags, GranulePos, 0, PageNum, chop_binary(Seglens, Body))),
	to_ogg_(Device, Rest, NewGranulePosState, PageNum+1);
to_ogg_(_, [], _, _) ->
	ok.
