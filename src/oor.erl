-module(oor).
-export([read_oor/2, to_ogg/2]).

-include("oor_records.hrl").
-import(util, [undefined_default/2, ilog2/1]).

read_oor(Data, SetupDataDir) ->
	read_oor_(SetupDataDir, Data, []).
read_oor_(SDataDir, <<>>, Pages) ->
	Rpages = lists:reverse(Pages),
	[Info|Rest] = Rpages,
	[Setup|Sound] = Rest,
	ParsedInfo = oor_headers:parse_info(Info),
	io:format("info header ~w~n", [ParsedInfo]),
	io:format("setup header ~s~n", [oor_headers:format_setup(Setup)]),
	{{ParsedInfo, Info}, {oor_headers:parse_setup(Setup, SDataDir), Setup}, Sound};
read_oor_(SDataDir, _, Pages=[{#page_header{flags= <<_:5, 1:1>>}, _, _}|_]) ->
	% ignore trailing garbage past the eos page
	read_oor_(SDataDir, <<>>, Pages);
read_oor_(SDataDir, Data, Pages) ->
	{F, NextData} = oor_framing:read_page(Data),
	io:format("~s~n", [oor_framing:format_page(F)]),
	read_oor_(SDataDir, NextData, [F|Pages]).

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

get_granulepos({#page_header{granulepos = undefined, flags = RawFlags}, Pktlens, Body}, {ModeInfo, PrevPos, PrevCount, PartialPkt}) ->
	Flags = oor_framing:flaglist(RawFlags),
	[FLen|Restlens] = Pktlens,
	{NewPos, NewCount, NewPartialPkt} = ogg_vorbis:granulepos(proplists:get_bool(partial, Flags), PrevPos, PrevCount, ModeInfo, [byte_size(PartialPkt)+FLen|Restlens], <<PartialPkt/bytes, Body/bytes>>),
	io:format("calculated granulepos: ~w~n", [NewPos]),
	{NewPos, {ModeInfo, NewPos, NewCount, NewPartialPkt}};
% this whole case is just to keep GranulePosState up to date in case the stream
% contains pages with granulepos and pages without granulepos (haven't actually
% encountered such a stream in practice so I'm not sure if there's a point in
% doing this)
get_granulepos({#page_header{granulepos = GranulePos, flags = RawFlags}, Pktlens, Body}, {ModeInfo, _, _, _}) ->
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
	SerialNum = rand:uniform(16#ffffffff),
	VI = ogg_framing:chop_packet(vorbis_headers:dump_info(Info)),
	VC = ogg_framing:chop_packet(vorbis_headers:dump_comment("deoptimizeobs-erl-20201224; original encoder unknown", [])),
	VS = ogg_framing:chop_packet(<<5, "vorbis", Setup/bytes>>),
	IFlags = oor_framing:flaglist(IH#page_header.flags),
	IGranulePos = undefined_default(IH#page_header.granulepos, fun() -> 0 end),
	SFlags = oor_framing:flaglist(SH#page_header.flags),
	SGranulePos = undefined_default(SH#page_header.granulepos, fun() -> 0 end),
	ok = file:write(Device, ogg_framing:dump_page(IFlags, IGranulePos, SerialNum, 0, VI)),
	ok = file:write(Device, ogg_framing:dump_page(SFlags, SGranulePos, SerialNum, 1, lists:flatten([VC, VS]))),
	{_, _, _, {Bs0, Bs1}, _} = Info, % TODO: consider converting to record
	ModeSizes = vorbis_headers:setup_mode_blocksizes(Setup),
	to_ogg_(Device, Sound, {{{1 bsl Bs0, 1 bsl Bs1}, ModeSizes, ilog2(length(ModeSizes))-1}, SGranulePos, none, <<>>}, {SerialNum, 2}).
to_ogg_(Device, [{H, Pktlens, Body}|Rest], GranulePosState, {SerialNum, PageNum}) ->
	Flags = oor_framing:flaglist(H#page_header.flags),
	{GranulePos, NewGranulePosState} = get_granulepos({H, Pktlens, Body}, GranulePosState),
	Seglens = segment_pktlens(Pktlens),
	ok = file:write(Device, ogg_framing:dump_page(Flags, GranulePos, SerialNum, PageNum, chop_binary(Seglens, Body))),
	to_ogg_(Device, Rest, NewGranulePosState, {SerialNum, PageNum+1});
to_ogg_(_, [], _, _) ->
	ok.
