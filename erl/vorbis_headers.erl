-module(vorbis_headers).
-export([dump_info/1, dump_comment/2]).
-export([setup_mode_blocksizes/1]).
-export([audio_sample_count/2]).

-import(util, [bytereflect/1]).

% this is all very oor-specific for now

dump_info({_, Channels, Rate, {Bs0, Bs1}, _})->
	<<1, "vorbis", 0:32, Channels:8, Rate:32/little, 0:32, 0:32, 0:32, Bs1:4, Bs0:4, 1>>.

dump_comment(Vendor, Fields) ->
	dump_comment_(<<3, "vorbis", (length(Vendor)):32/little, (unicode:characters_to_binary(Vendor))/bytes, (length(Fields)):32/little>>, Fields).
dump_comment_(Data, []) ->
	<<Data/bytes, 1>>;
dump_comment_(Data, [F|Fields]) ->
	dump_comment_(<<Data/bytes, (length(F)):32/little, (unicode:characters_to_binary(F))/bytes>>, Fields).

setup_mode_blocksizes(RawSetup) ->
	% TODO: figure out a less inefficient way
	setup_mode_blocksizes_(bytereflect(RawSetup)).
% /2, used only for removing padding
setup_mode_blocksizes_(<<0:1, Rest/bits>>) ->
	setup_mode_blocksizes_(Rest);
setup_mode_blocksizes_(<<1:1, Rest/bits>>) ->
	setup_mode_blocksizes_(Rest, []).
% /3, used for actual parsing
setup_mode_blocksizes_(<<_:8, 0:16, 0:16, 0:1, Rest/bits>>, ModeSizes) ->
	setup_mode_blocksizes_(Rest, [bs0|ModeSizes]);
setup_mode_blocksizes_(<<_:8, 0:16, 0:16, 1:1, Rest/bits>>, ModeSizes) ->
	setup_mode_blocksizes_(Rest, [bs1|ModeSizes]);
setup_mode_blocksizes_(<<ModeCount:6, _/bits>>, ModeSizes) ->
	% crappy sanity check since what we're doing is pretty hacky
	ModeCount = length(ModeSizes)-1,
	ModeSizes.

audio_sample_count(_, <<>>) ->
	io:format("empty packet (no samples)~n"),
	0;
audio_sample_count({{Bs0, Bs1}, ModeBlocksizes, ModeBits}, <<FirstByte:1/bytes, _/bytes>>) ->
	<<_:(8-1-ModeBits), ModeNum:ModeBits, 0:1>> = FirstByte,
	case lists:nth(ModeNum+1, ModeBlocksizes) of
		bs0 -> Bs0;
		bs1 -> Bs1
	end.
