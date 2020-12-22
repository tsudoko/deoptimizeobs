-module(vorbis_headers).
-export([dump_info/1, dump_comment/2]).

% this is all very oor-specific for now

dump_info({_, Channels, Rate, {Bs2, Bs1}, _})->
	<<1, "vorbis", 0:32, Channels:8, Rate:32/little, 0:32, 0:32, 0:32, Bs1:4, Bs2:4, 1>>.

dump_comment(Vendor, Fields) ->
	dump_comment_(<<3, "vorbis", (length(Vendor)):32/little, (unicode:characters_to_binary(Vendor))/bytes, (length(Fields)):32/little>>, Fields).
dump_comment_(Data, []) ->
	<<Data/bytes, 1>>;
dump_comment_(Data, [F|Fields]) ->
	dump_comment_(<<Data/bytes, (length(F)):32/little, (unicode:characters_to_binary(F))/bytes>>, Fields).
