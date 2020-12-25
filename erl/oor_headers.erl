-module(oor_headers).
-export([parse_info/1, parse_setup/1]).
-export([format_setup/1]).

-include("oor_records.hrl").

parse_info({_, _, <<Ver:2, _/bits>>}) when Ver > 1 ->
	error({badinfo, {badver, Ver}});
parse_info({H, _, <<Ver:2, Channels:3, RateSel:2, Rest/bits>>}) ->
	case oor_framing:is_bos(H) of
		1 -> ok;
		0 -> error({badinfo, notbos})
	end,
	{Rate, Rest2} = parse_info_rate(Ver, RateSel, Rest),
	{Unknowns, Rest3} = parse_info_unknowns(Ver, Rest2),
	<<Bs0:4, Bs1:4, 1:1, Padding/bits>> = Rest3,
	<<0:(bit_size(Padding))>> = Padding,
	{Ver, Channels, Rate, {Bs0, Bs1}, Unknowns}.
parse_info_rate(_, RateSel, Rest) when RateSel < 3 ->
	{11025 bsl RateSel, Rest};
parse_info_rate(Ver, _, <<RateSel:8, Rest/bits>>) ->
	Rate = case {Ver, RateSel} of
		{0, 3} -> 32000;
		{0, 4} -> 48000;
		{0, 5} -> 96000;

		{_, 4} -> 32000;
		{_, 5} -> 48000;
		{_, 6} -> 64000;
		{_, 7} -> 88200;
		{_, 8} -> 96000
	end,
	{Rate, Rest}.
parse_info_unknowns(0, Rest) ->
	{{undefined, undefined, undefined, undefined}, Rest};
parse_info_unknowns(_, <<Unk1:1, Unk2:1, Unk3:7, EndGranulePos:64, Rest/bits>>) ->
	{{Unk1, Unk2, Unk3, EndGranulePos}, Rest}.

parse_setup({_, _, <<_:2, 0:6, Rest/bytes>>}) ->
	Rest;
parse_setup({_, _, <<_:2, HeaderNum:6>>}) ->
	{ok, Header} = file:read_file(io_lib:format("data/setup~2..0w.hdr", [HeaderNum])),
	Header.

format_setup({_, _, <<_:2, HeaderNum:6, _/bytes>>}) ->
	io_lib:format("~w~s", [HeaderNum, if HeaderNum =:= 0 -> " (inline)"; true -> "" end]).
