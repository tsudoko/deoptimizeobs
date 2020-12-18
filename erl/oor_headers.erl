-module(oor_headers).
-export([parse_info/1, parse_setup/1]).

-include("oor_records.hrl").

parse_info({H, _, <<Ver:2, Channels:3, RateSel:2, Rest/bits>>}) ->
	case oor_framing:is_bos(H) of
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

parse_setup({H, _, <<_:2, HeaderNum:6, Rest/bytes>>}) ->
	if
		HeaderNum =:= 0 -> {Rest};
		HeaderNum > 5 -> error({badsetup, {badindex, HeaderNum}});
		true ->
			error({unimplemented, "predefined setup headers not implemented yet"})
	end.
