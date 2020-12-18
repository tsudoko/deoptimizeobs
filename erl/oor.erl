-module(oor).
-export([read_oor/1]).

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
