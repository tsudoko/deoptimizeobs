#!/usr/bin/env escript
%%! -pa erl

main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	oor:read_oor(Data).
