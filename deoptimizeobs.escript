#!/usr/bin/env escript
%%! -pa erl

main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	Oor = oor:read_oor(Data),
	{ok, File} = file:open([Filename, ".ogg"], [write]),
	ok = oor:to_ogg(File, Oor).
