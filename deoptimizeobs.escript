#!/usr/bin/env escript
%%! -pa erl

main([]) ->
	io:format("gimme filenames~n");
main(Filenames) ->
	process(Filenames).

process([Filename|Rest]) ->
	io:format("processing ~s~n", [Filename]),
	{ok, Data} = file:read_file(Filename),
	Oor = oor:read_oor(Data),
	{ok, File} = file:open([Filename, ".ogg"], [write]),
	ok = oor:to_ogg(File, Oor),
	process(Rest);
process([]) ->
	ok.
