#!/usr/bin/env escript

main([]) ->
	io:format("gimme filenames~n");
main(Filenames) ->
	true = code:add_patha(filename:join([filename:dirname(escript:script_name()), "erl"])),
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
