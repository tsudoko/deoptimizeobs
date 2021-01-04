#!/usr/bin/env escript

main([]) ->
	io:format("gimme filenames~n");
main(Filenames) ->
	io:setopts([{encoding, unicode}]),
	true = code:add_patha(filename:join([filename:dirname(escript:script_name()), "src"])),
	process(Filenames, filename:join([filename:dirname(escript:script_name()), "data"])).

process([Filename|Rest], SetupDataDir) ->
	io:format("processing ~ts~n", [Filename]),
	{ok, Data} = file:read_file(Filename),
	Oor = oor:read_oor(Data, SetupDataDir),
	{ok, File} = file:open([filename:rootname(Filename, ".oor"), ".ogg"], [write]),
	ok = oor:to_ogg(File, Oor),
	process(Rest, SetupDataDir);
process([], _) ->
	ok.
