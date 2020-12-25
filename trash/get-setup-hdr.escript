#!/usr/bin/escript

% this breaks on files with comment headers longer than 255 bytes
% or files with the setup header detached from the comment header
setup(<<"OggS", _:22/bytes, SL:8, CS:8, Sizes:(SL-1)/bytes, 3, "vorbis", _:(CS-1-6)/bytes, Rest/bytes>>) ->
	BS = lists:sum(binary_to_list(Sizes)),
	<<5, "vorbis", Setup:(BS-1-6)/bytes, _/bytes>> = Rest,
	Setup;
setup(<<"OggS", _:22/bytes, SL:8, Sizes:SL/bytes, Rest/bytes>>) ->
	BS = lists:sum(binary_to_list(Sizes)),
	<<_:BS/bytes, Next/bytes>> = Rest,
	setup(Next).

read(Dev) ->
	read(Dev, <<>>).
read(Dev, Data) ->
	case file:read(Dev, 4096) of
		{ok, NewData} -> read(Dev, <<Data/bytes, NewData/bytes>>);
		eof -> Data
	end.

main([]) ->
	io:setopts(standard_io, [binary]),
	ok = file:write(standard_io, setup(read(standard_io))).
