-module(util).
-export([undefined_default/2, ilog2/1, bitreflect8/1, bitreflect/1, bytereflect/1]).

undefined_default(undefined, Def) -> Def();
undefined_default(X, _) -> X.

ilog2(N) ->
	ilog2_(N, 0).
ilog2_(0, R) ->
	R;
ilog2_(N, R) ->
	ilog2_(N bsr 1, R+1).

bitreflect8(Bin) ->
	bitreflect8_(Bin, <<>>).
bitreflect8_(<<A:1, B:1, C:1, D:1, E:1, F:1, G:1, H:1, Rest/bytes>>, Done) ->
	bitreflect8_(Rest, <<Done/bytes, H:1, G:1, F:1, E:1, D:1, C:1, B:1, A:1>>);
bitreflect8_(<<>>, Done) ->
	Done.

bitreflect(Bin) ->
	bitreflect_(Bin, <<>>).
bitreflect_(<<A:1, B:1, C:1, D:1, E:1, F:1, G:1, H:1, Rest/bytes>>, Done) ->
	bitreflect_(Rest, <<H:1, G:1, F:1, E:1, D:1, C:1, B:1, A:1, Done/bytes>>);
bitreflect_(<<>>, Done) ->
	Done.

bytereflect(Bin) ->
	bytereflect_(Bin, <<>>).
bytereflect_(<<B:8, Rest/bytes>>, Done) ->
	bytereflect_(Rest, <<B:8, Done/bytes>>);
bytereflect_(<<>>, Done) ->
	Done.
