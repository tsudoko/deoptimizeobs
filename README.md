This is a set of tools and documentation for the oor (COptimizedObs,
OptimizedObsforR) sound container format used in the rUGP/AGES engine
developed by [âge][agewiki].

oordump
-------

Rough rUGP plugin for dumping sounds from a running game.

deoptimizeobs
-------------

Oor-to-ogg converter written in Erlang. To compile, run `erl -make`.
To convert a file, run `./deoptimizeobs.escript file.oor`.

[alterdec][]
------------

Modified version of alterdec with oor extraction support. Works with
a very limited range of titles.

[agewiki]: https://en.wikipedia.org/w/index.php?title=%C3%82ge&oldid=987317475
[alterdec]: https://github.com/tsudoko/chinesize/tree/oor/Rugp/alterdec
