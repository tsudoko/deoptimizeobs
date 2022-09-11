oordump
=======

Rough rUGP plugin for dumping sounds from a running game. Should work on
most versions or rUGP with plugin support; report an issue if it doesn't.

The plugin works by scanning memory for the COptimizedObs vtable, wrapping
the Serialize method and dumping whatever's passed to it to the specified
directory.

Dependencies
------------

[MinHook](https://github.com/TsudaKageyu/minhook)

How to set up
-------------

Build the plugin, put in `[game directory]/Plugins`, run the game.

How to build (MinGW)
--------------------

Make sure you've built MinHook first, copy `config.def.mk` to `config.mk`,
adjust settings as needed, run `make`.

How to build (MSVC)
-------------------

┐(´～｀)┌
