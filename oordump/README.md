oordump
=======

Rough rUGP plugin for dumping sound files from a running game. Should work
on most versions of rUGP with plugin support; report an issue if it doesn't.

The files are dumped as-is, without any conversion.

The plugin works by scanning memory for the COptimizedObs vtable, wrapping
the Serialize method and dumping whatever's passed to it to the specified
directory.

How to set up
-------------

Put the `s_oordump.rpo` file in `[game directory]/Plugins`, run the game.
Make sure to set the output path before dumping.

Usage
-----

The label in the lower left window corner indicates the current state.
There are three states:
 - `off`: dumping is turned off
 - `idle`: dumping is turned on but no files are being dumped right now
 - `dumping`: there is a file being dumped right now

You can turn dumping on/off by pressing the "Dump" button. Files are dumped
automatically as they're loaded by the engine - this means you need to enable
dumping before the file is played in-game. If the "One-shot" checkbox is
checked, dump mode is automatically turned off after dumping the file once.

Build dependencies
------------------

[MinHook](https://github.com/TsudaKageyu/minhook)

How to build (MinGW)
--------------------

Make sure you've built MinHook first, copy `config.def.mk` to `config.mk`,
adjust settings as needed, run `make`.

How to build (MSVC)
-------------------

┐(´～｀)┌
