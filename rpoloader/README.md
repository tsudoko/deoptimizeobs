rpoloader
=========

Small reimplementation of the pre-7.0 rUGP plugin loading system for AGES 7.0.

How to use
----------

(This assumes the `Ages3ResT.dll` file is present in the original game
directory. The loader is unlikely to work otherwise.)

Go to the game directory, rename `Ages3ResT.dll` to `Ages3ResT_orig.dll`,
move `Ages3ResT.dll` from here to the game directory, create a folder named
`Plugins` and move your plugins there.

How to build (MinGW)
--------------------

```
i686-w64-mingw32-gcc -static-libgcc -shared -o Ages3ResT.dll util.c rpoloader.c
```
