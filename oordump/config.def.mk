CROSS_PREFIX=
CC=$(CROSS_PREFIX)gcc
WINDRES=$(CROSS_PREFIX)windres
INCLUDES=-I./minhook/include
LIBS=./minhook/libMinHook.a
