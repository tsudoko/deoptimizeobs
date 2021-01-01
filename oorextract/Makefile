include config.mk

# rUGP appears to load plugins in lexicographic order, the "s_" prefix makes
# oorextract load after riooor (important as we hook a riooor function)
RPO_NAME=s_oorextract.rpo

CFLAGS=-Wall -gdwarf-2 $(INCLUDES)
OFILES=oorextract.o util.o memranges_win32.o vtable_msvc.o vtable_mfc.o

$(RPO_NAME): $(OFILES)
	$(CC) -static-libgcc -shared -o $(RPO_NAME) $(OFILES) $(LIBS) $(LDFLAGS) -lpsapi
