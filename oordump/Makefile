include config.mk

# rUGP appears to load plugins in lexicographic order, the "s_" prefix makes
# oordump load after riooor (important as we hook a riooor function)
RPO_NAME=s_oordump.rpo

CFLAGS=-Wall -gdwarf-2 $(INCLUDES)
OFILES=oordump.o util.o memranges_win32.o vtable_msvc.o vtable_mfc.o mfc_compat.o rugp_compat.o gui_res.o gui.o

.SUFFIXES: .c .o .rc
.rc.o:
	$(WINDRES) $< $@

$(RPO_NAME): $(OFILES)
	$(CC) -static-libgcc -shared -o $(RPO_NAME) $(OFILES) $(LIBS) $(LDFLAGS) -lpsapi -lole32
