include config.mk

# rUGP appears to load plugins in lexicographic order, the "s_" prefix makes
# oorextract load after riooor (important as we hook a riooor function)
RPO_NAME=s_oorextract.rpo

CFLAGS=-Wall $(INCLUDES) -DSERIALIZE_ADDR=$(SERIALIZE_ADDR)
OFILES=oorextract.o util.o

$(RPO_NAME): $(OFILES)
	$(CC) $(LDFLAGS) -static-libgcc -shared -o $(RPO_NAME) $(OFILES) $(LIBS)
