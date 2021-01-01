/* this header depends on memranges.h */

void *find_msvc_vtable(struct memranges r, char *classname, size_t nclassname, int offset, size_t lastr, uintptr_t lastaddr);
