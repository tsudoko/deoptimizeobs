/* this header depends on memranges.h */

char *msvc_vtable_classname(void *);
void *find_msvc_vtable(struct memranges r, char *classname, size_t nclassname, int offset, size_t *lastr, uintptr_t lastaddr);
