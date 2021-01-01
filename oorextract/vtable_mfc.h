/* this header depends on memranges.h */

char *mfc_vtable_classname(void *);
void *find_mfc_vtable(struct memranges r, char *classname, size_t nclassname, size_t *lastr, uintptr_t lastaddr);
