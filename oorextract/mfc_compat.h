/* this header depends on rugp.h */

extern int archive_file_offset;

extern unsigned long long (__fastcall *cfseek)(struct CFile *, void *_, long long off, unsigned int whence);
extern unsigned long long (__fastcall *cfgetlen)(struct CFile *);
extern size_t (__fastcall *cfread)(struct CFile *, void *_, void *buf, size_t count);

void prepare_cfmethods(struct CFile *cf);
void setup_archive_file_offset(void);
