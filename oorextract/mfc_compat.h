/* this header depends on rugp.h */

extern int archive_file_offset;

extern unsigned long long (__fastcall *cfseek)(struct CFile *, long long off, unsigned int whence);
extern unsigned long long (__fastcall *cfgetlen)(struct CFile *);
extern size_t (__fastcall *cfread)(struct CFile *, void *buf, size_t count);

_Bool setup_mfc_compat(void);
