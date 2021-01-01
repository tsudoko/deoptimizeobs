#include <windows.h>

#include "rugp.h"

int archive_file_offset = -1;

unsigned long long (*cfseek)(struct CFile *, long long off, unsigned int whence);
unsigned long long (*cfgetlen)(struct CFile *);
size_t (*cfread)(struct CFile *, void *buf, size_t count);

unsigned long long
cfseek42(struct CFile *cf, long long off, unsigned int whence)
{
	return ((struct CFile42 *)cf)->vtable->Seek(cf, 0, off, whence);
}

unsigned long long
cfseek100(struct CFile *cf, long long off, unsigned int whence)
{
	return ((struct CFile100 *)cf)->vtable->Seek(cf, 0, off, whence);
}

unsigned long long
cfgetlen42(struct CFile *cf)
{
	return ((struct CFile42 *)cf)->vtable->GetLength(cf);
}

unsigned long long
cfgetlen100(struct CFile *cf)
{
	return ((struct CFile100 *)cf)->vtable->GetLength(cf);
}

size_t
cfread42(struct CFile *cf, void *buf, size_t count)
{
	return ((struct CFile42 *)cf)->vtable->Read(cf, 0, buf, count);
}

size_t
cfread100(struct CFile *cf, void *buf, size_t count)
{
	return ((struct CFile100 *)cf)->vtable->Read(cf, 0, buf, count);
}

int
check_mfc_version(void)
{
	static int v;
	if(v)
		return v;

	if(GetModuleHandleA("mfc100"))
		v = 100;
	else if(GetModuleHandleA("mfc42"))
		v = 42;
	else
		v = -1;

	return v;
}

_Bool
setup_mfc_compat(void)
{
	_Bool mfc_found = 0;
	/* public symbol numbers change between major versions anyway so it's not like
	   we'd gain anything from using CArchive::GetFile instead of getting the file
	   object directly */
	switch(check_mfc_version()) {
	case 42:
		archive_file_offset = 8;
		cfseek = cfseek42;
		cfgetlen = cfgetlen42;
		cfread = cfread42;
		return 1;
	case 100:
		mfc_found = 1;
		/* fallthrough */
	default:
		archive_file_offset = 9;
		cfseek = cfseek100;
		cfgetlen = cfgetlen100;
		cfread = cfread100;
	}
	return mfc_found;
}
