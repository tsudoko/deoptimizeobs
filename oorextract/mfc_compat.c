#include <windows.h>

#include "rugp.h"

int archive_file_offset = -1;

unsigned long long (__fastcall *cfseek)(struct CFile *, void *_, long long off, unsigned int whence);
unsigned long long (__fastcall *cfgetlen)(struct CFile *);
size_t (__fastcall *cfread)(struct CFile *, void *_, void *buf, size_t count);

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

/* this might not be exactly correct as different subclasses of CFile could
   have different vtables, it would be safer if cf____ accessed the vtable
   directly but it'd require more boilerplate */
void
prepare_cfmethods(struct CFile *cf)
{
	static _Bool prepared;
	if(prepared)
		return;

	switch(check_mfc_version()) {
	case 42:
		cfseek = ((struct CFile42 *)cf)->vtable->Seek;
		cfgetlen = ((struct CFile42 *)cf)->vtable->GetLength;
		cfread = ((struct CFile42 *)cf)->vtable->Read;
		break;
	case 100:
	default:
		cfseek = cf->vtable->Seek;
		cfgetlen = cf->vtable->GetLength;
		cfread = cf->vtable->Read;
	}

	prepared = 1;
}

void
setup_archive_file_offset(void)
{
	/* public symbol numbers change between major versions anyway so it's not like
	   we'd gain anything from using CArchive::GetFile instead of getting the file
	   object directly */
	switch(check_mfc_version()) {
	case 42:
		archive_file_offset = 8;
		break;
	case 100:
		archive_file_offset = 9;
		break;
	default:
		MessageBoxA(NULL, "Unknown MFC version, you may encounter crashes.\n", "Warning",  MB_ICONWARNING);
		archive_file_offset = 9;
	}
}
