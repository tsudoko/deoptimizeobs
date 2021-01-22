#include <windows.h>

#include "rugp.h"

static int archive_file_offset = -1;

unsigned long long (__fastcall *cfseek)(struct CFile *, long long off, unsigned int whence);
unsigned long long (__fastcall *cfgetlen)(struct CFile *);
size_t (__fastcall *cfread)(struct CFile *, void *buf, size_t count);

struct CFile *
cagetfile(struct CArchive *ca)
{
	/* public symbol numbers change between major versions anyway so it's not like
	   we'd gain anything from using CArchive::GetFile instead of getting the file
	   object directly */
	return ((struct CFile **)ca)[archive_file_offset];
}

struct CFile100 {
	struct {
		void *stuff0[5];
		unsigned long long (__fastcall *GetPosition)(struct CFile *);
		void *stuff1[7];
		unsigned long long (__fastcall *Seek)(struct CFile *, void *_, long long off, unsigned int whence);
		void *stuff2;
		unsigned long long (__fastcall *GetLength)(struct CFile *);
		size_t (__fastcall *Read)(struct CFile *, void *_, void *buf, size_t count);
		// ...
	} *vtable;
	void *stuff[6];
};

struct CFile42 {
	struct {
		void *stuff0[5];
		DWORD (__fastcall *GetPosition)(struct CFile *);
		void *stuff1[6];
		LONG (__fastcall *Seek)(struct CFile *, void *_, LONG off, UINT whence);
		void *stuff2;
		DWORD (__fastcall *GetLength)(struct CFile *);
		UINT (__fastcall *Read)(struct CFile *, void *_, void *buf, UINT count);
		// ...
	} *vtable;
	// ...
};

unsigned long long __fastcall
cfseek42(struct CFile *cf, long long off, unsigned int whence)
{
	return ((struct CFile42 *)cf)->vtable->Seek(cf, 0, (LONG)off, whence);
}

unsigned long long __fastcall
cfseek100(struct CFile *cf, long long off, unsigned int whence)
{
	return ((struct CFile100 *)cf)->vtable->Seek(cf, 0, off, whence);
}

unsigned long long __fastcall
cfgetlen42(struct CFile *cf)
{
	return ((struct CFile42 *)cf)->vtable->GetLength(cf);
}

unsigned long long __fastcall
cfgetlen100(struct CFile *cf)
{
	return ((struct CFile100 *)cf)->vtable->GetLength(cf);
}

size_t __fastcall
cfread42(struct CFile *cf, void *buf, size_t count)
{
	return ((struct CFile42 *)cf)->vtable->Read(cf, 0, buf, (UINT)count);
}

size_t __fastcall
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

	if(GetModuleHandleA("mfc140") || GetModuleHandleA("mfc140u"))
		v = 140;
	else if(GetModuleHandleA("mfc100"))
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
	switch(check_mfc_version()) {
	case 42:
		archive_file_offset = 8;
		cfseek = cfseek42;
		cfgetlen = cfgetlen42;
		cfread = cfread42;
		return 1;
	case 100:
	case 140:
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
