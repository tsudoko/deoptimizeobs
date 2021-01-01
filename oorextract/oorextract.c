#include <stdio.h>
#include <string.h>

#include <windows.h>

#include <MinHook.h>

#include "util.h"
#include "memranges.h"
#include "vtable.h"
#include "vtable_mfc.h"
#include "rugp.h"

#define BUFLEN 4096

int archive_file_offset = -1;

/* I'd use thiscall here but msvc doesn't let me */
void (__fastcall *original_oor_serialize)(struct COptimizedObs *, void *_, struct CPmArchive *);

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

void __fastcall
wrapped_oor_serialize(struct COptimizedObs *that, void *_, struct CPmArchive *pmarchive)
{
	char fname[] = "/tmp/hauu";
	struct CFile *cf = pmarchive->archive.stuff[archive_file_offset];
	prepare_cfmethods(cf);

	fprintf(stderr, "oor read %p %p\n", that, pmarchive);

	/* TODO: figure out if there's a stable-ish way to get offset in rio file */

	FILE *f = fopen(fname, "wb");
	if(f == NULL) {
		MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to open %s: %s", fname, strerror(errno));
		goto err1;
	}

	char buf[BUFLEN];
	size_t ncur, nleft = cfgetlen(cf);
	while(nleft) {
		ncur = nleft > BUFLEN ? BUFLEN : nleft;
		if(cfread(cf, _, buf, ncur) != ncur) {
			MessageBoxA(NULL, "Failed to read sound file", NULL, MB_ICONERROR);
			goto err2;
		}
		if(fwrite(buf, 1, ncur, f) != ncur) {
			MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to write %s", fname);
			goto err2;
		}
		nleft -= ncur;
	}

err2:
	cfseek(cf, _, 0, 0);
	if(fclose(f))
		MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to close %s", fname);
err1:
	original_oor_serialize(that, _, pmarchive);
}

struct COptimizedObs_vtable1 *
find_coptimizedobs_vtable(HMODULE riooor_base)
{
	struct COptimizedObs_vtable1 *ret = NULL;
	struct memrange ranges[16];
	struct memranges mr = {ranges, 0};

	if(memscan(riooor_base, &mr, 16) == NULL)
		return NULL;

	ret = find_msvc_vtable(mr, ".?AVCOptimizedObs@@", sizeof ".?AVCOptimizedObs@@", 0, NULL, 0);
	if(ret != NULL && is_mem_r(mr, ret+(sizeof *ret)))
		return ret;

	ret = find_mfc_vtable(mr, "COptimizedObs", sizeof "COptimizedObs", NULL, 0);
	if(ret != NULL && is_mem_r(mr, ret+(sizeof *ret)))
		return ret;

	SetLastError(ERROR_NOT_FOUND);
	return NULL;
}

__declspec(dllexport) const char *
GetPluginString(void)
{
	return "oorextract.rpo Version 0.00.02\n\tstuff\n\tPublic domain";
}

__declspec(dllexport) void *
PluginThisLibrary(void)
{
	MH_STATUS mret;
	HMODULE riooor_base;
	struct COptimizedObs_vtable1 *vtable;

	if((riooor_base = GetModuleHandle("riooor.rpo")) == NULL) {
		MessageBoxError(NULL, GetLastError(), "Failed to get riooor.rpo module handle");
		return NULL;
	}

	if((vtable = find_coptimizedobs_vtable(riooor_base)) == NULL) {
		MessageBoxError(NULL, GetLastError(), "Failed to find COptimizedObs vtable");
		return NULL;
	}

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

	if((mret = MH_Initialize()) != MH_OK) {
		MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to initialize MinHook: %s", MH_StatusToString(mret));
		return NULL;
	}

	if((mret = MH_CreateHook(vtable->Serialize, (LPVOID)wrapped_oor_serialize, (LPVOID *)&original_oor_serialize)) != MH_OK) {
		MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to create hook for COptimizedObs::Serialize: %s", MH_StatusToString(mret));
		return NULL;
	}

	if((mret = MH_EnableHook(vtable->Serialize)) != MH_OK) {
		MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to hook COptimizedObs::Serialize: %s", MH_StatusToString(mret));
		return NULL;
	}
	return NULL;
}
