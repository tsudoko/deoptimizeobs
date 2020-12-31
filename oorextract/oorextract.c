#include <stdio.h>
#include <string.h>

#include <windows.h>

#include <MinHook.h>

#include "util.h"
#include "vtable.h"
#include "rugp.h"

#define BUFLEN 4096

/* I'd use thiscall here but msvc doesn't let me */
void (__fastcall *original_oor_serialize)(struct COptimizedObs *, void *_, struct CPmArchive *);

void __fastcall
wrapped_oor_serialize(struct COptimizedObs *that, void *_, struct CPmArchive *pmarchive)
{
	char fname[] = "/tmp/hauu";
	struct CFile *cf = pmarchive->archive.file;
	fprintf(stderr, "oor read %p %p\n", that, pmarchive);

	/* TODO: figure out if there's a stable-ish way to get offset in rio file */

	FILE *f = fopen(fname, "wb");
	if(f == NULL) {
		MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to open %s: %s", fname, strerror(errno));
		goto err1;
	}

	char buf[BUFLEN];
	size_t ncur, nleft = cf->vtable->GetLength(cf);
	while(nleft) {
		ncur = nleft > BUFLEN ? BUFLEN : nleft;
		if(cf->vtable->Read(cf, _, buf, ncur) != ncur) {
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
	cf->vtable->Seek(cf, _, 0, 0);
	if(fclose(f))
		MessageBoxS(NULL, NULL, MB_ICONERROR, "Failed to close %s", fname);
err1:
	original_oor_serialize(that, _, pmarchive);
}

__declspec(dllexport) const char *
GetPluginString(void)
{
	return "oorextract.rpo Version 0.00.01\n\tstuff\n\tPublic domain";
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

	if((vtable = vtable_for(riooor_base, ".?AVCOptimizedObs@@", sizeof ".?AVCOptimizedObs@@", 0)) == NULL) {
		MessageBoxError(NULL, GetLastError(), "Failed to find COptimizedObs vtable");
		return NULL;
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
