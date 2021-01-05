#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#include <windows.h>

#include <MinHook.h>

#include "util.h"
#include "memranges.h"
#include "vtable_msvc.h"
#include "vtable_mfc.h"
#include "rugp.h"
#include "mfc_compat.h"
#include "rugp_compat.h"
#include "gui.h"

#define BUFLEN 4096

wchar_t outdir[MAX_PATH] = {0};
_Bool gui_done = 0;

/* I'd use thiscall here but msvc doesn't let me */
void (__fastcall *original_oor_serialize)(struct COptimizedObs *, void *_, struct CPmArchive *);

void __fastcall
wrapped_oor_serialize(struct COptimizedObs *that, void *_, struct CPmArchive *pmarchive)
{
	if(!is_dumping_enabled())
		goto fin0;

	struct CFile *cf = cpagetarchive(pmarchive)->stuff[archive_file_offset];

	fprintf(stderr, "oor read %p %p\n", that, pmarchive);

	gui_setstatus("dumping");
	gui_disable();

	wchar_t fpath[MAX_PATH];
	wchar_t *outdirend = wcschr(outdir, L'\0');
	if(outdirend == NULL) {
		MessageBoxA(NULL, "Output path invalid.", NULL, MB_ICONERROR);
		disable_dumping();
		goto fin1;
	}
	size_t pathn = outdirend - outdir;
	if(pathn > sizeof fpath) {
		MessageBoxA(NULL, "Output path too long.", NULL, MB_ICONERROR);
		disable_dumping();
		goto fin1;
	}

	memcpy(fpath, outdir, pathn * sizeof (wchar_t));
	wchar_t *fname = fpath + pathn;

	/* TODO: figure out if there's a stable-ish way to get offset in rio file */
	if(swprintf(fname, MAX_PATH-pathn-1-1, L"\\%10ld_%06x.oor", time(NULL), rand()) < 0) {
		MessageBoxA(NULL, "Failed to prepare final output path.", NULL, MB_ICONERROR);
		disable_dumping();
		goto fin1;
	}

	FILE *f = _wfopen(fpath, L"wb");
	if(f == NULL) {
		MessageBoxSW(NULL, NULL, MB_ICONERROR, L"Failed to open %ls: %hs", fpath, strerror(errno));
		disable_dumping();
		goto fin1;
	}

	char buf[BUFLEN];
	unsigned long long ncur, nleft = cfgetlen(cf);
	while(nleft) {
		ncur = nleft > BUFLEN ? BUFLEN : nleft;
		if(cfread(cf, buf, ncur) != ncur) {
			MessageBoxA(NULL, "Failed to read sound file", NULL, MB_ICONERROR);
			disable_dumping();
			goto fin2;
		}
		if(fwrite(buf, 1, ncur, f) != ncur) {
			MessageBoxSW(NULL, NULL, MB_ICONERROR, L"Failed to write %ls", fname);
			disable_dumping();
			goto fin2;
		}
		nleft -= ncur;
	}

fin2:
	cfseek(cf, 0, 0);
	if(fclose(f))
		MessageBoxSW(NULL, NULL, MB_ICONERROR, L"Failed to close %ls", fname);
fin1:
	gui_enable();
	gui_resetstatus();
fin0:
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
	return "oordump.rpo Version 0.00.02\n\tstuff\n\tPublic domain";
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

	if(!setup_mfc_compat())
		MessageBoxA(NULL, "Unknown MFC version, you may encounter crashes.", "Warning", MB_ICONWARNING);

	if(!setup_rugp_compat())
		MessageBoxA(NULL, "Failed to determine rUGP build date, you may encounter crashes.", "Warning", MB_ICONWARNING);

	if((mret = MH_Initialize()) != MH_OK) {
		MessageBoxSA(NULL, NULL, MB_ICONERROR, "Failed to initialize MinHook: %s", MH_StatusToString(mret));
		return NULL;
	}

	if((mret = MH_CreateHook(vtable->Serialize, (LPVOID)wrapped_oor_serialize, (LPVOID *)&original_oor_serialize)) != MH_OK) {
		MessageBoxSA(NULL, NULL, MB_ICONERROR, "Failed to create hook for COptimizedObs::Serialize: %s", MH_StatusToString(mret));
		goto err;
	}

	if((mret = MH_EnableHook(vtable->Serialize)) != MH_OK) {
		MessageBoxSA(NULL, NULL, MB_ICONERROR, "Failed to hook COptimizedObs::Serialize: %s", MH_StatusToString(mret));
		goto err;
	}

	srand(time(NULL));
	return NULL;

err:
	MH_Uninitialize();
	// TODO: send window destroy message
	return NULL;
}

BOOL WINAPI
DllMain(HINSTANCE dll, DWORD reason, LPVOID _)
{
	if(reason == DLL_PROCESS_ATTACH && !gui_done) {
		gui_done = gui_init(dll);
		return gui_done;
	}

	return TRUE;
}
