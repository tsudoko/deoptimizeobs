#include <stdio.h>

#include <windows.h>
#include <tchar.h>

#include "util.h"

#define LIBNAME "Ages3ResT"

void *
load_plugin(TCHAR *libname)
{
	void *(*lib_PluginThisLibrary)(void);
	HMODULE lib = LoadLibrary(libname);
	if(lib == NULL) {
		MessageBoxError(NULL, GetLastError(), "LoadLibrary");
		return NULL;
	}
	fputs("LoadLibrary successful\n", stderr);

	FARPROC fp = GetProcAddress(lib, TEXT("PluginThisLibrary"));
	if(fp == NULL) {
		MessageBoxError(NULL, GetLastError(), "GetProcAddress(..., \"PluginThisLibrary\")");
		return NULL;
	}
	fputs("found PluginThisLibrary\n", stderr);

	lib_PluginThisLibrary = (void *(*)(void))fp;
	return lib_PluginThisLibrary();
}

void
load_rpo_files(void)
{
	HANDLE f;
	WIN32_FIND_DATA fd;

	/* TODO: make sure it's relative to executable name? */
	f = FindFirstFile(TEXT("Plugins\\*.rpo"), &fd);
	if(f == INVALID_HANDLE_VALUE)
		return;
	fputs("found rpo file\n", stderr);
	size_t plen = _tcslen(TEXT("Plugins\\"));
	TCHAR path[MAX_PATH], *pathfile = path + plen;
	_tcscpy(path, TEXT("Plugins\\"));
	_tcscpy(pathfile, fd.cFileName);
	/* TODO: do the same for other plugins too */
	{
		/* XXX: not sure if passing the same pointer to both is valid */
		DWORD n = GetFullPathName(path, (sizeof path)/(sizeof *path), path, NULL);
		if(n == 0) {
			MessageBoxError(NULL, GetLastError(), "GetFullPathName");
			return;
		} else if(n >= sizeof path) {
			MessageBoxSA(NULL, NULL, 0, "Plugin path too long");
			return;
		}
		_ftprintf(stderr, TEXT("%s\n"), path);
		load_plugin(path);
	}

	while(FindNextFile(f, &fd))
		load_plugin(fd.cFileName);
}

__declspec(dllexport) int
PluginThisLibrary_Ages3Res(void)
{
	/* TODO: call original func? */
	return 1;
}

__declspec(dllexport) void *
PluginThisLibrary(void)
{
	void *r = load_plugin(TEXT(LIBNAME) TEXT("_orig"));
	load_rpo_files();
	return r;
}
