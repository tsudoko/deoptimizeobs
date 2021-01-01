#include <stdio.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

void
MessageBoxS(HWND parent, LPCSTR title, UINT type, const char *fmt, ...)
{
	static char msgbuf[4096];
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(msgbuf, (sizeof msgbuf)-1, fmt, ap);
	va_end(ap);
	MessageBoxA(parent, msgbuf, title, type);
}

void
MessageBoxError(HWND parent, DWORD code, const char *s)
{
	static char msgbuf[4096];
	int slen = s == NULL ? 0 : snprintf(msgbuf, (sizeof msgbuf)-1, "%s: ", s);

	FormatMessageA(
		FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		code,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPSTR)(msgbuf + slen),
		(sizeof msgbuf) - slen,
		NULL
	);

	MessageBoxA(parent, msgbuf, NULL, MB_ICONERROR);
}

void
PrintError(DWORD code, const char *s)
{
	LPSTR msgbuf;

	FormatMessageA(
		FORMAT_MESSAGE_ALLOCATE_BUFFER |
			FORMAT_MESSAGE_FROM_SYSTEM |
			FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		code,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPSTR)&msgbuf,
		0,
		NULL
	);

	if(s != NULL && *s)
		fprintf(stderr, "%s: ", s);
	fprintf(stderr, "%s", msgbuf);
	LocalFree(msgbuf);
}

void
PrintLastError(const char *s)
{
	PrintError(GetLastError(), s);
}

HMODULE
TryGetModuleHandle(LPCSTR name)
{
	HMODULE m = NULL;
	int tries = 0;
	while((m = GetModuleHandleA(name)) == NULL) {
		fprintf(stderr, "GetModuleHandleA try %d failed\n", tries);
		if(tries > 10) {
			fprintf(stderr, "failed to get handle for %s after 10 tries\n", name);
			return NULL;
		}
		Sleep(1000);
		++tries;
	}
	return m;
}
