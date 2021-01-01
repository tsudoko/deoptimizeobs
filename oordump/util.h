void MessageBoxS(HWND parent, LPCSTR title, UINT type, const char *fmt, ...);
void MessageBoxError(HWND parent, DWORD code, const char *s);
void PrintError(DWORD code, const char *s);
void PrintLastError(const char *s);
HMODULE TryGetModuleHandle(LPCSTR name);
