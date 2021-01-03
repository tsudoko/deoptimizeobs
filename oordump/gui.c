#include <stdio.h>

#include <windows.h>
#include <shlobj.h>

#include "util.h"
#include "gui_res.h"

extern char outdir[MAX_PATH]; /* oordump.c */
static HANDLE dll;
static HWND maindlg;
static BOOL dumping_enabled = FALSE;

static _Bool
gui_select_dir(HWND parent, char **result)
{
	BROWSEINFO bi = {
		.hwndOwner = parent,
		.ulFlags = BIF_RETURNONLYFSDIRS | BIF_USENEWUI,
	};
	PCIDLIST_ABSOLUTE idl;
	if((idl = SHBrowseForFolderA(&bi)) == NULL)
		return 0;

	if(!SHGetPathFromIDListA(idl, result)) {
		MessageBoxA(maindlg, "Failed to convert path.", NULL, MB_ICONERROR);
		return 0;
	}

	return 1;
}

void
disable_dumping(void)
{
	CheckDlgButton(maindlg, IDBTN_DUMP, BST_UNCHECKED);
}

_Bool
is_dumping_enabled(void)
{
	return dumping_enabled;
}

void
gui_setstatus(char *s)
{
	static HWND sw;
	if(!sw)
		sw = GetDlgItem(maindlg, IDST_STATUS);

	if(!SetWindowTextA(sw, s))
		MessageBoxError(maindlg, GetLastError(), "Failed to set status.");
}

void
gui_resetstatus(void)
{
	gui_setstatus(dumping_enabled ? "idle" : "off");
}

static void
set_outdir(HWND maindlg, char *outdir)
{
	HWND outpath_label = GetDlgItem(maindlg, IDST_OUTPATH);
	SetWindowTextA(outpath_label, outdir);
}

INT_PTR __stdcall
proc_main(HWND dlg, UINT msg, WPARAM wparam, LPARAM lparam)
{
	switch(msg) {
	case WM_COMMAND:
		switch(LOWORD(wparam)) {
		case IDBTN_DUMP:
			if(HIWORD(wparam) != BN_CLICKED)
				return FALSE;
			dumping_enabled = !dumping_enabled;
			gui_resetstatus();
			return TRUE;
		case IDBTN_OUTBROWSE:
			if(!gui_select_dir(dlg, &outdir))
				return TRUE;
			set_outdir(dlg, outdir);
			return TRUE;
		}
/*
		fprintf(stderr, "unhandled WM_COMMAND (wp=%#x, lp=%#lx)\n", wparam, lparam);
		break;
	default:
		fprintf(stderr, "unhandled msg %#x (hwnd=%#x, wp=%#x, lp=%#lx)\n", msg, dlg, wparam, lparam);
*/
	}

	return FALSE;
}

void
gui_disable(void)
{
	EnableWindow(maindlg, FALSE);
}

void
gui_enable(void)
{
	EnableWindow(maindlg, TRUE);
}

BOOL
gui_init(HMODULE module)
{
	dll = module;
	maindlg = CreateDialogA(dll, MAKEINTRESOURCE(IDD_MAIN), NULL, proc_main);
	if(!maindlg) {
		MessageBoxError(maindlg, GetLastError(), "Failed to create main window");
		return FALSE;
	}

	RECT dr, sr;
	if(!SystemParametersInfoA(SPI_GETWORKAREA, 0, &sr, 0))
		goto err_noncrit1;
	if(!GetWindowRect(maindlg, &dr))
		goto err_noncrit1;
	if(!SetWindowPos(maindlg, HWND_TOP, sr.right - (sr.right/8 + (dr.right-dr.left)), (sr.top + (dr.right-dr.left)/2), 0, 0, SWP_NOSIZE|SWP_SHOWWINDOW))
		goto err_noncrit1;

err_noncrit1:
	gui_resetstatus();
	if(SHGetSpecialFolderPathA(HWND_DESKTOP, outdir, CSIDL_DESKTOP, FALSE))
		set_outdir(maindlg, outdir);

	return TRUE;
}
