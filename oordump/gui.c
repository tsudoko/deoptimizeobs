#include <stdio.h>

#define WINNT         0x0601
#define NTDDI_VERSION 0x06010000
#include <windows.h>
#include <objbase.h>
#include <shlobj.h>

#include "util.h"
#include "gui_res.h"

extern wchar_t outdir[MAX_PATH]; /* oordump.c */
static HANDLE dll;
static HWND maindlg;
static HWND st_outdir, tt_outdir;

static _Bool
gui_select_dir(HWND parent, wchar_t **result)
{
	APTTYPEQUALIFIER atq;
	APTTYPE at;
	UINT bif_usebetterui;
	if(CoGetApartmentType(&at, &atq) != S_OK || at == APTTYPE_MTA)
		bif_usebetterui = BIF_EDITBOX;
	else
		bif_usebetterui = BIF_USENEWUI;

	BROWSEINFOW bi = {
		.hwndOwner = parent,
		.ulFlags = BIF_RETURNONLYFSDIRS | bif_usebetterui,
	};

	PCIDLIST_ABSOLUTE idl;
	if((idl = SHBrowseForFolderW(&bi)) == NULL)
		return 0;

	if(!SHGetPathFromIDListW(idl, result)) {
		MessageBoxA(maindlg, "Failed to convert path.", NULL, MB_ICONERROR);
		CoTaskMemFree(idl);
		return 0;
	}

	CoTaskMemFree(idl);
	return 1;
}

static HWND
tooltip_init(HMODULE module, HWND control, HWND parent, wchar_t *text)
{
	HWND tooltip = CreateWindowW(
		TOOLTIPS_CLASSW, NULL,
		WS_POPUP | TTS_NOPREFIX | TTS_ALWAYSTIP,
		CW_USEDEFAULT, CW_USEDEFAULT,
		CW_USEDEFAULT, CW_USEDEFAULT,
		parent, NULL,
		module, NULL
	);
	if(!tooltip)
		return NULL;

	TOOLINFOW info = {
		.hwnd = parent,
		.uFlags = TTF_IDISHWND | TTF_SUBCLASS,
		.uId = control,
		.lpszText = text,
		.cbSize = sizeof info,
	};
	SendMessageW(tooltip, TTM_ADDTOOLW, 0, (LPARAM)&info);

	return tooltip;
}

_Bool
is_dumping_enabled(void)
{
	return IsDlgButtonChecked(maindlg, IDBTN_DUMP) == BST_CHECKED;
}

_Bool
is_oneshot_enabled(void)
{
	return IsDlgButtonChecked(maindlg, IDBTN_ONESHOT) == BST_CHECKED;
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
	gui_setstatus(is_dumping_enabled() ? "idle" : "off");
}

void
disable_dumping(void)
{
	CheckDlgButton(maindlg, IDBTN_DUMP, BST_UNCHECKED);
	gui_resetstatus();
}

static void
set_outdir(HWND maindlg, wchar_t *outdir)
{
	TOOLINFOW info = {
		.hwnd = maindlg,
		.uFlags = TTF_IDISHWND | TTF_SUBCLASS,
		.uId = st_outdir,
		.lpszText = outdir,
		.cbSize = sizeof info,
	};
	SetWindowTextW(st_outdir, outdir);
	SendMessageW(tt_outdir, TTM_UPDATETIPTEXTW, 0, (LPARAM)&info);
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
	maindlg = CreateDialogW(dll, MAKEINTRESOURCEW(IDD_MAIN), NULL, proc_main);
	if(!maindlg) {
		MessageBoxError(maindlg, GetLastError(), "Failed to create main window");
		return FALSE;
	}

	RECT dr, sr;
	if(!SystemParametersInfoA(SPI_GETWORKAREA, 0, &sr, 0))
		goto err_noncrit1;
	if(!GetWindowRect(maindlg, &dr))
		goto err_noncrit1;
	if(!SetWindowPos(maindlg, HWND_TOP, sr.right - 8 - (dr.right-dr.left), (sr.top + (dr.right-dr.left)/2), 0, 0, SWP_NOSIZE|SWP_SHOWWINDOW))
		goto err_noncrit1;

err_noncrit1:

	st_outdir = GetDlgItem(maindlg, IDST_OUTPATH);
	tt_outdir = tooltip_init(dll, st_outdir, maindlg, outdir);
	gui_resetstatus();
	if(SHGetSpecialFolderPathW(HWND_DESKTOP, outdir, CSIDL_DESKTOP, FALSE))
		set_outdir(maindlg, outdir);

	return TRUE;
}
