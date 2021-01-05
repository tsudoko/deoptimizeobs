#include <windows.h>

#include "rugp.h"

#define TS_1990 631152000
#define TS_2005 1104537600

int pmarchive_archive_offset = 2;

struct CArchive *
cpagetarchive(struct CPmArchive *pmarchive)
{
	return (struct CArchive *)(((void **)pmarchive) + pmarchive_archive_offset);
}

static long long
module_timestamp(HMODULE base)
{
	unsigned char *m = (unsigned char *)base;
	if(m[0] != 'M' || m[1] != 'Z')
		return -1;

	unsigned char *l = m + 0x3c;
	ptrdiff_t pe_offset = l[0] | (l[1] << 8) | (l[2] << 16) | (l[3] << 24);
	unsigned char *p = m + pe_offset;
	if(p[0] != 'P' || p[1] != 'E' || p[2] != '\0' || p[3] != '\0')
		return -2;

	return p[8] | (p[9] << 8) | (p[10] << 16) | (p[11] <<24);
}

_Bool
setup_rugp_compat(void)
{
	HMODULE univui_base = GetModuleHandleA("UnivUI");
	if(univui_base == NULL && (univui_base = GetModuleHandleA(NULL)) == NULL)
		return 0;

	long long ts = module_timestamp(univui_base);
	if(ts < TS_1990) /* sanity check */
		return 0;
	else if(ts < TS_2005) /* rough estimate */
		pmarchive_archive_offset = 1;
	else
		pmarchive_archive_offset = 2;

	return 1;
}
