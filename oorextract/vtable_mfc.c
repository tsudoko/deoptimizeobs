#include <string.h>

#include "memranges.h"
#include "vtable_mfc.h"

/* vtable_mfc.c - vtable enumerator for MFC runtime classes
 *
 * Mostly useful if your application doesn't have detailed enough RTTI
 * but still has MFC runtime class information and the vtable you want
 * to find belongs to an MFC class.
 *
 * Unlikely to work on anything that isn't 32-bit x86 as it parses
 * machine code to find runtime class getters.
 */

#define MOV_R32_IMM32 0xb8
#define RET 0xc3
#define GETTER_LENGTH 6

struct CRuntimeClass {
	char *name;
	size_t objsize;
	unsigned int schema;
	void *new;
	void *get_parent;
	void *next;
	void *init;
};

struct CRuntimeClassEx58 {
	struct CRuntimeClass rtc;
	void *stuff[3];
	struct {
		char *name;
		char *filename;
		char *ver;
	} *plugin_rtc;
	void *other_init;
	char *profile;
};

_Bool
is_rtc_candidate(struct memranges r, struct CRuntimeClass *rtc)
{
	return is_mem_r(r, rtc) &&
		is_mem_r(r, rtc+(sizeof *rtc)) &&
		is_mem_r(r, rtc->name) &&
		(rtc->new != NULL ? 1 : is_mem_x(r, rtc->new)) &&
		(rtc->get_parent != NULL ? 1 : is_mem_x(r, rtc->get_parent));
}

char *
mfc_vtable_classname(void *v)
{
	unsigned char *g = ((unsigned char **)v)[0];
	struct CRuntimeClass *rtc = (void *)(g[1] | (g[2] << 8) | (g[3] << 16) | (g[4] << 24));
	return rtc->name;
}

void *
find_mfc_vtable(struct memranges r, char *classname, size_t nclassname, size_t *lastr, uintptr_t lastaddr)
{
	size_t si;
	if(r.n == 0)
		return NULL;

	if(!lastaddr) {
		si = 0;
		lastaddr = r.r[si].from;
	} else {
		si = *lastr;
	}

	for(int i = si; i < r.n; ++i)
	for(void *v = (void *)lastaddr; v < (void *)r.r[i].to; ++v) {
		if(!is_mem_x(r, ((unsigned char **)v)[0]))
			continue;
		unsigned char *g = ((unsigned char **)v)[0];
		if(!is_mem_x(r, g) || !is_mem_x(r, g+GETTER_LENGTH))
			continue;
		if(g[0] != MOV_R32_IMM32 || g[5] != RET)
			continue;
		struct CRuntimeClass *rtc = (void *)(g[1] | (g[2] << 8) | (g[3] << 16) | (g[4] << 24));
		if(!is_rtc_candidate(r, rtc))
			continue;

		if(lastr != NULL)
			*lastr = i;

		if(classname == NULL)
			return v;
		if(!is_mem_r(r, rtc->name+nclassname))
			continue;
		if(memcmp(rtc->name, classname, nclassname) == 0)
			return v;
	}

	return NULL;
}
