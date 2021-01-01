#include <windows.h>
#define PSAPI_VERSION 1
#include <psapi.h>

#define PAGE_READ_ANY (PAGE_READONLY | PAGE_READWRITE | PAGE_WRITECOPY)
#define PAGE_EXECUTE_ANY (PAGE_EXECUTE | PAGE_EXECUTE_READ | PAGE_EXECUTE_READWRITE | PAGE_EXECUTE_WRITECOPY)

#include "memranges.h"

_Bool
is_mem_x(struct memranges r, void *vaddr)
{
	uintptr_t addr = (uintptr_t)vaddr;
	for(size_t i = 0; i < r.n; ++i)
		if(r.r[i].perms & MEM_X && addr >= r.r[i].from && addr <= r.r[i].to)
			return 1;
	return 0;
}

_Bool
is_mem_r(struct memranges r, void *vaddr)
{
	uintptr_t addr = (uintptr_t)vaddr;
	for(size_t i = 0; i < r.n; ++i)
		if(r.r[i].perms & MEM_R && addr >= r.r[i].from && addr <= r.r[i].to)
			return 1;
	return 0;
}

struct memranges *
memscan(void *module, struct memranges *r, size_t rcap)
{
	MODULEINFO mod;
	MEMORY_BASIC_INFORMATION mem;

	r->n = 0;
	if(GetModuleInformation(GetCurrentProcess(), (HMODULE)module, &mod, sizeof mod) == 0)
		return NULL;

	uintptr_t pagebase = (uintptr_t)module,
	          moduleend = pagebase+mod.SizeOfImage;

	for(r->n = 0; pagebase < moduleend; pagebase += mem.RegionSize, ++r->n) {
		if(r->n >= rcap) {
			SetLastError(ERROR_NOT_ENOUGH_MEMORY);
			return NULL;
		}

		if(VirtualQuery((void *)pagebase, &mem, sizeof mem) != sizeof mem)
			return NULL;

		/* ignore unreadable pages */
		if(mem.Protect & (PAGE_GUARD | PAGE_NOACCESS) || !(mem.Protect & (PAGE_READ_ANY | PAGE_EXECUTE_ANY)))
			continue;

		enum MemPerms newperms = MEM_R;
		if(mem.Protect & PAGE_EXECUTE_ANY)
			newperms |= MEM_X;

		if(r->n > 0 && r->r[r->n-1].to == (uintptr_t)mem.BaseAddress && r->r[r->n-1].perms == newperms) {
			/* this page has the same permissions as the last page
			   and there are no gaps between them, can merge */
			r->r[r->n-1].to += mem.RegionSize;
			--r->n;
		} else {
			r->r[r->n].from = (uintptr_t)mem.BaseAddress;
			r->r[r->n].to = r->r[r->n].from + mem.RegionSize;
			r->r[r->n].perms = newperms;
		}
	}
	return r;
}
