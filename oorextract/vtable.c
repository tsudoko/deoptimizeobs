#include <windows.h>
#define PSAPI_VERSION 1
#include <psapi.h>

#define PAGE_READ_ANY (PAGE_READONLY | PAGE_READWRITE | PAGE_WRITECOPY)
#define PAGE_EXECUTE_ANY (PAGE_EXECUTE | PAGE_EXECUTE_READ | PAGE_EXECUTE_READWRITE | PAGE_EXECUTE_WRITECOPY)

enum MemPerms {
	MEM_R = 1 << 0,
	MEM_X = 1 << 1,
};

struct memrange {
	uintptr_t from;
	uintptr_t to;
	enum MemPerms perms;
};

struct rangelist {
	struct memrange *r;
	int n;
};

inline static _Bool
is_mem_readable(struct rangelist ranges, void *vaddr)
{
	uintptr_t addr = (uintptr_t)vaddr;
	for(int i = 0; i < ranges.n; ++i)
		if(ranges.r[i].perms & MEM_R && addr >= ranges.r[i].from && addr <= ranges.r[i].to)
			return 1;
	return 0;
}

struct rtti_objloc {
	size_t signature;
	size_t offset;
	size_t coffset;
	struct {
		void *vtable;
		void *spare;
		char classname[];
	} *typedesc;
	struct {
		size_t signature;
		size_t attrs;
		size_t nbases;
		void *bases[];
	} *classdesc;
};

struct complete_vtable {
	struct rtti_objloc *loc;
	void *vtable[];
};

inline static _Bool
is_vtable_candidate(struct rangelist rl, struct complete_vtable *v, size_t nclassname)
{
	if(!is_mem_readable(rl, v) || !is_mem_readable(rl, (void *)(((uintptr_t)v)+(sizeof *v))))
		return 0;
	struct rtti_objloc *l = v->loc;
	return is_mem_readable(rl, l) &&
		is_mem_readable(rl, (void *)(((uintptr_t)l)+(sizeof *l))) &&
		is_mem_readable(rl, l->typedesc) &&
		is_mem_readable(rl, (void *)(((uintptr_t)l->typedesc)+(sizeof *l->typedesc))) &&
		is_mem_readable(rl, l->typedesc->vtable) &&
		is_mem_readable(rl, l->typedesc->classname) &&
		is_mem_readable(rl, (void *)(((uintptr_t)l->typedesc->classname)+nclassname));
}

void *
vtable_for(HMODULE module, char *classname, size_t nclassname, int offset)
{
	MODULEINFO mod;
	MEMORY_BASIC_INFORMATION mem;
	uintptr_t pagebase = (uintptr_t)module;
	struct memrange ranges[32];
	int nranges = 0;

	if(GetModuleInformation(GetCurrentProcess(), module, &mod, sizeof mod) == 0)
		return NULL;

	uintptr_t moduleend = ((uintptr_t)module)+mod.SizeOfImage;

	for(nranges = 0; pagebase < moduleend; pagebase += mem.RegionSize, ++nranges) {
		if(nranges >= sizeof ranges) {
			SetLastError(ERROR_NOT_ENOUGH_MEMORY);
			return NULL;
		}

		if(VirtualQuery((void *)pagebase, &mem, sizeof mem) != sizeof mem)
			return NULL;

		/* ignore unreadable pages */
		if(mem.Protect & (PAGE_GUARD | PAGE_NOACCESS) || !(mem.Protect & (PAGE_READ_ANY | PAGE_EXECUTE_ANY)))
			continue;

		ranges[nranges].from = (uintptr_t)mem.BaseAddress;
		ranges[nranges].to = ranges[nranges].from + mem.RegionSize;
		ranges[nranges].perms = 0;
		if(mem.Protect & PAGE_EXECUTE_ANY)
			ranges[nranges].perms |= MEM_X;
		ranges[nranges].perms |= MEM_R;
	}

	for(int i = 0; i < nranges; ++i) {
		struct rangelist rl = {ranges, nranges};
		for(struct complete_vtable *v = (void *)ranges[i].from; v < (struct complete_vtable *)(void *)ranges[i].to; ++v) {
			if(!is_vtable_candidate(rl, v, nclassname))
				continue;
			if(offset >= 0 && v->loc->offset != offset)
				continue;

			if(memcmp(classname, v->loc->typedesc->classname, nclassname) == 0)
				return v->vtable;
		}
	}

	SetLastError(ERROR_NOT_FOUND);
	return NULL;
}
