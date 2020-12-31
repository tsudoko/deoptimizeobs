#include <string.h>

#include "memranges.h"

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
is_vtable_candidate(struct memranges rl, struct complete_vtable *v, size_t nclassname)
{
	/* +(sizeof (void *)) because flexible array members have a size of 0,
	   we assume there's at least one element */
	if(!is_mem_r(rl, v) || !is_mem_r(rl, (void *)(((uintptr_t)v)+(sizeof *v)+(sizeof (void *)))))
		return 0;
	struct rtti_objloc *l = v->loc;
	return is_mem_x(rl, v->vtable[0]) &&
		is_mem_r(rl, l) &&
		is_mem_r(rl, (void *)(((uintptr_t)l)+(sizeof *l))) &&
		is_mem_r(rl, l->typedesc) &&
		is_mem_r(rl, (void *)(((uintptr_t)l->typedesc)+(sizeof *l->typedesc))) &&
		is_mem_r(rl, l->typedesc->vtable) &&
		is_mem_r(rl, l->typedesc->classname) &&
		is_mem_r(rl, (void *)(((uintptr_t)l->typedesc->classname)+nclassname));
}

void *
find_msvc_vtable(struct memranges r, char *classname, size_t nclassname, int offset)
{
	for(int i = 0; i < r.n; ++i) {
		for(struct complete_vtable *v = (void *)r.r[i].from; v < (struct complete_vtable *)(void *)r.r[i].to; ++v) {
			if(!is_vtable_candidate(r, v, nclassname))
				continue;
			if(offset >= 0 && v->loc->offset != offset)
				continue;

			if(memcmp(classname, v->loc->typedesc->classname, nclassname) == 0)
				return v->vtable;
		}
	}

	return NULL;
}
