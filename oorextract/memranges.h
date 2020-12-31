enum MemPerms {
	MEM_R = 1 << 0,
	MEM_X = 1 << 1,
};

struct memrange {
	uintptr_t from;
	uintptr_t to;
	enum MemPerms perms;
};

struct memranges {
	struct memrange *r;
	size_t n;
};

_Bool is_mem_r(struct memranges, void *);
struct memranges *memscan(void *modulebase, struct memranges *, size_t);
