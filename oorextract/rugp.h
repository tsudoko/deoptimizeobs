/* Rough MFC/rUGP types */

struct CFile {
	struct {
		void *stuff0[5];
		size_t (__fastcall *GetPosition)(struct CFile *);
		void *stuff1[7];
		unsigned long long (__fastcall *Seek)(struct CFile *, void *_, long long off, unsigned int whence);
		void *stuff2;
		unsigned long long (__fastcall *GetLength)(struct CFile *);
		size_t (__fastcall *Read)(struct CFile *, void *_, void *buf, size_t count);
		// ...
	} *vtable;
	void *stuff[6];
};

struct CFile42 {
	struct {
		void *stuff0[5];
		size_t (__fastcall *GetPosition)(struct CFile *);
		void *stuff1[6];
		unsigned long long (__fastcall *Seek)(struct CFile *, void *_, long long off, unsigned int whence);
		void *stuff2;
		unsigned long long (__fastcall *GetLength)(struct CFile *);
		size_t (__fastcall *Read)(struct CFile *, void *_, void *buf, size_t count);
		// ...
	} *vtable;
	// ...
};

struct CArchive {
	/* this is obviously incorrect but we only care about
	   the file member; position of this member changes
	   between mfc versions */
	struct CFile *stuff[10];
};

struct CPmArchive {
	void *vtable;
	void *stuff;
	struct CArchive archive;
};

struct COceanNode {
	void *obj;
	struct COceanNode *next;
	char *name;
	struct COceanNode *parent;
	void **stuff0;
	struct CRuntimeClassEx *rtc;
	int stuff1;
	void *stuff2[4];
};

struct COptimizedObs {
	struct COptimizedObs_vtable1 {
		void *stuff[5];
		void (__fastcall *Serialize)(struct COptimizedObs *, void *, struct CPmArchive *);
		// ...
	} vtable1;
	struct COceanNode *node;
	void *stuff0;
	void *vtable0;
	void *stuff1[4];
};
