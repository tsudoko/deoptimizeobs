/* Rough MFC/rUGP types */

struct CFile;

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
