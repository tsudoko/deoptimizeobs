/* this header depends on windows.h */

_Bool disable_dumping(void);
_Bool is_dumping_enabled(void);
void gui_enable(void);
void gui_disable(void);
void gui_setstatus(char *status);
BOOL gui_init(HINSTANCE);