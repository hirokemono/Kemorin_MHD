
/*  coastline_c.h */

#ifndef COASTLINE_C_
#define COASTLINE_C_

#ifdef __cplusplus
extern "C" {
#endif

/* prototypes */

int get_nedge_coastline(void);
void get_coastline
		(int iedge, double *tp_coast, double *lake);
#ifdef __cplusplus
}
#endif

#endif
