
/*  set_new_patch_4_map_c.h */

#ifndef SET_NEW_PATCH_4_MAP_C_
#define SET_NEW_PATCH_4_MAP_C_

#include "calypso_param_c.h"
#include "projection_patch_to_map.h"


/* prototypes */

void projection_patch_4_map(double *xyz_patch, double *xyz_map);

int count_new_patch_at_phi180(double *xyz_tri);
int cut_new_patch_at_phi180(double *xyz_org, long ie_cut[9],
							long inod_src[4], double coef_cut[4]);

#endif
