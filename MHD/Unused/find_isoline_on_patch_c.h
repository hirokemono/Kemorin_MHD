
/* find_isoline_on_patch_c.h */

#ifndef FIND_ISOLINE_ON_PATCH_
#define FIND_ISOLINE_ON_PATCH_

#include <math.h>
#include "kemoviewer_param_c.h"
#include "cal_surface_center_normal_c.h"

/* prototypes */
int mark_isoline_on_patch_c(const double *d_tri, const double v_line);
int find_isoline_on_patch_c(double x_line[6], double dir_line[6], double norm_line[6], 
							  const double *xx_tri, const double *d_tri, const double v_line);
#endif
