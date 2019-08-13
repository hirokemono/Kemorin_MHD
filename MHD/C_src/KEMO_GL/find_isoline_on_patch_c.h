
/* find_isoline_on_patch_c.h */

#ifndef FIND_ISOLINE_ON_PATCH_
#define FIND_ISOLINE_ON_PATCH_

#include "kemoviewer_param_c.h"
#include "cal_surface_center_normal_c.h"

/* prototypes */
int find_isoline_on_patch_c(double *x_line, 
							const double *xx_tri, const double *d_tri, const double v_line);
int find_isoribbon_on_patch_c(double *x_ribbon, const double width, 
							  const double *xx_tri, const double *d_tri, const double v_line);
#endif
