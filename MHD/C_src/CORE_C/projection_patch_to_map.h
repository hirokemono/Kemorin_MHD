
/*  projection_patch_to_map.h */
#ifndef PROJECTION_PATCH_TO_MAP_
#define PROJECTION_PATCH_TO_MAP_

#include <math.h>
#include "calypso_param_c.h"
#include "coordinate_converter_c.h"

/* prototypes */

int latitude_longitude_on_map(double *xyz_patch, double *rtp_patch);
void projection_patch_to_map(double *xyz_patch, double *xy_map_patch);

	/* triangle patch data
	number of grid...three
	xyz_patch[3*i  ]: x,
	xyz_patch[3*i+1]: y, 
	xyz_patch[3*i+2]: z
	
	xy_map_patch[2*i]:  x on map,
	xy_map_patch[2*i+1]: y on map,  */

#endif
