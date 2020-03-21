
/*  coordinate_converter_c.h */
#ifndef COORDINATE_CONVERTER_C_
#define COORDINATE_CONVERTER_C_

#include <math.h>

#include "calypso_param_c.h"
/*
    map projection using The Hammer-Aitoff equal-area projection
*/
/* prototypes */

void aitoff_c(int nnod, double *rtp, double *xy_map);
void position_2_sph_c(int nnod, double *xyz, double *rtp);

/*
	position data
	nnod: number of grid
	rtp[3*i]: r,
	rtp[3*i+1]: theta, 
	rtp[3*i+2]: phi
	
	projection data
	nnod: number of grid
	xy_map[2*i]:  x on map,
	xy_map[2*i+1]: y on map,

	position data
	nnod: number of grid
	xyz[3*i  ]: x,
	xyz[3*i+1]: y, 
	xyz[3*i+2]: z
	
	rtp[3*i]: r,
	rtp[3*i+1]: theta, 
	rtp[3*i+2]: phi
*/

void const_map_data_from_xyz(int nnod, double *xyz, double *rtp, double *xy_map);

void sph_vector_to_xyz_vect(double theta, double phi, double v_sph[3], double v_xyz[3]);
void cyl_vector_to_xyz_vect(double phi, double v_cyl[3], double v_xyz[3]);

#endif
