/***********************************************************************
 !!  set_vector_component_name.h
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#ifndef SET_VECTOR_COMPONENT_NAME_
#define SET_VECTOR_COMPONENT_NAME_

#include <stdio.h>
#include "skip_comment_c.h"


#define XYZ_COORDINATE    0
#define SPH_COORDINATE    1
#define CYL_COORDINATE    2

#define SPH_to_R    0
#define SPH_to_T    1
#define SPH_to_P    2
#define SPH_to_S    3
#define SPH_to_Z    4
#define SPH_to_X    5
#define SPH_to_Y    6
#define SPH_to_MAG  7

#define CYL_to_S    0
#define CYL_to_P    1
#define CYL_to_Z    2
#define CYL_to_R    3
#define CYL_to_T    4
#define CYL_to_X    5
#define CYL_to_Y    6
#define CYL_to_MAG  7

#define XYZ_to_X    0
#define XYZ_to_Y    1
#define XYZ_to_Z    2
#define XYZ_to_R    3
#define XYZ_to_T    4
#define XYZ_to_P    5
#define XYZ_to_S    6
#define XYZ_to_MAG  7

const char * const hd_x = "X";
const char * const hd_y = "Y";
const char * const hd_z = "Z";

const char * const hd_r = "R";
const char * const hd_t = "Theta";
const char * const hd_p = "Phi";

const char * const hd_s = "S";

const char * const hd_magitude = "magnitude";


/* prototypes */
int find_comp_id_from_by_sph_vector(const char *selected);
void set_component_name_by_sph_vector(int icomp, char *comp_name);

int find_comp_id_from_by_cyl_vector(const char *selected);
void set_component_name_by_cyl_vector(int icomp, char *comp_name);

int find_comp_id_from_by_xyz_vector(const char *selected);
void set_component_name_by_xyz_vector(int icomp, char *comp_name);

int find_vector_component_id(const int id_coord, const char *selected);
void set_vector_component_name(const int id_coord, const int icomp, 
                                   char *comp_name);

#endif /* SET_VECTOR_COMPONENT_NAME_*/
