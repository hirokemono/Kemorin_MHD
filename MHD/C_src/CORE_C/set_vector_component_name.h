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
#include "calypso_direction_flags.h"
#include "skip_comment_c.h"


extern const char * const hd_x;
extern const char * const hd_y;
extern const char * const hd_z;

extern const char * const hd_r;
extern const char * const hd_t;
extern const char * const hd_p;

extern const char * const hd_s;

extern const char * const hd_magnitude;

extern const char * const hd_scalar;


extern const char * const xyz_vector_list[];
extern const char * const sph_vector_list[];
extern const char * const cyl_vector_list[];

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
