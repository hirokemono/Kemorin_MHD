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
#include "Calypso_direction_flags.h"
#include "skip_comment_c.h"


const char * const hd_x = "X";
const char * const hd_y = "Y";
const char * const hd_z = "Z";

const char * const hd_r = "R";
const char * const hd_t = "Theta";
const char * const hd_p = "Phi";

const char * const hd_s = "S";

const char * const hd_magitude = "magnitude";


const char * const hd_scalar =   "scalar";
const char * const xyz_vector_list[] = {hd_x,
                                        hd_y,
                                        hd_z,
                                        hd_r,
                                        hd_t,
                                        hd_p,
                                        hd_s,
                                        hd_magitude,
                                        NULL};

const char * const sph_vector_list[] = {hd_r,
                                        hd_t,
                                        hd_p,
                                        hd_s,
                                        hd_z,
                                        hd_x,
                                        hd_y,
                                        hd_magitude,
                                        NULL};

const char * const cyl_vector_list[] = {hd_s,
                                        hd_p,
                                        hd_z,
                                        hd_r,
                                        hd_t,
                                        hd_x,
                                        hd_y,
                                        hd_magitude,
                                        NULL};


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
