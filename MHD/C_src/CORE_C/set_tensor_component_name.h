/***********************************************************************
 !!  set_tensor_component_name.h
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#ifndef SET_TENSOR_COMPONENT_NAME_
#define SET_TENSOR_COMPONENT_NAME_

#include <stdio.h>
#include "calypso_direction_flags.h"
#include "skip_comment_c.h"


extern const char * const hd_xx;
extern const char * const hd_xy;
extern const char * const hd_xz;
extern const char * const hd_yy;
extern const char * const hd_yz;
extern const char * const hd_zz;
extern const char * const hd_zx;

extern const char * const hd_rr;
extern const char * const hd_rt;
extern const char * const hd_rp;
extern const char * const hd_tt;
extern const char * const hd_tp;
extern const char * const hd_pp;
extern const char * const hd_pr;

extern const char * const hd_ss;
extern const char * const hd_sp;
extern const char * const hd_sz;
/* extern const char * const hd_pp; */
extern const char * const hd_pz;
/* extern const char * const hd_zz; */
extern const char * const hd_zs;

extern const char * const xyz_tensor_list[];
extern const char * const sph_tensor_list[];
extern const char * const cyl_tensor_list[];

extern const char * const xyz_asym_tsr_list[];
extern const char * const sph_asym_tsr_list[];
extern const char * const cyl_asym_tsr_list[];


/* prototypes */
int find_comp_id_from_by_sph_tensor(const char *selected);
void set_component_name_by_sph_tensor(int icomp, char *comp_name);

int find_comp_id_from_by_cyl_tensor(const char *selected);
void set_component_name_by_cyl_tensor(int icomp, char *comp_name);

int find_comp_id_from_by_xyz_tensor(const char *selected);
void set_component_name_by_xyz_tensor(int icomp, char *comp_name);

int find_tensor_component_id(const int id_coord, const char *selected);
void set_tensor_component_name(const int id_coord, const int icomp, 
                                   char *comp_name);

#endif /* SET_TENSOR_COMPONENT_NAME_*/
