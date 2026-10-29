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
#include "skip_comment_c.h"


#define XYZ_COORDINATE    0
#define SPH_COORDINATE    1
#define CYL_COORDINATE    2

#define SPH_to_RR    0
#define SPH_to_RT    1
#define SPH_to_RP    2
#define SPH_to_TT    3
#define SPH_to_TP    4
#define SPH_to_PP    5
#define SPH_to_SS    6
#define SPH_to_SP    7
#define SPH_to_SZ    8
#define SPH_to_PZ    9
#define SPH_to_ZZ   10
#define SPH_to_XX   11
#define SPH_to_XY   12
#define SPH_to_XZ   13
#define SPH_to_YY   14
#define SPH_to_YZ   15
#define SPH_to_PR   16
#define SPH_to_ZS   17
#define SPH_to_ZX   18
#define SPH_to_MAG  19

#define CYL_to_SS    0
#define CYL_to_SP    1
#define CYL_to_SZ    2
#define CYL_to_PP    3
#define CYL_to_PZ    4
#define CYL_to_ZZ    5
#define CYL_to_RR    6
#define CYL_to_RT    7
#define CYL_to_RP    8
#define CYL_to_TT    9
#define CYL_to_TP   10
#define CYL_to_XX   11
#define CYL_to_XY   12
#define CYL_to_XZ   13
#define CYL_to_YY   14
#define CYL_to_YZ   15
#define CYL_to_ZS   16
#define CYL_to_PR   17
#define CYL_to_ZX   18
#define CYL_to_MAG  19

#define XYZ_to_XX    0
#define XYZ_to_XY    1
#define XYZ_to_XZ    2
#define XYZ_to_YY    3
#define XYZ_to_YZ    4
#define XYZ_to_ZZ    5
#define XYZ_to_RR    6
#define XYZ_to_RT    7
#define XYZ_to_RP    8
#define XYZ_to_TT    9
#define XYZ_to_TP   10
#define XYZ_to_PP   11
#define XYZ_to_SS   12
#define XYZ_to_SP   13
#define XYZ_to_SZ   14
#define XYZ_to_PZ   15
#define XYZ_to_ZX   16
#define XYZ_to_PR   17
#define XYZ_to_ZS   18
#define XYZ_to_MAG  19

const char * const hd_magitude = "magnitude";

const char * const hd_xx = "XX";
const char * const hd_xy = "XY";
const char * const hd_xz = "XZ";
const char * const hd_yy = "YY";
const char * const hd_yz = "YZ";
const char * const hd_zz = "ZZ";
const char * const hd_zx = "ZX";

const char * const hd_rr = "RR";
const char * const hd_rt = "R-Theta";
const char * const hd_rp = "R-Phi";
const char * const hd_tt = "Theta-Theta";
const char * const hd_tp = "Theta-Phi";
const char * const hd_pp = "Phi-Phi";
const char * const hd_pr = "Phi-R";

const char * const hd_ss = "SS";
const char * const hd_sp = "S-Phi";
const char * const hd_sz = "SZ";
/* const char * const hd_pp = "Phi-Phi"; */
const char * const hd_pz = "Phi-Z";
/* const char * const hd_zz = "ZZ"; */
const char * const hd_zs = "ZS";


const char * const xyz_tensor_list[] = {hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        hd_zz,
                                        hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_pp,
                                        hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pz,
                                        hd_magitude,
                                        NULL};

const char * const sph_tensor_list[] = {hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_pp,
                                        hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pz,
                                        hd_zz,
                                        hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        hd_magitude,
                                        NULL};

const char * const cyl_tensor_list[] = {hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pp,
                                        hd_pz,
                                        hd_zz,
                                        hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        hd_magitude,
                                        NULL};


const char * const xyz_asym_tsr_list[] = {hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_magitude,
                                          NULL};

const char * const sph_asym_tsr_list[] = {hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          hd_magitude,
                                          NULL};

const char * const cyl_asym_tsr_list[] = {hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          hd_magitude,
                                          NULL};


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
