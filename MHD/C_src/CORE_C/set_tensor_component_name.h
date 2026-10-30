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
#include "Calypso_direction_flags.h"


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
