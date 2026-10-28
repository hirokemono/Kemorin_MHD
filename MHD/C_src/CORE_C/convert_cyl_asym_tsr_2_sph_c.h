/***********************************************************************
 !!*  convert_cyl_asym_tsr_2_sph_c.h
 !!*
 !!*  Created by Hiroaki Matsui on Dec. 3, 2012
 !!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 !!
 !!   urut =         uzus
 !!   upur = (as) * ( z* upuz - s* usup)
 !!   utup = (as) * ( s* upuz + z* usup)
 !!
 !!   usup = (as) * (z* utup - s* upur)
 !!   uzus =         urut 
 !!   upuz = (as) * (s* utup + z* upur)
 ***********************************************************************/
#ifndef CONVERT_CYL_ASYM_TSR_2_SPH_C_
#define CONVERT_CYL_ASYM_TSR_2_SPH_C_

#include <math.h>

/* prototypes */
void sph_rt_asym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_rt);
void sph_pr_asym_tensor_by_cyl_c(int numnod, const double *theta, 
                                 const double *st_cyl, double *t_pr);
void sph_tp_asym_tensor_by_cyl_c(int numnod, const double *theta, 
                                 const double *st_cyl, double *t_tp);

void sph_sp_asym_tensor_by_sph_c(int numnod, const double *theta, 
                                 const double *st_sph, double *t_sp);
void sph_zs_asym_tensor_by_sph_c(int numnod, const double *st_sph, double *t_zs);
void sph_pz_asym_tensor_by_sph_c(int numnod, const double *theta, 
                                 const double *st_sph, double *t_pz);

#endif /* CONVERT_CYL_ASYM_TSR_2_SPH_C_ */
