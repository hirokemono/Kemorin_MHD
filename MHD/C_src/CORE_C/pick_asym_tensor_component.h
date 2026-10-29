/***********************************************************************
 !!  pick_asym_tensor_component.h
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#ifndef PICK_ASYM_TENSOR_COMPONENT_
#define PICK_ASYM_TENSOR_COMPONENT_

#include <math.h>
#include <stdio.h>

#include "calypso_param_c.h"
#include "set_tensor_component_name.h"
#include "convert_xyz_asym_tsr_2_sph_c.h"
#include "convert_sph_asym_tsr_2_xyz_c.h"
#include "convert_cyl_asym_tsr_2_sph_c.h"
#include "pick_sym_tensor_component.h"

/* prototypes */
void pick_tensor_comp_c(int icomp, int numnod, 
                        const double *tensor, double *component);

void pick_sph_sym_tensor_by_sph(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component);
void pick_sph_sym_tensor_by_cyl(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component);
void pick_sph_sym_tensor_by_xyz(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component);

void pick_asym_tensor_component(const int id_coord, const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component);

#endif /* PICK_ASYM_TENSOR_COMPONENT_ */
