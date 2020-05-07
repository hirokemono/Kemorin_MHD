/*
//  m_field_name_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifndef M_FIELD_NAME_FROM_F_
#define M_FIELD_NAME_FROM_F_

struct field_names_f{
	int len_f;
	
	int istack_base_fields[2];
	int istack_base_forces[2];
	int istack_rot_forces[2];
	int istack_div_forces[2];
	int istack_energy_fluxes[2];
	int istack_divergence_fields[2];
	int istack_gradient_fields[2];
	int istack_base_diffusions[2];
	int istack_base_diffusivities[2];
	int istack_field_products[2];
	int istack_work_4_explicit[2];
	int istack_check_fields[2];
	int istack_filtered_forces[2];
	
	int istack_rot_filtered_forces[2];
	int istack_div_filtered_forces[2];
	int istack_filtered_ene_fluxes[2];
	int istack_SGS_terms[2];
	
	int istack_diff_SGS_terms[2];
	int istack_SGS_energy_fluxes[2];
	int istack_SGS_model_coefs[2];
	int istack_filter_fields[2];
	
	int istack_div_filter_fields[2];
	int istack_grad_filter_fields[2];
	int istack_wide_filter_fields[2];
	
	int istack_double_filter_fields[2];
	int istack_difference_vector[2];
	int istack_diff_filter_vector[2];
	int istack_wide_SGS_terms[2];
	int istack_dynamic_SGS_work[2];
	int istack_force_w_SGS[2];
	int istack_true_SGS_terms[2];
	
	int ntot_fields;
	
	int *num_comp;
	char **field_name;
	char **field_math;
};

/*  prototype */

struct field_names_f * init_field_name_f();
void dealloc_field_name_f(struct field_names_f *fnames);

#endif

