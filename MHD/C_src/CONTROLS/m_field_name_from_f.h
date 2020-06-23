/*
//  m_field_name_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "t_control_label_from_f.h"

#ifndef M_FIELD_NAME_FROM_F_
#define M_FIELD_NAME_FROM_F_

struct field_names_f{
	int len_f;
	
	int num_field_groups[3];
	int istack_field_groups[4];
	int ntot_field_groups;
	
	int ntot_fields;
	int *num_fields;
	int *istack_fields;
	char **field_group_name;
	
	int *num_comp;
	char **field_name;
	char **field_math;
};

struct component_flags_f{
    struct flag_with_math_f *scalar_components_flag;
    struct flag_with_math_f *vector_components_flag;
    struct flag_with_math_f *sym_tensor_components_flag;
    struct flag_with_math_f *asym_tensor_components_flag;
};

/*  prototype */

struct field_names_f * init_field_name_f();
void dealloc_field_name_f(struct field_names_f *fld_list);
void check_field_name_f(struct field_names_f *fld_list);


struct flag_with_math_f * init_scalar_components_flag();
struct flag_with_math_f * init_vector_components_flag();
struct flag_with_math_f * init_sym_tensor_components_flag();
struct flag_with_math_f * init_asym_tensor_components_flag();

struct component_flags_f * init_component_flags_f();
void dealloc_component_flags_f(struct component_flags_f *comp_flags);
void check_component_flags_f(struct component_flags_f *comp_flags);


#endif

