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

/*  prototype */

struct field_names_f * init_field_name_f();
void dealloc_field_name_f(struct field_names_f *fld_list);
void check_field_name_f(struct field_names_f *fld_list);

#endif

