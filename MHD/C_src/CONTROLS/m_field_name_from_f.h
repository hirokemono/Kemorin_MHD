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
#include "t_control_chara_IO.h"
#include "t_control_chara2_int_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara2_int_items_c.h"

#ifndef M_FIELD_NAME_FROM_F_
#define M_FIELD_NAME_FROM_F_

struct field_names_f{
    int len_f;
    
    int istack_field_groups[4];
    int ntot_field_groups;
    
    int ntot_fields;
    int *istack_fields;
    
    struct chara_clist      *fld_grp_list;
    struct chara2_int_clist *field_label;
};

struct component_flags_f{
    struct chara2_int_clist *scalar_components_flag;
    struct chara2_int_clist *vector_components_flag;
    struct chara2_int_clist *sym_tensor_components_flag;
    struct chara2_int_clist *asym_tensor_components_list;
};

/*  prototype */

struct field_names_f * init_field_name_f();
void dealloc_field_name_f(struct field_names_f *fld_list);
void check_field_name_f(struct field_names_f *fld_list);


struct component_flags_f * init_component_flags_f();
void dealloc_component_flags_f(struct component_flags_f *comp_flags);
void check_component_flags_f(struct component_flags_f *comp_flags);


#endif

