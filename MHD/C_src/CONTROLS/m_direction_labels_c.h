/*
//  m_direction_labels_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#ifndef m_direction_labels_c_h_
#define m_direction_labels_c_h_

#include <stdio.h>
#include "calypso_param_c.h"

#define NUM_VECTOR_FLAG     11
#define NUM_SYM_TENSOR_FLAG 22

#define NUM_XYZ_FLAG      3
#define NTERM_PLANE       10

#define NUM_TOTAL_FORCE  9
#define NUM_BASIC_FORCE  4
#define NUM_DEFAULT_COEF_DEF  6
#define NUM_GRAVITY_DEF  3

#define NUM_BASIC_BC_TYPE_DEF  10
#define NUM_BOUNDARY_TYPE_DEF  25

#define NCHARA_FLAG 30

struct direction_flag_def{
	char flag_name[NCHARA_FLAG];
	char flag_math[KCHARA_C];
};

struct constant_flag_def{
    char flag_name[NCHARA_FLAG];
    char flag_math[KCHARA_C];
    double value;
};


extern const struct direction_flag_def vector_flags[NUM_VECTOR_FLAG];
extern const struct direction_flag_def sym_tensor_flags[NUM_SYM_TENSOR_FLAG];
extern const struct direction_flag_def xyz_vector_flags[NUM_XYZ_FLAG];
extern const struct direction_flag_def surface_equation_flags[NTERM_PLANE];

extern const struct direction_flag_def force_flag_def[NUM_TOTAL_FORCE];
extern const struct constant_flag_def default_coefs_def[NUM_DEFAULT_COEF_DEF];
extern const struct direction_flag_def gravity_type_def[NUM_GRAVITY_DEF];

extern const char boundary_type_def[NUM_BOUNDARY_TYPE_DEF][NCHARA_FLAG];

/* prototypes */

void get_vector_direction_flags(int index, char *name, char *math);
void get_surface_equation_flags(int index, char *name, char *math);

void get_force_flag(int index, char *name, char *math);
void get_basic_force_flag(int index, char *name, char *math);
void get_default_const_flag(int index, char *name, char *math, double *value);
void get_gravity_flag(int index, char *name, char *math);

#endif /* m_direction_labels_c_h_ */
