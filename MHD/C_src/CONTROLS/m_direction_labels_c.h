/*
//  m_direction_labels_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#ifndef m_direction_labels_c_h_
#define m_direction_labels_c_h_

#include <stdio.h>
#include "kemosrc_param_c.h"

#define NUM_SCALAR_FLAG      1
#define NUM_VECTOR_FLAG     11
#define NUM_SYM_TENSOR_FLAG 22

#define NUM_XYZ_FLAG      3
#define NTERM_PLANE       10

#define NCHARA_FLAG 30

struct direction_flag_def{
	char flag_name[NCHARA_FLAG];
	char flag_math[KCHARA_C];
};


extern const struct direction_flag_def scalar_flags[NUM_SCALAR_FLAG];
extern const struct direction_flag_def vector_flags[NUM_VECTOR_FLAG];
extern const struct direction_flag_def sym_tensor_flags[NUM_SYM_TENSOR_FLAG];
extern const struct direction_flag_def xyz_vector_flags[NUM_XYZ_FLAG];
extern const struct direction_flag_def surface_equation_flags[NTERM_PLANE];

/* prototypes */

void get_scalar_flags(char *name, char *math);
void get_vector_flags(int index, char *name, char *math);
void get_sym_tensor_flags(int index, char *name, char *math);
void get_vector_direction_flags(int index, char *name, char *math);
void get_surface_equation_flags(int index, char *name, char *math);

#endif /* m_direction_labels_c_h_ */
