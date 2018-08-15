/*
//  m_direction_labels_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "m_direction_labels_c.h"

const struct direction_flag_def scalar_flags[NUM_SCALAR_FLAG] = {
			{"scalar", "$$  S  $$"},
};

const struct direction_flag_def vector_flags[NUM_VECTOR_FLAG] = {
			{"vector", "$$  (V_{x}, V_{y}, V_{z})  $$"},
			{"spherical_vector", "$$  (V_{r}, V_{\\theta}, V_{\\phi})  $$"},
			{"cylindrical_vector", "$$  (V_{s}, V_{\\phi}, V_{z})  $$"},
			{"magnitude", "$$  |V|  $$"},
			
			{"x", "$$  V_{x}  $$"},
			{"y", "$$  V_{y}  $$"},
			{"z", "$$  V_{z}  $$"},
			
			{"r", "$$  V_{r}  $$"},
			{"theta", "$$  V_{\\theta}  $$"},
			{"phi", "$$  V_{\\phi}  $$"},
			{"s", "$$  V_{s}  $$"},
};

const struct direction_flag_def sym_tensor_flags[NUM_SYM_TENSOR_FLAG] = {
			{"sym_tensor", "$$  (T_{xx}, T_{xy}, T_{xz}, T_{yy}, T_{yz}, T_{zz})  $$"},
			{"spherical_vector", "$$  (T_{rr}, T_{r \\theta}, T_{r \\phi}, T_{\\theta \\theta}, T_{\\theta \\phi}, T_{\\phi \\phi})  $$"},
			{"spherical_sym_tensor", "$$  (T_{ss}, T_{s \\phi}, T_{sz}, T_{\\phi \\phi}, T_{\\phi z}, T_{zz})  $$"},
			{"cylindrical_sym_tensor", "$$  || T ||  $$"},

			{"xx", "$$  T_{xx}  $$"},
			{"xy", "$$  T_{xy}  $$"},
			{"xz", "$$  T_{xz}  $$"},
			{"yy", "$$  T_{yy}  $$"},
			{"yz", "$$  T_{yz}  $$"},
			{"zz", "$$  T_{zz}  $$"},
		
			{"rr", "$$  T_{rr}  $$"},
			{"rt", "$$  T_{r \\theta}  $$"},
			{"rp", "$$  T_{\\phi}  $$"},
			{"tt", "$$  T_{\\theta \\theta}  $$"},
			{"tp", "$$  T_{\\theta \\phi}  $$"},
			{"pp", "$$  T_{\\phi \\phi}  $$"},
		
			{"ss", "$$  T_{ss}  $$"},
			{"sp", "$$  T_{s \\phi}  $$"},
			{"sz", "$$  T_{sz}  $$"},
			{"pp_cyl", "$$  T_{\\phi \\phi}  $$"},
			{"pz", "$$  T_{\\phi z}  $$"},
			{"zz_cyl", "$$  T_{zz}  $$"}
};

const struct direction_flag_def xyz_vector_flags[NUM_XYZ_FLAG] = {
			{"x", "$$  x  $$"},
			{"y", "$$  y  $$"},
			{"z", "$$  z  $$"}
};

const struct direction_flag_def surface_equation_flags[NTERM_PLANE] = {
			{"x^2", "$$  x^{2}  $$"},
			{"y^2", "$$  y^{2}  $$"},
			{"z^2", "$$  z^{2}  $$"},
			
			{"x", "$$  x  $$"},
			{"y", "$$  y  $$"},
			{"z", "$$  z  $$"},
			
			{"xy", "$$  xy  $$"},
			{"yz", "$$  yz  $$"},
			{"zx", "$$  zx  $$"},
			
			{"Const", "$$  C  $$"}
};

void get_scalar_flags(char *name, char *math){
	int j;
	
	for (j = 0; j < NUM_SCALAR_FLAG;j++) {
		name[j] = scalar_flags[0].flag_name[j];
	};
	for (j = 0; j < KCHARA_C;j++) {
		math[j] = scalar_flags[0].flag_math[j];
	};
    return;
};

void get_vector_flags(int index, char *name, char *math){
	int j;
	
	if(index < -1 || index >= NUM_VECTOR_FLAG) return;
	for (j = 0; j < NCHARA_FLAG;j++) {
		name[j] = vector_flags[index].flag_name[j];
	};
	for (j = 0; j < KCHARA_C;j++) {
		math[j] = vector_flags[index].flag_math[j];
	};
    return;
};

void get_sym_tensor_flags(int index, char *name, char *math){
	int j;
	
	if(index < -1 || index >= NUM_SYM_TENSOR_FLAG) return;
	for (j = 0; j < NCHARA_FLAG;j++) {
		name[j] = sym_tensor_flags[index].flag_name[j];
	};
	for (j = 0; j < KCHARA_C;j++) {
		math[j] = sym_tensor_flags[index].flag_math[j];
	};
    return;
};

void get_vector_direction_flags(int index, char *name, char *math){
	int j;
	
	if(index < -1 || index >= NUM_XYZ_FLAG) return;
	for (j = 0; j < NCHARA_FLAG;j++) {
		name[j] = xyz_vector_flags[index].flag_name[j];
	};
	for (j = 0; j < KCHARA_C;j++) {
		math[j] = xyz_vector_flags[index].flag_math[j];
	};
    return;
};

void get_surface_equation_flags(int index, char *name, char *math){
	int j;
	
	if(index < -1 || index >= NTERM_PLANE) return;
	for (j = 0; j < NCHARA_FLAG;j++) {
		name[j] = surface_equation_flags[index].flag_name[j];
	};
	for (j = 0; j < KCHARA_C;j++) {
		math[j] = surface_equation_flags[index].flag_math[j];
	};
    return;
};
