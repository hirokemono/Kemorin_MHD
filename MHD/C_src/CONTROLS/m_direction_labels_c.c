/*
//  m_direction_labels_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "m_direction_labels_c.h"

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
			{"spherical_sym_tensor", "$$  (T_{rr}, T_{r \\theta}, T_{r \\phi}, T_{\\theta \\theta}, T_{\\theta \\phi}, T_{\\phi \\phi})  $$"},
			{"cylindrical_sym_tensor", "$$  (T_{ss}, T_{s \\phi}, T_{sz}, T_{\\phi \\phi}, T_{\\phi z}, T_{zz})  $$"},
			{"magnitude", "$$  || T ||  $$"},

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

const struct direction_flag_def force_flag_def[NUM_TOTAL_FORCE] = {
			{"Thermal_buoyancy", "$$  -\\alpha g T   $$"},
			{"Composite_buoyancy", "$$  -\\alpha g C  $$"},
			{"Coriolis", "$$  (- \\Omega \\times \bvec{u})  $$"},
			{"Lorentz", "$$  \\bvec{J} \\times \\bvec{B}  $$"},
			
			{"Thermal_buoyancy_ele", "$$  -\\alpha g T  $$"},
			{"Composite_buoyancy_ele", "$$  -\\alpha g C  $$"},
			
			{"Coriolis_node", "$$  (- \\Omega \\times \bvec{u})  $$"},
			{"Coriolis_imp", "$$  (- \\Omega \\times \bvec{u})  $$"},
			{"Coriolis_node_imp", "$$  (- \\Omega \\times \bvec{u})  $$"},
};

const struct constant_flag_def default_coefs_def[NUM_DEFAULT_COEF_DEF] = {
			{"New_number", "$$ ...  $$", 1.0},
			
			{"Zero", "$$  0.0  $$", 00.},
			{"One", "$$  1.0  $$", 1.0},
			{"Two", "$$  2.0  $$", 2.0},
			
			{"Radial_parameter", "$$  1 / r_{o}  $$", 1.0},
			{"Radial_35", "$$  13 / 20  $$", 0.65}
};

const struct direction_flag_def gravity_type_def[NUM_GRAVITY_DEF] = {
			{"radial", "$$  -g_{0} \\bvec{r}  $$"},
			{"constant_radial", "$$  -g_{0} \\hat{r}  $$"},
			{"constant", "$$  \\bvec{g}  $$"}
};

const char boundary_type_def[NUM_BOUNDARY_TYPE_DEF][NCHARA_FLAG]
= {
			"fixed",
			"fixed_file",
			"fixed_flux",
			"fixed_flux_file",
			"free_slip_sph",
			"rot_inner_core",
			"insulator",
			"sph_to_center",
			"pseudo_vacuum",
			"fix_at_center",
			
			"rot_x",
			"rot_y",
			"rot_z",
			
			"fixed_ctl",
			"fix_ctl_x",
			"fix_ctl_y",
			"fix_ctl_z",
			"fix_file_x",
			"fix_file_y",
			"fix_file_z",
			"sgs",
			"sgs_x",
			"sgs_y",
			"sgs_z",
			"non_slip_sph"
		};


static void get_flags(struct direction_flag_def flags, char *name, char *math){
	int j;
	
	for (j = 0; j < NCHARA_FLAG;j++) {
		name[j] = flags.flag_name[j];
	};
	for (j = 0; j < KCHARA_C;j++) {
		math[j] = flags.flag_math[j];
	};
    return;
};

static void get_constant_flags(struct constant_flag_def flags, 
                               char *name, char *math, double *value){
    int j;
    
    for (j = 0; j < NCHARA_FLAG;j++) {
        name[j] = flags.flag_name[j];
    };
    for (j = 0; j < KCHARA_C;j++) {
        math[j] = flags.flag_math[j];
    };
    *value = flags.value;
    return;
};

void get_vector_direction_flags(int index, char *name, char *math){
	if(index < -1 || index >= NUM_XYZ_FLAG) return;
	get_flags(xyz_vector_flags[index], name, math);
    return;
};

void get_surface_equation_flags(int index, char *name, char *math){
	if(index < -1 || index >= NTERM_PLANE) return;
	get_flags(surface_equation_flags[index], name, math);
    return;
};

void get_force_flag(int index, char *name, char *math){
	if(index < -1 || index >= NUM_TOTAL_FORCE) return;
	get_flags(force_flag_def[index], name, math);
    return;
};
void get_basic_force_flag(int index, char *name, char *math){
	if(index < -1 || index >= NUM_BASIC_FORCE) return;
	get_flags(force_flag_def[index], name, math);
    return;
};
void get_default_const_flag(int index, char *name, char *math, double *value){
	if(index < -1 || index >= NUM_DEFAULT_COEF_DEF) return;
	get_constant_flags(default_coefs_def[index], name, math, value);
    return;
};
void get_gravity_flag(int index, char *name, char *math){
	if(index < -1 || index >= NUM_GRAVITY_DEF) return;
	get_flags(gravity_type_def[index], name, math);
    return;
};

