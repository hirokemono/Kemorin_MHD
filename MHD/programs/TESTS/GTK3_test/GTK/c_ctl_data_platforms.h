/*
//  c_ctl_data_platforms.h
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_int_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_control_label_from_f.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "t_ctl_array_chara_int_items_c.h"
#include "t_ctl_array_int_real_items_c.h"
#include "t_ctl_array_int2_items_c.h"
#

#ifndef C_CTL_DATA_PLATFORMS_GTK_H_
#define C_CTL_DATA_PLATFORMS_GTK_H_

extern void * c_plt_block_name(void *f_plt);
extern void * c_plt_iflag(void *f_plt);
extern void * c_plt_ndomain_ctl(void *f_plt);
extern void * c_plt_num_smp_ctl(void *f_plt);
extern void * c_plt_debug_flag_ctl(void *f_plt);
extern void * c_plt_sph_file_prefix(void *f_plt);
extern void * c_plt_mesh_file_prefix(void *f_plt);
extern void * c_plt_restart_file_prefix(void *f_plt);
extern void * c_plt_field_file_prefix(void *f_plt);
extern void * c_plt_spectr_field_file_prefix(void *f_plt);
extern void * c_plt_coriolis_int_file_name(void *f_plt);
extern void * c_plt_bc_data_file_name_ctl(void *f_plt);
extern void * c_plt_radial_data_file_name_ctl(void *f_plt);
extern void * c_plt_interpolate_sph_to_fem(void *f_plt);
extern void * c_plt_interpolate_fem_to_sph(void *f_plt);
extern void * c_plt_rayleigh_spectr_dir(void *f_plt);
extern void * c_plt_rayleigh_field_dir(void *f_plt);
extern void * c_plt_sph_file_fmt_ctl(void *f_plt);
extern void * c_plt_mesh_file_fmt_ctl(void *f_plt);
extern void * c_plt_restart_file_fmt_ctl(void *f_plt);
extern void * c_plt_field_file_fmt_ctl(void *f_plt);
extern void * c_plt_spectr_field_fmt_ctl(void *f_plt);
extern void * c_plt_itp_file_fmt_ctl(void *f_plt);
extern void * c_plt_coriolis_file_fmt_ctl(void *f_plt);
extern void * c_plt_del_org_data_ctl(void *f_plt);

extern void * c_MHD_forces_block_name(void *f_frc_ctl);
extern void * c_MHD_forces_iflag(void *f_frc_ctl);
extern void * c_MHD_forces_array(void *f_frc_ctl);

struct f_platform_control{
	void * f_self;
    int * f_iflag;
	
	char * c_block_name;
	
	struct int_ctl_item * f_ndomain_ctl;
	struct int_ctl_item * f_num_smp_ctl;
	struct chara_ctl_item * f_debug_flag_ctl;
	struct chara_ctl_item * f_sph_file_prefix;
	struct chara_ctl_item * f_mesh_file_prefix;
	struct chara_ctl_item * f_restart_file_prefix;
	struct chara_ctl_item * f_field_file_prefix;
	struct chara_ctl_item * f_spectr_field_file_prefix;
	struct chara_ctl_item * f_coriolis_int_file_name;
	struct chara_ctl_item * f_bc_data_file_name_ctl;
	struct chara_ctl_item * f_radial_data_file_name_ctl;
	struct chara_ctl_item * f_interpolate_sph_to_fem;
	struct chara_ctl_item * f_interpolate_fem_to_sph;
	struct chara_ctl_item * f_rayleigh_spectr_dir;
	struct chara_ctl_item * f_rayleigh_field_dir;
	struct chara_ctl_item * f_sph_file_fmt_ctl;
	struct chara_ctl_item * f_mesh_file_fmt_ctl;
	struct chara_ctl_item * f_restart_file_fmt_ctl;
	struct chara_ctl_item * f_field_file_fmt_ctl;
	struct chara_ctl_item * f_spectr_field_fmt_ctl;
	struct chara_ctl_item * f_itp_file_fmt_ctl;
	struct chara_ctl_item * f_coriolis_file_fmt_ctl;
	struct chara_ctl_item * f_del_org_data_ctl;
};

struct f_MHD_sph_resolution_control{
	void * f_self;
    int * f_iflag;
	
	char * c_block_name;
	
	struct int_ctl_item *f_ltr_ctl;
	struct int_ctl_item *f_phi_symmetry_ctl;
	struct chara_ctl_item *f_sph_grid_type_ctl;
	struct chara_ctl_item *f_sph_coef_type_ctl;
	struct int_ctl_item *f_ngrid_elevation_ctl;
	struct int_ctl_item *f_ngrid_azimuth_ctl;
	struct int_real_clist *f_radius_ctl;
	struct chara_int_clist *f_radial_grp_ctl;
	struct real_clist *f_add_ext_layer_ctl;
	struct chara_ctl_item *f_radial_grid_type_ctl;
	struct int_ctl_item *f_num_fluid_grid_ctl;
	struct int_ctl_item *f_increment_cheby_ctl;
	struct real_ctl_item *f_fluid_core_size_ctl;
	struct real_ctl_item *f_ICB_to_CMB_ratio_ctl;
	struct real_ctl_item *f_Min_radius_ctl;
	struct real_ctl_item *f_ICB_radius_ctl;
	struct real_ctl_item *f_CMB_radius_ctl;
	struct real_ctl_item *f_Max_radius_ctl;
	struct int_ctl_item *f_num_radial_layer_ctl;
	struct int_ctl_item *f_num_med_layer_ctl;
	struct int2_clist *f_radial_layer_list_ctl;
	struct int2_clist *f_med_layer_list_ctl;
};

struct f_FEM_mesh_FILE_ctl{
	void * f_self;
    int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_FEM_mesh_output_switch;
	struct chara_ctl_item *f_FEM_surface_output_switch;
	struct chara_ctl_item *f_FEM_viewer_output_switch;
	struct chara_ctl_item *f_memory_conservation_ctl;
};

struct f_MHD_sph_subdomain_control{
	void * f_self;
    int * f_iflag;
	
	char * c_block_name;
	
	struct int_ctl_item *f_num_radial_domain_ctl;
	struct int_ctl_item *f_num_horiz_domain_ctl;
	
	struct chara_int_clist *f_ndomain_sph_grid_ctl;
	struct chara_int_clist *f_ndomain_legendre_ctl;
	struct chara_int_clist *f_ndomain_spectr_ctl;
	
	struct chara_ctl_item *f_indices_ordering_set;
	struct chara_ctl_item *f_inner_decomp_ctl;
	
	struct chara_ctl_item *f_rj_inner_loop_ctl;
	struct chara_ctl_item *f_rlm_inner_loop_ctl;
	struct chara_ctl_item *f_rtm_inner_loop_ctl;
	struct chara_ctl_item *f_rtp_inner_loop_ctl;
	
	struct chara_ctl_item *f_rlm_distibution_ctl;
	struct chara_ctl_item *f_simple_r_decomp_ctl;
};

struct f_MHD_sph_shell_control{
	void * f_self;
    int * f_iflag;
	
	char * c_block_name;
	
	struct f_FEM_mesh_FILE_ctl *f_Fmesh_ctl;
	struct f_MHD_sph_subdomain_control *f_sdctl;
	struct f_MHD_sph_resolution_control *f_spctl;
};


struct f_MHD_forces_control{
	void * f_self;
    int * f_iflag;
	char * c_block_name;
	
	struct chara_clist * f_force_names;
};



struct f_MHD_mom_eq_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_real_clist * f_coef_4_intertia;
	struct chara_real_clist * f_coef_4_grad_p;
	struct chara_real_clist * f_coef_4_viscous;
	struct chara_real_clist * f_coef_4_termal_buo;
	struct chara_real_clist * f_coef_4_comp_buo;
	struct chara_real_clist * f_coef_4_Coriolis;
	struct chara_real_clist * f_coef_4_Lorentz;
};

struct f_MHD_induct_eq_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_real_clist * f_coef_4_magne_evo;
	struct chara_real_clist * f_coef_4_mag_diffuse;
	struct chara_real_clist * f_coef_4_mag_potential;
	struct chara_real_clist * f_coef_4_induction;
};

struct f_MHD_heat_eq_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_real_clist * f_coef_4_adv_flux;
	struct chara_real_clist * f_coef_4_diffuse;
	struct chara_real_clist * f_coef_4_source;
};

struct f_MHD_equations_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_MHD_mom_eq_control *    f_mom_eq_ctl;
	struct f_MHD_induct_eq_control * f_induct_ctl;
	struct f_MHD_heat_eq_control *   f_heat_ctl;
	struct f_MHD_heat_eq_control *   f_comp_ctl;
};

struct f_MHD_dimless_control{
	void * f_self;
	int * f_iflag;
    char * c_block_name;
	
	struct chara_real_clist * f_dimess_names;
};




struct f_platform_control * init_f_platform_control(void *(*c_load_self)(void *f_parent), void *f_parent);

struct f_MHD_sph_shell_control * init_f_MHD_sph_shell_ctl(void *(*c_load_self)(void *f_parent), 
														  void *f_parent);

struct f_MHD_forces_control * init_f_MHD_forces_ctl(void *(*c_load_self)(void *f_parent),
													void *f_parent);

struct f_MHD_dimless_control * init_f_MHD_dimless_ctl(void *(*c_load_self)(void *f_parent),
													  void *f_parent);

struct f_MHD_equations_control * init_f_MHD_equations_ctl(void *(*c_load_self)(void *f_parent), void *f_parent);


#endif    /* C_CTL_DATA_PLATFORMS_GTK_H_ */
