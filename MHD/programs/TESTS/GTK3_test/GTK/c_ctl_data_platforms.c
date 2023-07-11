/*
//  c_ctl_data_platforms.o.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "c_ctl_data_platforms.h"

extern int lengthchara_f();
extern int c_yes_flag(const char *text);
extern int c_no_file_flag(const char *file_name);
extern int num_file_fmt_items_f();
extern void set_file_fmt_items_f(char *fmt_names_c);

extern void * c_sphere_data_ctl_block_name(void *f_spctl);
extern void * c_sphere_data_ctl_iflag(void *f_spctl);
extern void * c_sphere_data_ltr_ctl(void *f_spctl);
extern void * c_sphere_data_phi_symmetry_ctl(void *f_spctl);
extern void * c_sphere_data_sph_grd_tpe_ctl(void *f_spctl);
extern void * c_sphere_data_coef_type_ctl(void *f_spctl);
extern void * c_sphere_data_n_elevation_ctl(void *f_spctl);
extern void * c_sphere_data_ngrid_azmth_ctl(void *f_spctl);
extern void * c_sphere_data_radius_ctl(void *f_spctl);
extern void * c_sphere_data_radial_grp_ctl(void *f_spctl);
extern void * c_sphere_data_add_ext_ctl(void *f_spctl);
extern void * c_sphere_data_r_grid_type_ctl(void *f_spctl);
extern void * c_sphere_data_num_fld_grid_ctl(void *f_spctl);
extern void * c_sphere_data_icrmnt_cheby_ctl(void *f_spctl);
extern void * c_sphere_data_Min_radius_ctl(void *f_spctl);
extern void * c_sphere_data_ICB_radius_ctl(void *f_spctl);
extern void * c_sphere_data_CMB_radius_ctl(void *f_spctl);
extern void * c_sphere_data_Max_radius_ctl(void *f_spctl);
extern void * c_sphere_data_fld_core_sze_ctl(void *f_spctl);
extern void * c_sphere_data_ICB_CMB_ratio_ctl(void *f_spctl);
extern void * c_sphere_data_num_r_layer_ctl(void *f_spctl);
extern void * c_sphere_data_n_med_layer_ctl(void *f_spctl);
extern void * c_sphere_data_r_layer_list_ctl(void *f_spctl);
extern void * c_sphere_data_med_list_ctl(void *f_spctl);

extern void * c_sph_indices_ordering_set(void *f_sdctl);
extern void * c_sph_domain_ctl_block_name(void *f_sdctl);
extern void * c_sph_domain_ctl_iflag(void *f_sdctl);
extern void * c_sph_inner_decomp_ctl(void *f_sdctl);
extern void * c_sph_rj_inner_loop_ctl(void *f_sdctl);
extern void * c_sph_rlm_inner_loop_ctl(void *f_sdctl);
extern void * c_sph_rtm_inner_loop_ctl(void *f_sdctl);
extern void * c_sph_rtp_inner_loop_ctl(void *f_sdctl);
extern void * c_sph_domain_rlm_distr_ctl(void *f_sdctl);
extern void * c_sph_domain_smpl_r_decomp_ctl(void *f_sdctl);
extern void * c_sph_num_radial_domain_ctl(void *f_sdctl);
extern void * c_sph_num_horiz_domain_ctl(void *f_sdctl);
extern void * c_sph_ndomain_sph_grid_ctl(void *f_sdctl);
extern void * c_sph_ndomain_legendre_ctl(void *f_sdctl);
extern void * c_sph_ndomain_spectr_ctl(void *f_sdctl);

extern void * c_FEM_mesh_FILE_ctl_block_name(void *f_Fmesh_ctl);
extern void * c_FEM_mesh_FILE_ctl_iflag(void *f_Fmesh_ctl);
extern void * c_FEM_mesh_mem_conserve_ctl(void *f_Fmesh_ctl);
extern void * c_FEM_mesh_output_switch(void *f_Fmesh_ctl);
extern void * c_FEM_surface_output_switch(void *f_Fmesh_ctl);
extern void * c_FEM_viewer_output_switch(void *f_Fmesh_ctl);

extern void * c_sph_shell_ctl_block_name(void *f_psph_ctl);
extern void * c_sph_shell_ctl_iflag(void *f_psph_ctl);
extern void * c_sph_shell_Fmesh_ctl(void *f_psph_ctl);
extern void * c_sph_shell_spctl(void *f_psph_ctl);
extern void * c_sph_shell_sdctl(void *f_psph_ctl);

extern void * c_MHD_momentum_eq_block_name(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_iflag(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_viscous(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_inertia(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_grad_p(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_t_buoyancy(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_c_buoyancy(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_coriolis(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_lorentz(void *f_mom_eq_ctl);

extern void * c_MHD_induction_block_name(void *f_induct_ctl);
extern void * c_MHD_induction_iflag(void *f_induct_ctl);
extern void * c_MHD_induction_evo(void *f_induct_ctl);
extern void * c_MHD_induction_diffuse(void *f_induct_ctl);
extern void * c_MHD_induction_potential(void *f_induct_ctl);
extern void * c_MHD_induction_uxb(void *f_induct_ctl);

extern void * c_MHD_heat_block_name(void *f_heat_ctl);
extern void * c_MHD_heat_iflag(void *f_heat_ctl);
extern void * c_MHD_heat_advect(void *f_heat_ctl);
extern void * c_MHD_heat_diffuse(void *f_heat_ctl);
extern void * c_MHD_heat_source(void *f_heat_ctl);

extern void * c_MHD_eqs_block_name(void *f_eqs_ctl);
extern void * c_MHD_eqs_iflag(void *f_eqs_ctl);
extern void * c_MHD_eqs_mom_ctl(void *f_eqs_ctl);
extern void * c_MHD_eqs_induct_ctl(void *f_eqs_ctl);
extern void * c_MHD_eqs_heat_ctl(void *f_eqs_ctl);
extern void * c_MHD_eqs_comp_ctl(void *f_eqs_ctl);

extern void * c_MHD_dimless_block_name(void *f_dimless_ctl);
extern void * c_MHD_dimless_iflag(void *f_dimless_ctl);
extern void * c_MHD_dimless_array(void *f_dimless_ctl);


struct f_platform_control * init_f_platform_control(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_platform_control *f_plt = (struct f_platform_control *) malloc(sizeof(struct f_platform_control));
	if(f_plt == NULL){
		printf("malloc error for f_plt\n");
		exit(0);
	};
	f_plt->f_self =  c_load_self(f_parent);
	
	f_plt->f_iflag =       (int *)  c_plt_iflag(f_plt->f_self);
	char *f_block_name =  (char *) c_plt_block_name(f_plt->f_self);
	f_plt->c_block_name = strngcopy_from_f(f_block_name);
	
	f_plt->f_ndomain_ctl =               init_f_ctl_int_item(c_plt_ndomain_ctl, f_plt->f_self);
	f_plt->f_num_smp_ctl =               init_f_ctl_int_item(c_plt_num_smp_ctl, f_plt->f_self);
	
	f_plt->f_debug_flag_ctl =            init_f_ctl_chara_item(c_plt_debug_flag_ctl, f_plt->f_self);
	f_plt->f_sph_file_prefix =           init_f_ctl_chara_item(c_plt_sph_file_prefix, f_plt->f_self);
	f_plt->f_mesh_file_prefix =          init_f_ctl_chara_item(c_plt_mesh_file_prefix, f_plt->f_self);
	f_plt->f_restart_file_prefix =       init_f_ctl_chara_item(c_plt_restart_file_prefix, f_plt->f_self);
	f_plt->f_field_file_prefix =         init_f_ctl_chara_item(c_plt_field_file_prefix, f_plt->f_self);
	f_plt->f_spectr_field_file_prefix =  init_f_ctl_chara_item(c_plt_spectr_field_file_prefix, f_plt->f_self);
	f_plt->f_coriolis_int_file_name =    init_f_ctl_chara_item(c_plt_coriolis_int_file_name, f_plt->f_self);
	f_plt->f_bc_data_file_name_ctl =     init_f_ctl_chara_item(c_plt_bc_data_file_name_ctl, f_plt->f_self);
	f_plt->f_radial_data_file_name_ctl = init_f_ctl_chara_item(c_plt_radial_data_file_name_ctl, f_plt->f_self);
	f_plt->f_interpolate_sph_to_fem =    init_f_ctl_chara_item(c_plt_interpolate_sph_to_fem, f_plt->f_self);
	f_plt->f_interpolate_fem_to_sph =    init_f_ctl_chara_item(c_plt_interpolate_fem_to_sph, f_plt->f_self);
	f_plt->f_rayleigh_spectr_dir =       init_f_ctl_chara_item(c_plt_rayleigh_spectr_dir, f_plt->f_self);
	f_plt->f_rayleigh_field_dir =        init_f_ctl_chara_item(c_plt_rayleigh_field_dir, f_plt->f_self);
	f_plt->f_sph_file_fmt_ctl =          init_f_ctl_chara_item(c_plt_sph_file_fmt_ctl, f_plt->f_self);
	f_plt->f_mesh_file_fmt_ctl =         init_f_ctl_chara_item(c_plt_mesh_file_fmt_ctl, f_plt->f_self);
	f_plt->f_restart_file_fmt_ctl =      init_f_ctl_chara_item(c_plt_restart_file_fmt_ctl, f_plt->f_self);
	f_plt->f_field_file_fmt_ctl =        init_f_ctl_chara_item(c_plt_field_file_fmt_ctl, f_plt->f_self);
	f_plt->f_spectr_field_fmt_ctl =      init_f_ctl_chara_item(c_plt_spectr_field_fmt_ctl, f_plt->f_self);
	f_plt->f_itp_file_fmt_ctl =          init_f_ctl_chara_item(c_plt_itp_file_fmt_ctl, f_plt->f_self);
	f_plt->f_coriolis_file_fmt_ctl =     init_f_ctl_chara_item(c_plt_coriolis_file_fmt_ctl, f_plt->f_self);
	f_plt->f_del_org_data_ctl =          init_f_ctl_chara_item(c_plt_del_org_data_ctl, f_plt->f_self);
	
	f_plt->label_file_format_list = init_control_labels_f(num_file_fmt_items_f, 
														  set_file_fmt_items_f);
	check_control_labels_f(f_plt->label_file_format_list);
	return f_plt;
}


struct f_MHD_sph_resolution_control * init_f_MHD_sph_resolution_control(void *(*c_load_self)(void *f_parent), 
																		void *f_parent)
{
	struct f_MHD_sph_resolution_control *f_spctl 
			= (struct f_MHD_sph_resolution_control *) malloc(sizeof(struct f_MHD_sph_resolution_control));
	if(f_spctl == NULL){
		printf("malloc error for f_spctl\n");
		exit(0);
	};
	
	f_spctl->f_self =  c_load_self(f_parent);
	
	f_spctl->f_iflag =        (int *) c_sphere_data_ctl_iflag(f_spctl->f_self);
	char *f_block_name =   (char *) c_sphere_data_ctl_block_name(f_spctl->f_self);
	f_spctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_spctl->f_ltr_ctl =               init_f_ctl_int_item(c_sphere_data_ltr_ctl, f_spctl->f_self);
	f_spctl->f_phi_symmetry_ctl =      init_f_ctl_int_item(c_sphere_data_phi_symmetry_ctl, f_spctl->f_self);
	f_spctl->f_sph_grid_type_ctl =     init_f_ctl_chara_item(c_sphere_data_sph_grd_tpe_ctl, f_spctl->f_self);
	f_spctl->f_sph_coef_type_ctl =     init_f_ctl_chara_item(c_sphere_data_coef_type_ctl, f_spctl->f_self);
	f_spctl->f_ngrid_elevation_ctl =   init_f_ctl_int_item(c_sphere_data_n_elevation_ctl, f_spctl->f_self);
	f_spctl->f_ngrid_azimuth_ctl =     init_f_ctl_int_item(c_sphere_data_ngrid_azmth_ctl, f_spctl->f_self);
	f_spctl->f_radius_ctl =            init_f_ctl_ir_array(c_sphere_data_radius_ctl, f_spctl->f_self);
	f_spctl->f_radial_grp_ctl =        init_f_ctl_ci_array(c_sphere_data_radial_grp_ctl, f_spctl->f_self);
	f_spctl->f_add_ext_layer_ctl =     init_f_ctl_real_array(c_sphere_data_add_ext_ctl, f_spctl->f_self);
	f_spctl->f_radial_grid_type_ctl =  init_f_ctl_chara_item(c_sphere_data_r_grid_type_ctl, f_spctl->f_self);
	f_spctl->f_num_fluid_grid_ctl =    init_f_ctl_int_item(c_sphere_data_num_fld_grid_ctl, f_spctl->f_self);
	f_spctl->f_increment_cheby_ctl =   init_f_ctl_int_item(c_sphere_data_icrmnt_cheby_ctl, f_spctl->f_self);
	f_spctl->f_Min_radius_ctl =        init_f_ctl_real_item(c_sphere_data_Min_radius_ctl, f_spctl->f_self);
	f_spctl->f_ICB_radius_ctl =        init_f_ctl_real_item(c_sphere_data_ICB_radius_ctl, f_spctl->f_self);
	f_spctl->f_CMB_radius_ctl =        init_f_ctl_real_item(c_sphere_data_CMB_radius_ctl, f_spctl->f_self);
	f_spctl->f_Max_radius_ctl =        init_f_ctl_real_item(c_sphere_data_Max_radius_ctl, f_spctl->f_self);
	f_spctl->f_fluid_core_size_ctl =   init_f_ctl_real_item(c_sphere_data_fld_core_sze_ctl, f_spctl->f_self);
	f_spctl->f_ICB_to_CMB_ratio_ctl =  init_f_ctl_real_item(c_sphere_data_ICB_CMB_ratio_ctl, f_spctl->f_self);
	f_spctl->f_num_radial_layer_ctl =  init_f_ctl_int_item(c_sphere_data_num_r_layer_ctl, f_spctl->f_self);
	f_spctl->f_num_med_layer_ctl =     init_f_ctl_int_item(c_sphere_data_n_med_layer_ctl, f_spctl->f_self);
	f_spctl->f_radial_layer_list_ctl = init_f_ctl_i2_array(c_sphere_data_r_layer_list_ctl, f_spctl->f_self);
	f_spctl->f_med_layer_list_ctl =    init_f_ctl_i2_array(c_sphere_data_med_list_ctl, f_spctl->f_self);
	return f_spctl;
};

struct f_MHD_sph_subdomain_control * init_f_MHD_sph_domain_control(void *(*c_load_self)(void *f_parent), 
																   void *f_parent)
{
	struct f_MHD_sph_subdomain_control *f_sdctl 
			= (struct f_MHD_sph_subdomain_control *) malloc(sizeof(struct f_MHD_sph_subdomain_control));
	if(f_sdctl == NULL){
		printf("malloc error for f_sdctl\n");
		exit(0);
	};
	
	f_sdctl->f_self =  c_load_self(f_parent);
	
	f_sdctl->f_iflag =        (int *) c_sph_domain_ctl_iflag(f_sdctl->f_self);
	char *f_block_name =   (char *) c_sph_domain_ctl_block_name(f_sdctl->f_self);
	f_sdctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_sdctl->f_indices_ordering_set = init_f_ctl_chara_item(c_sph_indices_ordering_set, f_sdctl->f_self);
	f_sdctl->f_inner_decomp_ctl =     init_f_ctl_chara_item(c_sph_inner_decomp_ctl, f_sdctl->f_self);
	f_sdctl->f_rj_inner_loop_ctl =    init_f_ctl_chara_item(c_sph_rj_inner_loop_ctl, f_sdctl->f_self);
	f_sdctl->f_rlm_inner_loop_ctl =   init_f_ctl_chara_item(c_sph_rlm_inner_loop_ctl, f_sdctl->f_self);
	f_sdctl->f_rtm_inner_loop_ctl =   init_f_ctl_chara_item(c_sph_rtm_inner_loop_ctl, f_sdctl->f_self);
	f_sdctl->f_rtp_inner_loop_ctl =   init_f_ctl_chara_item(c_sph_rtp_inner_loop_ctl, f_sdctl->f_self);
	f_sdctl->f_rlm_distibution_ctl =  init_f_ctl_chara_item(c_sph_domain_rlm_distr_ctl, f_sdctl->f_self);
	f_sdctl->f_simple_r_decomp_ctl =  init_f_ctl_chara_item(c_sph_domain_smpl_r_decomp_ctl, f_sdctl->f_self);
	f_sdctl->f_num_radial_domain_ctl = init_f_ctl_int_item(c_sph_num_radial_domain_ctl, f_sdctl->f_self);
	f_sdctl->f_num_horiz_domain_ctl =  init_f_ctl_int_item(c_sph_num_horiz_domain_ctl, f_sdctl->f_self);
	f_sdctl->f_ndomain_sph_grid_ctl =  init_f_ctl_ci_array(c_sph_ndomain_sph_grid_ctl, f_sdctl->f_self);
	f_sdctl->f_ndomain_legendre_ctl =  init_f_ctl_ci_array(c_sph_ndomain_legendre_ctl, f_sdctl->f_self);
	f_sdctl->f_ndomain_spectr_ctl =    init_f_ctl_ci_array(c_sph_ndomain_spectr_ctl, f_sdctl->f_self);
	return f_sdctl;
}

struct f_FEM_mesh_FILE_ctl * init_f_FEM_mesh_FILE_ctl(void *(*c_load_self)(void *f_parent), 
													  void *f_parent)
{
	struct f_FEM_mesh_FILE_ctl *f_Fmesh_ctl 
			= (struct f_FEM_mesh_FILE_ctl *) malloc(sizeof(struct f_FEM_mesh_FILE_ctl));
	if(f_Fmesh_ctl == NULL){
		printf("malloc error for f_Fmesh_ctl\n");
		exit(0);
	};
	
	f_Fmesh_ctl->f_self =  c_load_self(f_parent);
	
	f_Fmesh_ctl->f_iflag =        (int *) c_FEM_mesh_FILE_ctl_iflag(f_Fmesh_ctl->f_self);
	char *f_block_name =   (char *) c_FEM_mesh_FILE_ctl_block_name(f_Fmesh_ctl->f_self);
	f_Fmesh_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_Fmesh_ctl->f_memory_conservation_ctl =   init_f_ctl_chara_item(c_FEM_mesh_mem_conserve_ctl, f_Fmesh_ctl->f_self);
	f_Fmesh_ctl->f_FEM_mesh_output_switch =    init_f_ctl_chara_item(c_FEM_mesh_output_switch, f_Fmesh_ctl->f_self);
	f_Fmesh_ctl->f_FEM_surface_output_switch = init_f_ctl_chara_item(c_FEM_surface_output_switch, f_Fmesh_ctl->f_self);
	f_Fmesh_ctl->f_FEM_viewer_output_switch =  init_f_ctl_chara_item(c_FEM_viewer_output_switch, f_Fmesh_ctl->f_self);
	return f_Fmesh_ctl;
}


struct f_MHD_sph_shell_control * init_f_MHD_sph_shell_ctl(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_sph_shell_control *f_psph_ctl 
			= (struct f_MHD_sph_shell_control *) malloc(sizeof(struct f_MHD_sph_shell_control));
	if(f_psph_ctl == NULL){
		printf("malloc error for f_psph_ctl\n");
		exit(0);
	};
	
	f_psph_ctl->f_self =  c_load_self(f_parent);
	
	f_psph_ctl->f_iflag =        (int *) c_sph_shell_ctl_iflag(f_psph_ctl->f_self);
	char *f_block_name =   (char *) c_sph_shell_ctl_block_name(f_psph_ctl->f_self);
	f_psph_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_psph_ctl->f_Fmesh_ctl = init_f_FEM_mesh_FILE_ctl(c_sph_shell_Fmesh_ctl, f_psph_ctl->f_self);
	f_psph_ctl->f_sdctl =     init_f_MHD_sph_domain_control(c_sph_shell_sdctl, f_psph_ctl->f_self);
	f_psph_ctl->f_spctl =     init_f_MHD_sph_resolution_control(c_sph_shell_spctl, f_psph_ctl->f_self);
	return f_psph_ctl;
};


struct f_MHD_forces_control * init_f_MHD_forces_ctl(void *(*c_load_self)(void *f_parent),
													void *f_parent)
{
	struct f_MHD_forces_control *f_frc_ctl
			= (struct f_MHD_forces_control *) malloc(sizeof(struct f_MHD_forces_control));
	if(f_frc_ctl == NULL){
		printf("malloc error for f_frc_ctl\n");
		exit(0);
	};
	
	f_frc_ctl->f_self =  c_load_self(f_parent);
	
	f_frc_ctl->f_iflag =        (int *)  c_MHD_forces_iflag(f_frc_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_forces_block_name(f_frc_ctl->f_self);
	f_frc_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_frc_ctl->f_force_names = init_f_ctl_chara_array(c_MHD_forces_array, f_frc_ctl->f_self);
	return f_frc_ctl;
};

struct f_MHD_dimless_control * init_f_MHD_dimless_ctl(void *(*c_load_self)(void *f_parent),
													 void *f_parent)
{
	struct f_MHD_dimless_control *f_dimless_ctl
			= (struct f_MHD_dimless_control *) malloc(sizeof(struct f_MHD_dimless_control));
	if(f_dimless_ctl == NULL){
		printf("malloc error for f_dimless_ctl\n");
		exit(0);
	};
	
	f_dimless_ctl->f_self =  c_load_self(f_parent);
	
	f_dimless_ctl->f_iflag =        (int *)  c_MHD_dimless_iflag(f_dimless_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_dimless_block_name(f_dimless_ctl->f_self);
	f_dimless_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_dimless_ctl->f_dimess_names = init_f_ctl_cr_array(c_MHD_dimless_array, f_dimless_ctl->f_self);
	int i;
	return f_dimless_ctl;
};




struct f_MHD_mom_eq_control * init_f_MHD_mom_eq_ctl(void *(*c_load_self)(void *f_parent),
													void *f_parent)
{
	struct f_MHD_mom_eq_control *f_mom_eq_ctl
			= (struct f_MHD_mom_eq_control *) malloc(sizeof(struct f_MHD_mom_eq_control));
	if(f_mom_eq_ctl == NULL){
		printf("malloc error for f_mom_eq_ctl\n");
		exit(0);
	};
	
	f_mom_eq_ctl->f_self =  c_load_self(f_parent);
	
	char *f_block_name =   (char *) c_MHD_momentum_eq_block_name(f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_iflag =        (int *)  c_MHD_momentum_eq_iflag(f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_mom_eq_ctl->f_coef_4_viscous =    init_f_ctl_cr_array(c_MHD_momentum_eq_viscous, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_coef_4_intertia =   init_f_ctl_cr_array(c_MHD_momentum_eq_inertia, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_coef_4_grad_p =     init_f_ctl_cr_array(c_MHD_momentum_eq_grad_p, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_coef_4_termal_buo = init_f_ctl_cr_array(c_MHD_momentum_eq_t_buoyancy, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_coef_4_comp_buo =   init_f_ctl_cr_array(c_MHD_momentum_eq_c_buoyancy, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_coef_4_Coriolis =   init_f_ctl_cr_array(c_MHD_momentum_eq_coriolis, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_coef_4_Lorentz =    init_f_ctl_cr_array(c_MHD_momentum_eq_lorentz, f_mom_eq_ctl->f_self);
	
	return f_mom_eq_ctl;
};

struct f_MHD_induct_eq_control * init_f_MHD_induction_eq_ctl(void *(*c_load_self)(void *f_parent),
															 void *f_parent)
{
	struct f_MHD_induct_eq_control *f_induct_ctl
			= (struct f_MHD_induct_eq_control *) malloc(sizeof(struct f_MHD_induct_eq_control));
	if(f_induct_ctl == NULL){
		printf("malloc error for f_induct_ctl\n");
		exit(0);
	};
	
	f_induct_ctl->f_self =  c_load_self(f_parent);
	
	f_induct_ctl->f_iflag =        (int *)  c_MHD_induction_iflag(f_induct_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_induction_block_name(f_induct_ctl->f_self);
	f_induct_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_induct_ctl->f_coef_4_magne_evo = init_f_ctl_cr_array(c_MHD_induction_evo,           f_induct_ctl->f_self);
	f_induct_ctl->f_coef_4_mag_diffuse = init_f_ctl_cr_array(c_MHD_induction_diffuse,     f_induct_ctl->f_self);
	f_induct_ctl->f_coef_4_mag_potential = init_f_ctl_cr_array(c_MHD_induction_potential, f_induct_ctl->f_self);
	f_induct_ctl->f_coef_4_induction = init_f_ctl_cr_array(c_MHD_induction_uxb,           f_induct_ctl->f_self);
	return f_induct_ctl;
};

struct f_MHD_heat_eq_control * init_f_MHD_heat_eq_ctl(void *(*c_load_self)(void *f_parent),
													  void *f_parent)
{
	struct f_MHD_heat_eq_control *f_heat_ctl
			= (struct f_MHD_heat_eq_control *) malloc(sizeof(struct f_MHD_heat_eq_control));
	if(f_heat_ctl == NULL){
		printf("malloc error for f_heat_ctl\n");
		exit(0);
	};
	
	f_heat_ctl->f_self =  c_load_self(f_parent);
	
	f_heat_ctl->f_iflag =        (int *)  c_MHD_heat_iflag(f_heat_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_heat_block_name(f_heat_ctl->f_self);
	f_heat_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_heat_ctl->f_coef_4_adv_flux = init_f_ctl_cr_array(c_MHD_heat_advect, f_heat_ctl->f_self);
	f_heat_ctl->f_coef_4_diffuse = init_f_ctl_cr_array(c_MHD_heat_diffuse, f_heat_ctl->f_self);
	f_heat_ctl->f_coef_4_source = init_f_ctl_cr_array(c_MHD_heat_source,   f_heat_ctl->f_self);
	
	return f_heat_ctl;
};

struct f_MHD_equations_control * init_f_MHD_equations_ctl(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_equations_control *f_eqs_ctl 
			= (struct f_MHD_equations_control *) malloc(sizeof(struct f_MHD_equations_control));
	if(f_eqs_ctl == NULL){
		printf("malloc error for f_eqs_ctl\n");
		exit(0);
	};
	
	f_eqs_ctl->f_self =  c_load_self(f_parent);
	
	f_eqs_ctl->f_iflag =        (int *) c_MHD_eqs_iflag(f_eqs_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_eqs_block_name(f_eqs_ctl->f_self);
	f_eqs_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_eqs_ctl->f_mom_eq_ctl = init_f_MHD_mom_eq_ctl(c_MHD_eqs_mom_ctl, f_eqs_ctl->f_self);
	f_eqs_ctl->f_induct_ctl = init_f_MHD_induction_eq_ctl(c_MHD_eqs_induct_ctl, f_eqs_ctl->f_self);
	f_eqs_ctl->f_heat_ctl =   init_f_MHD_heat_eq_ctl(c_MHD_eqs_heat_ctl, f_eqs_ctl->f_self);
	f_eqs_ctl->f_comp_ctl =   init_f_MHD_heat_eq_ctl(c_MHD_eqs_comp_ctl, f_eqs_ctl->f_self);
	return f_eqs_ctl;
}


