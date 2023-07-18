/*
//  ctl_data_platforms_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "ctl_data_platforms_GTK.h"

extern int lengthchara_f(void);
extern int c_yes_flag(const char *text);
extern int c_no_file_flag(const char *file_name);

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



GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt,
                                       struct chara_clist *label_file_format_list,
                                       GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *hbox_c1 = draw_chara_switch_entry_hbox(f_plt->f_debug_flag_ctl);
	GtkWidget *hbox_i1 = draw_int_item_entry_hbox(f_plt->f_ndomain_ctl);
	GtkWidget *hbox_i2 = draw_int_item_entry_hbox(f_plt->f_num_smp_ctl);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_plt->f_sph_file_prefix);
	GtkWidget *hbox_c3 = draw_chara_item_combobox_hbox(label_file_format_list,
													  f_plt->f_sph_file_fmt_ctl, window);
	/*
	GtkWidget *hbox_c4 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_prefix);
	GtkWidget *hbox_c5 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_fmt_ctl);
	*/
	GtkWidget *hbox_c6 = draw_chara_item_entry_hbox(f_plt->f_restart_file_prefix);
	GtkWidget *hbox_c7 = draw_chara_item_combobox_hbox(label_file_format_list,
													  f_plt->f_restart_file_fmt_ctl, window);
	GtkWidget *hbox_c8 = draw_chara_item_entry_hbox(f_plt->f_field_file_prefix);
	GtkWidget *hbox_c9 = draw_chara_item_combobox_hbox(label_file_format_list,
													  f_plt->f_field_file_fmt_ctl, window);
	/*
 	GtkWidget *hbox_c10 = draw_chara_item_entry_hbox(f_plt->f_spectr_field_file_prefix);
	GtkWidget *hbox_c11 = draw_chara_item_combobox_hbox(label_file_format_list,
													  f_plt->f_spectr_field_fmt_ctl, window);
	GtkWidget *hbox_c12 = draw_chara_item_entry_hbox(f_plt->f_coriolis_int_file_name);
	GtkWidget *hbox_c13 = draw_chara_item_entry_hbox(f_plt->f_coriolis_file_fmt_ctl);
	*/
	GtkWidget *hbox_c14 = draw_chara_item_entry_hbox(f_plt->f_bc_data_file_name_ctl);
	GtkWidget *hbox_c15 = draw_chara_item_entry_hbox(f_plt->f_radial_data_file_name_ctl);
	/*
	GtkWidget *hbox_c16 = draw_chara_item_entry_hbox(f_plt->f_interpolate_sph_to_fem);
	GtkWidget *hbox_c17 = draw_chara_item_entry_hbox(f_plt->f_interpolate_fem_to_sph);
	GtkWidget *hbox_c18 = draw_chara_item_combobox_hboxdraw_chara_item_combobox_hbox(label_file_format_list, 
													  f_plt->f_itp_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c19 = draw_chara_item_entry_hbox(f_plt->f_rayleigh_spectr_dir);
	GtkWidget *hbox_c20 = draw_chara_item_entry_hbox(f_plt->f_rayleigh_field_dir);
	
	GtkWidget *hbox_c21 = draw_chara_item_entry_hbox(f_plt->f_del_org_data_ctl);
	
	GtkWidget *vbox_plt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_i1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_i2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c7, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c8, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c9, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c14, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c15, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c19, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c20, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c21, FALSE, FALSE, 0);
	
	GtkWidget *expand_PLT = draw_control_block(f_plt->c_block_name, f_plt->f_iflag,
											   window, vbox_plt);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_PLT, FALSE, FALSE, 0);
	return vbox_out;
};


GtkWidget * draw_sph_resolution_vbox(struct f_MHD_sph_resolution_control *f_spctl, 
									 struct f_MHD_sph_resolution_views *f_spctl_vws, GtkWidget *window){
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	f_spctl_vws = (struct f_MHD_sph_resolution_views *) malloc(sizeof(struct f_MHD_sph_resolution_views));
	if(f_spctl_vws == NULL){
		printf("malloc error for f_MHD_sph_resolution_views\n");
		exit(0);
	};
	
	GtkWidget *hbox_i1 = draw_int_item_entry_hbox(f_spctl->f_ltr_ctl);
	GtkWidget *hbox_i2 = draw_int_item_entry_hbox(f_spctl->f_phi_symmetry_ctl);
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_spctl->f_sph_grid_type_ctl);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_spctl->f_sph_coef_type_ctl);
	
	GtkWidget *hbox_i3 = draw_int_item_entry_hbox(f_spctl->f_ngrid_elevation_ctl);
	GtkWidget *hbox_i4 = draw_int_item_entry_hbox(f_spctl->f_ngrid_azimuth_ctl);
	
	GtkWidget *hbox_d1 = add_ir_list_box_w_addbottun(f_spctl->f_radius_ctl,
													 f_spctl_vws->f_radius_ctl_tree);
	GtkWidget *hbox_d2 = add_ci_list_box_w_addbottun(f_spctl->f_radial_grp_ctl,
													 f_spctl_vws->f_radial_grp_ctl_tree);
	GtkWidget *hbox_d3 = real_array_vbox_w_addbottun(f_spctl->f_add_ext_layer_ctl,
													 f_spctl_vws->f_add_ext_layer_tree);
	GtkWidget *hbox_c3 = draw_chara_item_entry_hbox(f_spctl->f_radial_grid_type_ctl);
	
	GtkWidget *hbox_i5 = draw_int_item_entry_hbox(f_spctl->f_num_fluid_grid_ctl);
	GtkWidget *hbox_i6 = draw_int_item_entry_hbox(f_spctl->f_increment_cheby_ctl);
	GtkWidget *hbox_r1 = draw_real_item_entry_hbox(f_spctl->f_fluid_core_size_ctl);
	GtkWidget *hbox_r2 = draw_real_item_entry_hbox(f_spctl->f_ICB_to_CMB_ratio_ctl);
	GtkWidget *hbox_r3 = draw_real_item_entry_hbox(f_spctl->f_Min_radius_ctl);
	GtkWidget *hbox_r4 = draw_real_item_entry_hbox(f_spctl->f_ICB_radius_ctl);
	GtkWidget *hbox_r5 = draw_real_item_entry_hbox(f_spctl->f_CMB_radius_ctl);
	GtkWidget *hbox_r6 = draw_real_item_entry_hbox(f_spctl->f_Max_radius_ctl);
	
	GtkWidget *hbox_i7 = draw_int_item_entry_hbox(f_spctl->f_num_radial_layer_ctl);
	GtkWidget *hbox_d4 = add_i2_list_box_w_addbottun(f_spctl->f_radial_layer_list_ctl, 
													 f_spctl_vws->f_radial_layer_list_ctl_tree);
	
	GtkWidget *hbox_i8 = draw_int_item_entry_hbox(f_spctl->f_num_med_layer_ctl);
	GtkWidget *hbox_d5 = add_i2_list_box_w_addbottun(f_spctl->f_med_layer_list_ctl, 
													 f_spctl_vws->f_med_layer_list_ctl_tree);
	
	GtkWidget *vbox_sph = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r5, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i7, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i8, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d5, FALSE, FALSE, 0);
	GtkWidget *expand = draw_control_block(f_spctl->c_block_name, f_spctl->f_iflag,
                                           window, vbox_sph);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand, FALSE, FALSE, 0);
	return vbox_out;
};


GtkWidget * draw_sph_subdomain_vbox(struct f_MHD_sph_subdomain_control *f_sdctl, 
									struct f_MHD_sph_subdomain_views *f_sdctl_vws, GtkWidget *window){
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	f_sdctl_vws = (struct f_MHD_sph_subdomain_views *) malloc(sizeof(struct f_MHD_sph_subdomain_views));
	if(f_sdctl_vws == NULL){
		printf("malloc error for f_MHD_sph_subdomain_views\n");
		exit(0);
	};
	
	GtkWidget *hbox_i1 = draw_int_item_entry_hbox(f_sdctl->f_num_radial_domain_ctl);
	GtkWidget *hbox_i2 = draw_int_item_entry_hbox(f_sdctl->f_num_horiz_domain_ctl);
	
	GtkWidget *hbox_d1 = add_ci_list_box_w_addbottun(f_sdctl->f_ndomain_sph_grid_ctl,
													 f_sdctl_vws->f_ndomain_sph_grid_tree);
	GtkWidget *hbox_d2 = add_ci_list_box_w_addbottun(f_sdctl->f_ndomain_legendre_ctl,
													 f_sdctl_vws->f_ndomain_legendre_tree);
	GtkWidget *hbox_d3 = add_ci_list_box_w_addbottun(f_sdctl->f_ndomain_spectr_ctl,
													 f_sdctl_vws->f_ndomain_spectr_tree);
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_sdctl->f_indices_ordering_set);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_sdctl->f_inner_decomp_ctl);
	GtkWidget *hbox_c3 = draw_chara_item_entry_hbox(f_sdctl->f_rj_inner_loop_ctl);
	GtkWidget *hbox_c4 = draw_chara_item_entry_hbox(f_sdctl->f_rlm_inner_loop_ctl);
	GtkWidget *hbox_c5 = draw_chara_item_entry_hbox(f_sdctl->f_rtm_inner_loop_ctl);
	GtkWidget *hbox_c6 = draw_chara_item_entry_hbox(f_sdctl->f_rtp_inner_loop_ctl);
	GtkWidget *hbox_c7 = draw_chara_item_entry_hbox(f_sdctl->f_rlm_distibution_ctl);
	GtkWidget *hbox_c8 = draw_chara_item_entry_hbox(f_sdctl->f_simple_r_decomp_ctl);
	
	
	GtkWidget *vbox_sph = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i2, FALSE, FALSE, 0);
	
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d3, FALSE, FALSE, 0);
	
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c5, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c7, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c8, FALSE, FALSE, 0);
	
	GtkWidget *expand = draw_control_block(f_sdctl->c_block_name, f_sdctl->f_iflag,
                                           window, vbox_sph);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand, FALSE, FALSE, 0);
	return vbox_out;
};

GtkWidget * draw_sph_FEM_mesh_file_vbox(struct f_FEM_mesh_FILE_ctl *f_Fmesh_ctl, GtkWidget *window){
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *hbox_c1 = draw_chara_switch_entry_hbox(f_Fmesh_ctl->f_FEM_mesh_output_switch);
	GtkWidget *hbox_c2 = draw_chara_switch_entry_hbox(f_Fmesh_ctl->f_FEM_viewer_output_switch);
	GtkWidget *hbox_c3 = draw_chara_switch_entry_hbox(f_Fmesh_ctl->f_FEM_surface_output_switch);
	GtkWidget *hbox_c4 = draw_chara_switch_entry_hbox(f_Fmesh_ctl->f_memory_conservation_ctl);
	
	
	GtkWidget *vbox_sph = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c4, FALSE, FALSE, 0);
	
	GtkWidget *expand = draw_control_block(f_Fmesh_ctl->c_block_name, f_Fmesh_ctl->f_iflag,
										   window, vbox_sph);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand, FALSE, FALSE, 0);
	return vbox_out;
};


GtkWidget *MHD_sph_shell_ctl_expander(GtkWidget *window, struct f_MHD_sph_shell_control * f_psph_ctl, 
									  struct f_sph_shell_views *f_psph_vws){
	GtkWidget *expand_sph_shell;
	
	f_psph_vws = (struct f_sph_shell_views *) malloc(sizeof(struct f_sph_shell_views));
	if(f_psph_vws == NULL){
		printf("malloc error for f_sph_shell_views\n");
		exit(0);
	};
	
	GtkWidget * vbox_sph_FEM_output = draw_sph_FEM_mesh_file_vbox(f_psph_ctl->f_Fmesh_ctl,
																  window);
	GtkWidget * vbox_sph_subdomain = draw_sph_subdomain_vbox(f_psph_ctl->f_sdctl,
															 f_psph_vws->f_sdctl_vws, window);
	GtkWidget * vbox_sph_resolution = draw_sph_resolution_vbox(f_psph_ctl->f_spctl,
															   f_psph_vws->f_spctl_vws, window);
	GtkWidget *vbox_sph_shell = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sph_shell), vbox_sph_FEM_output, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph_shell), vbox_sph_subdomain, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph_shell), vbox_sph_resolution, FALSE, FALSE, 0);
	
	expand_sph_shell = draw_control_block_w_file_switch(f_psph_ctl->c_block_name, f_psph_ctl->f_iflag,
                                                        f_psph_ctl->fname_sph_shell,
                                                        window, vbox_sph_shell);
	return expand_sph_shell;
}




GtkWidget * draw_momentum_equation_vbox(struct f_MHD_mom_eq_control *f_mom_eq_ctl, 
										struct f_MHD_mom_eq_views *f_mom_eq_vws, GtkWidget *window){
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	f_mom_eq_vws = (struct f_MHD_mom_eq_views *) malloc(sizeof(struct f_MHD_mom_eq_views));
	if(f_mom_eq_vws == NULL){
		printf("malloc error for f_MHD_mom_eq_views\n");
		exit(0);
	};
	
	GtkWidget *hbox_d1 = add_cr_list_box_w_addbottun(f_mom_eq_ctl->f_coef_4_intertia,
													 f_mom_eq_vws->f_coef_4_intertia_tree);
	GtkWidget *hbox_d2 = add_cr_list_box_w_addbottun(f_mom_eq_ctl->f_coef_4_grad_p,
													 f_mom_eq_vws->f_coef_4_grad_p_tree);
	GtkWidget *hbox_d3 = add_cr_list_box_w_addbottun(f_mom_eq_ctl->f_coef_4_viscous,
													 f_mom_eq_vws->f_coef_4_viscous_tree);
	GtkWidget *hbox_d4 = add_cr_list_box_w_addbottun(f_mom_eq_ctl->f_coef_4_termal_buo,
													 f_mom_eq_vws->f_coef_4_termal_buo_tree);
	GtkWidget *hbox_d5 = add_cr_list_box_w_addbottun(f_mom_eq_ctl->f_coef_4_comp_buo,
													 f_mom_eq_vws->f_coef_4_comp_buo_tree);
	GtkWidget *hbox_d6 = add_cr_list_box_w_addbottun(f_mom_eq_ctl->f_coef_4_Coriolis,
													 f_mom_eq_vws->f_coef_4_Coriolis_tree);
	GtkWidget *hbox_d7 = add_cr_list_box_w_addbottun(f_mom_eq_ctl->f_coef_4_Lorentz,
													 f_mom_eq_vws->f_coef_4_Lorentz_tree);
	
	GtkWidget *vbox_sph = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d6, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d7, FALSE, FALSE, 0);
	
	GtkWidget *expand = draw_control_block(f_mom_eq_ctl->c_block_name, f_mom_eq_ctl->f_iflag,
										   window, vbox_sph);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand, FALSE, FALSE, 0);
	return vbox_out;
};

GtkWidget * draw_induction_equation_vbox(struct f_MHD_induct_eq_control *f_induct_ctl, 
										 struct f_MHD_induct_eq_views *f_induct_vws, GtkWidget *window){
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	f_induct_vws = (struct f_MHD_induct_eq_views *) malloc(sizeof(struct f_MHD_induct_eq_views));
	if(f_induct_vws == NULL){
		printf("malloc error for f_MHD_induct_eq_views\n");
		exit(0);
	};
	
	GtkWidget *hbox_d1 = add_cr_list_box_w_addbottun(f_induct_ctl->f_coef_4_magne_evo,
													 f_induct_vws->f_coef_4_magne_evo_tree);
	GtkWidget *hbox_d3 = add_cr_list_box_w_addbottun(f_induct_ctl->f_coef_4_mag_potential,
													 f_induct_vws->f_coef_4_mag_potential_tree);
	GtkWidget *hbox_d2 = add_cr_list_box_w_addbottun(f_induct_ctl->f_coef_4_mag_diffuse,
													 f_induct_vws->f_coef_4_mag_diffuse_tree);
	GtkWidget *hbox_d4 = add_cr_list_box_w_addbottun(f_induct_ctl->f_coef_4_induction,
													 f_induct_vws->f_coef_4_induction_tree);
	
	GtkWidget *vbox_sph = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d4, FALSE, FALSE, 0);
	
	GtkWidget *expand = draw_control_block(f_induct_ctl->c_block_name, f_induct_ctl->f_iflag,
										   window, vbox_sph);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand, FALSE, FALSE, 0);
	return vbox_out;
};

#define FONT_NAME "Times New Roman"
#define STRING "Hello, world!"

void draw_coef_and_power(struct chara_real_clist *coef_cr_clist, int font_size, cairo_t *cr){
	char *ctmp;
	double x, y;
	int len;
	int i;
	for(i=0;i<count_chara_real_clist(coef_cr_clist);i++){
		len = strlen(chara_real_clist_at_index(i, coef_cr_clist)->c_tbl);
		ctmp = alloc_string(len+2);
		sprintf(ctmp,"(%s)", chara_real_clist_at_index(i, coef_cr_clist)->c_tbl);
		cairo_show_text(cr, ctmp);
		free(ctmp);
		
		ctmp = alloc_string(32);
		sprintf(ctmp,"(%d)", (int) chara_real_clist_at_index(i, coef_cr_clist)->r_data);
		cairo_set_font_size(cr, (int) ((double) font_size*2./3.));
		cairo_get_current_point(cr, &x, &y);
		cairo_move_to(cr, x, y-18);
		cairo_show_text(cr, ctmp);
		cairo_get_current_point(cr, &x, &y);
		cairo_move_to(cr, x, y+18);
		cairo_set_font_size(cr, font_size);
		free(ctmp);
	};
	return;
}

void draw_induction_equaiton(struct f_MHD_heat_eq_control *f_heat_ctl, cairo_t *cr)
{
	int def_size =  36;
	int ist = 50;
	cairo_set_font_size( cr, def_size );
	cairo_select_font_face( cr, FONT_NAME, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL );
	cairo_move_to( cr, ist, 50 );
	draw_coef_and_power(f_heat_ctl->f_coef_4_adv_flux, def_size, cr);
	cairo_show_text( cr, " D" );
	
	cairo_set_font_size(cr, (int) ((double) def_size*2./3.));
	cairo_show_text( cr, "t" );
	cairo_set_font_size( cr, def_size );
	
	cairo_show_text( cr, "B = " );
	draw_coef_and_power(f_heat_ctl->f_coef_4_diffuse, def_size, cr);
	cairo_show_text( cr, " ∆ B" );
	
	if(count_chara_real_clist(f_heat_ctl->f_coef_4_source) <= 0) return;
	cairo_show_text( cr, " + " );
	draw_coef_and_power(f_heat_ctl->f_coef_4_source, def_size, cr);
	cairo_show_text( cr, " (J x B)" );
	return;
}

void draw_heat_equaiton(struct f_MHD_heat_eq_control *f_heat_ctl, cairo_t *cr)
{
	int def_size =  36;
	int ist = 50;
	cairo_set_font_size( cr, def_size );
	cairo_select_font_face( cr, FONT_NAME, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL );
	cairo_move_to( cr, ist, 50 );
	draw_coef_and_power(f_heat_ctl->f_coef_4_adv_flux, def_size, cr);
	cairo_show_text( cr, " D ω Ω" );
	
	cairo_set_font_size(cr, (int) ((double) def_size*2./3.));
	cairo_show_text( cr, "t" );
	cairo_set_font_size( cr, def_size );
	
	cairo_show_text( cr, "T = " );
	draw_coef_and_power(f_heat_ctl->f_coef_4_diffuse, def_size, cr);
	cairo_show_text( cr, " ∆T" );
	
	if(count_chara_real_clist(f_heat_ctl->f_coef_4_source) <= 0) return;
	cairo_show_text( cr, " + " );
	draw_coef_and_power(f_heat_ctl->f_coef_4_source, def_size, cr);
	cairo_show_text( cr, " Q" );
	
	cairo_set_font_size(cr, (int) ((double) def_size*2./3.));
	cairo_show_text( cr, "T" );
	cairo_set_font_size(cr, def_size);
	return;
}

void draw_comp_equaiton(struct f_MHD_heat_eq_control *f_heat_ctl, cairo_t *cr)
{
	int def_size =  36;
	int ist = 50;
	cairo_set_font_size( cr, def_size );
	cairo_select_font_face( cr, FONT_NAME, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL );
	cairo_move_to( cr, ist, 50 );
	draw_coef_and_power(f_heat_ctl->f_coef_4_adv_flux, def_size, cr);
	cairo_show_text( cr, " D" );
	
	cairo_set_font_size(cr, (int) ((double) def_size*2./3.));
	cairo_show_text( cr, "t" );
	cairo_set_font_size( cr, def_size );
	
	cairo_show_text( cr, "C = " );
	draw_coef_and_power(f_heat_ctl->f_coef_4_diffuse, def_size, cr);
	cairo_show_text( cr, " ∆T" );
	
	if(count_chara_real_clist(f_heat_ctl->f_coef_4_source) <= 0) return;
	cairo_show_text( cr, " + " );
	draw_coef_and_power(f_heat_ctl->f_coef_4_source, def_size, cr);
	cairo_show_text( cr, " Q" );
	
	cairo_set_font_size(cr, (int) ((double) def_size*2./3.));
	cairo_show_text( cr, "C" );
	cairo_set_font_size(cr, def_size);
	return;
}

gboolean draw_callback (GtkWidget *widget, cairo_t *cr, gpointer data)
{
	struct f_MHD_heat_eq_control *f_heat_ctl = (struct f_MHD_heat_eq_control *) g_object_get_data(G_OBJECT(widget), "heat");
	printf("f_heat_ctl y %p \n", f_heat_ctl);
    guint width, height;
    GdkRGBA color;
    GtkStyleContext *context;
    
    context = gtk_widget_get_style_context (widget);
    width = gtk_widget_get_allocated_width (widget);
	height = gtk_widget_get_allocated_height (widget);
	
	gtk_render_background(context, cr, 0, 0, width, height);
	
	cairo_set_source_rgb( cr, 1, 0, 0 );
	cairo_set_font_size( cr, 18 );
	draw_heat_equaiton(f_heat_ctl, cr);
	
//    cairo_arc (cr, width/2.0, height/2.0, MIN (width, height) / 2.0, 0, 2 * G_PI);
    gtk_style_context_get_color (context, gtk_style_context_get_state (context), &color);
    gdk_cairo_set_source_rgba (cr, &color);
    cairo_fill (cr);
    return FALSE;
}



GtkWidget * draw_heat_equation_vbox(struct f_MHD_heat_eq_control *f_heat_ctl, 
									struct f_MHD_heat_eq_views *f_heat_vws, GtkWidget *window){
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	f_heat_vws = (struct f_MHD_heat_eq_views *) malloc(sizeof(struct f_MHD_heat_eq_views));
	if(f_heat_vws == NULL){
		printf("malloc error for f_MHD_heat_eq_views\n");
		exit(0);
	};
	 
	GtkWidget *draw_Area = gtk_drawing_area_new();
	gtk_widget_set_size_request (draw_Area, 540, 80);
	
	g_object_set_data(G_OBJECT(draw_Area), "heat", (gpointer) f_heat_ctl);
	g_signal_connect (G_OBJECT (draw_Area), "draw", G_CALLBACK (draw_callback), NULL);
	
	GtkWidget *hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox_draw), draw_Area, FALSE, FALSE, 0);
	
	GtkWidget *hbox_d1 = add_cr_list_box_w_addbottun(f_heat_ctl->f_coef_4_adv_flux,
													 f_heat_vws->f_coef_4_adv_flux_tree);
	GtkWidget *hbox_d2 = add_cr_list_box_w_addbottun(f_heat_ctl->f_coef_4_diffuse,
													 f_heat_vws->f_coef_4_diffuse_tree);
	GtkWidget *hbox_d3 = add_cr_list_box_w_addbottun(f_heat_ctl->f_coef_4_source,
													 f_heat_vws->f_coef_4_source_tree);
	
	GtkWidget *vbox_sph = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_draw, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d3, FALSE, FALSE, 0);
	
	GtkWidget *expand = draw_control_block(f_heat_ctl->c_block_name, f_heat_ctl->f_iflag,
										   window, vbox_sph);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand, FALSE, FALSE, 0);
	return vbox_out;
};


GtkWidget * draw_MHD_equations_vbox(struct f_MHD_equations_control *f_eqs_ctl, 
									struct f_MHD_equations_views *f_eqs_vws, GtkWidget *window){
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	f_eqs_vws = (struct f_MHD_equations_views *) malloc(sizeof(struct f_MHD_equations_views));
	if(f_eqs_vws == NULL){
		printf("malloc error for f_MHD_equations_views\n");
		exit(0);
	};
	
	GtkWidget *vbox_MHD_mom =   draw_momentum_equation_vbox(f_eqs_ctl->f_mom_eq_ctl, 
															f_eqs_vws->f_mom_eq_vws, window);
	GtkWidget *vbox_MHD_magne = draw_induction_equation_vbox(f_eqs_ctl->f_induct_ctl, 
															 f_eqs_vws->f_induct_vws, window);
	GtkWidget *vbox_MHD_heat = draw_heat_equation_vbox(f_eqs_ctl->f_heat_ctl, 
													   f_eqs_vws->f_heat_vws, window);
	GtkWidget *vbox_MHD_light = draw_heat_equation_vbox(f_eqs_ctl->f_comp_ctl, 
														f_eqs_vws->f_comp_vws, window);
	
	gtk_box_pack_start(GTK_BOX(vbox_out), vbox_MHD_mom,   FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_out), vbox_MHD_magne, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_out), vbox_MHD_heat,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_out), vbox_MHD_light, FALSE, FALSE, 0);
	return vbox_out;
};
