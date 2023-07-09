/*
//  ctl_data_platforms_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include <string.h>
#include <stdio.h>
#include <gtk/gtk.h>

#include "skip_comment_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_int_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_control_label_from_f.h"
#include "control_combobox_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "t_ctl_array_chara_int_items_c.h"
#include "t_ctl_array_int_real_items_c.h"
#include "t_ctl_array_int2_items_c.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panel_real_GTK.h"
#include "control_panel_int_real_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_panel_chara_real_GTK.h"
#include "control_panel_chara_int_GTK.h"
#include "control_panel_chara_GTK.h"
#include "tree_view_real_GTK.h"


#ifndef CTL_DATA_PLATFORMS_GTK_
#define CTL_DATA_PLATFORMS_GTK_

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
	
	struct control_labels_f *label_file_format_list;
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

struct f_MHD_sph_resolution_views{
	GtkWidget *f_radius_ctl_tree;
	GtkWidget *f_add_ext_layer_tree;
	GtkWidget *f_radial_grp_ctl_tree;
	GtkWidget *f_radial_layer_list_ctl_tree;
	GtkWidget *f_med_layer_list_ctl_tree;
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

struct f_MHD_sph_subdomain_views{
	GtkWidget *f_ndomain_sph_grid_tree;
	GtkWidget *f_ndomain_legendre_tree;
	GtkWidget *f_ndomain_spectr_tree;
};

struct f_sph_shell_views{
	struct f_MHD_sph_subdomain_views *f_sdctl_vws;
	struct f_MHD_sph_resolution_views *f_spctl_vws;
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

struct f_MHD_mom_eq_views{
	GtkWidget * f_coef_4_intertia_tree;
	GtkWidget * f_coef_4_grad_p_tree;
	GtkWidget * f_coef_4_viscous_tree;
	GtkWidget * f_coef_4_termal_buo_tree;
	GtkWidget * f_coef_4_comp_buo_tree;
	GtkWidget * f_coef_4_Coriolis_tree;
	GtkWidget * f_coef_4_Lorentz_tree;
};

struct f_MHD_induct_eq_views{
	GtkWidget * f_coef_4_magne_evo_tree;
	GtkWidget * f_coef_4_mag_diffuse_tree;
	GtkWidget * f_coef_4_mag_potential_tree;
	GtkWidget * f_coef_4_induction_tree;
};

struct f_MHD_heat_eq_views{
	GtkWidget * f_coef_4_adv_flux_tree;
	GtkWidget * f_coef_4_diffuse_tree;
	GtkWidget * f_coef_4_source_tree;
};

struct f_MHD_equations_views{
	struct f_MHD_mom_eq_views    *f_mom_eq_vws;
	struct f_MHD_induct_eq_views *f_induct_vws;
	struct f_MHD_heat_eq_views   *f_heat_vws;
	struct f_MHD_heat_eq_views   *f_comp_vws;
};




struct f_platform_control * init_f_platform_control(void *(*c_load_self)(void *f_parent), void *f_parent);

void cb_chara_ctl_item(GtkEntry *entry, gpointer data);
void cb_check_toggle(GtkWidget *widget, gpointer iflag_ptr);

GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window);

struct f_MHD_sph_resolution_control * init_f_MHD_sph_resolution_control(void *(*c_load_self)(void *f_parent), 
																		void *f_parent);
struct f_MHD_sph_subdomain_control * init_f_MHD_sph_domain_control(void *(*c_load_self)(void *f_parent), 
																   void *f_parent);
struct f_FEM_mesh_FILE_ctl * init_f_FEM_mesh_FILE_ctl(void *(*c_load_self)(void *f_parent), 
													  void *f_parent);

struct f_MHD_sph_shell_control * init_f_MHD_sph_shell_ctl(void *(*c_load_self)(void *f_parent), 
														  void *f_parent);
GtkWidget *MHD_sph_shell_ctl_expander(GtkWidget *window, struct f_MHD_sph_shell_control * f_psph_ctl, 
									  char * f_fname_psph, struct f_sph_shell_views *f_psph_vws);


struct f_MHD_forces_control * init_f_MHD_forces_ctl(void *(*c_load_self)(void *f_parent),
													void *f_parent);


struct f_MHD_mom_eq_control * init_f_MHD_mom_eq_ctl(void *(*c_load_self)(void *f_parent),
													void *f_parent);
struct f_MHD_induct_eq_control * init_f_MHD_induction_eq_ctl(void *(*c_load_self)(void *f_parent),
															 void *f_parent);
struct f_MHD_heat_eq_control * init_f_MHD_heat_eq_ctl(void *(*c_load_self)(void *f_parent),
													  void *f_parent);
struct f_MHD_equations_control * init_f_MHD_equations_ctl(void *(*c_load_self)(void *f_parent), void *f_parent);


GtkWidget * draw_momentum_equation_vbox(struct f_MHD_mom_eq_control *f_mom_eq_ctl, 
										struct f_MHD_mom_eq_views *f_mom_eq_vws, GtkWidget *window);
GtkWidget * draw_induction_equation_vbox(struct f_MHD_induct_eq_control *f_induct_ctl, 
										 struct f_MHD_induct_eq_views *f_induct_vws, GtkWidget *window);
GtkWidget * draw_heat_equation_vbox(struct f_MHD_heat_eq_control *f_heat_ctl, 
									struct f_MHD_heat_eq_views *f_heat_vws, GtkWidget *window);
GtkWidget * draw_MHD_equations_vbox(struct f_MHD_equations_control *f_eqs_ctl, 
									struct f_MHD_equations_views *f_eqs_vws, GtkWidget *window);


#endif    /* CTL_DATA_PLATFORMS_GTK_ */
