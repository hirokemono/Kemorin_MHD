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
#include "control_panel_chara_int_GTK.h"
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

extern void * c_chara_real_item_block_name(void *f_ctl);
extern void * c_chara_real_item_iflag(void *f_ctl);
extern void * c_chara_real_item_charavalue(void *f_ctl);
extern void * c_chara_real_item_realvalue(void *f_ctl);

extern void * c_chara_real_array_block_name(void *f_ctl);
extern void * c_chara_real_array_num(void *f_ctl);
extern void * c_chara_real_array_icou(void *f_ctl);
extern void * c_chara_real_array_c_tbl(void *f_ctl);
extern void * c_chara_real_array_r_tbl(void *f_ctl);

struct c_array_views{
    int index_c_array;
    GtkWidget *c_array_tree_view;
    GtkWidget *c_array_default_view;
    
    struct chara_clist *c_array_clist;
};

struct cr_array_views{
    int index_cr_array;
    GtkWidget *cr_array_tree_view;
    GtkWidget *cr_array_default_view;
    
    struct chara_real_clist *cr_array_clist;
};


struct f_platform_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_int_item * f_ndomain_ctl;
	struct f_ctl_int_item * f_num_smp_ctl;
	struct f_ctl_chara_item * f_debug_flag_ctl;
	struct f_ctl_chara_item * f_sph_file_prefix;
	struct f_ctl_chara_item * f_mesh_file_prefix;
	struct f_ctl_chara_item * f_restart_file_prefix;
	struct f_ctl_chara_item * f_field_file_prefix;
	struct f_ctl_chara_item * f_spectr_field_file_prefix;
	struct f_ctl_chara_item * f_coriolis_int_file_name;
	struct f_ctl_chara_item * f_bc_data_file_name_ctl;
	struct f_ctl_chara_item * f_radial_data_file_name_ctl;
	struct f_ctl_chara_item * f_interpolate_sph_to_fem;
	struct f_ctl_chara_item * f_interpolate_fem_to_sph;
	struct f_ctl_chara_item * f_rayleigh_spectr_dir;
	struct f_ctl_chara_item * f_rayleigh_field_dir;
	struct f_ctl_chara_item * f_sph_file_fmt_ctl;
	struct f_ctl_chara_item * f_mesh_file_fmt_ctl;
	struct f_ctl_chara_item * f_restart_file_fmt_ctl;
	struct f_ctl_chara_item * f_field_file_fmt_ctl;
	struct f_ctl_chara_item * f_spectr_field_fmt_ctl;
	struct f_ctl_chara_item * f_itp_file_fmt_ctl;
	struct f_ctl_chara_item * f_coriolis_file_fmt_ctl;
	struct f_ctl_chara_item * f_del_org_data_ctl;
	
	struct control_labels_f *label_file_format_list;
};

struct f_MHD_sph_resolution_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_int_item *f_ltr_ctl;
	struct f_ctl_int_item *f_phi_symmetry_ctl;
	struct f_ctl_chara_item *f_sph_grid_type_ctl;
	struct f_ctl_chara_item *f_sph_coef_type_ctl;
	struct f_ctl_int_item *f_ngrid_elevation_ctl;
	struct f_ctl_int_item *f_ngrid_azimuth_ctl;
	struct f_ctl_ir_array *f_radius_ctl;
	struct f_ctl_ci_array *f_radial_grp_ctl;
	struct f_ctl_real_array *f_add_ext_layer_ctl;
	struct f_ctl_chara_item *f_radial_grid_type_ctl;
	struct f_ctl_int_item *f_num_fluid_grid_ctl;
	struct f_ctl_int_item *f_increment_cheby_ctl;
	struct f_ctl_real_item *f_fluid_core_size_ctl;
	struct f_ctl_real_item *f_ICB_to_CMB_ratio_ctl;
	struct f_ctl_real_item *f_Min_radius_ctl;
	struct f_ctl_real_item *f_ICB_radius_ctl;
	struct f_ctl_real_item *f_CMB_radius_ctl;
	struct f_ctl_real_item *f_Max_radius_ctl;
	struct f_ctl_int_item *f_num_radial_layer_ctl;
	struct f_ctl_int_item *f_num_med_layer_ctl;
	struct int2_clist *f_radial_layer_list_ctl;
	struct int2_clist *f_med_layer_list_ctl;
};

struct f_MHD_sph_resolution_views{
	GtkWidget *f_radius_ctl_tree;
	struct r_clist_view  *f_add_ext_layer_vws;
	struct ci_clist_view *f_radial_grp_ctl_vws;
	GtkWidget *f_radial_layer_list_ctl_tree;
	GtkWidget *f_med_layer_list_ctl_tree;
};

struct cr_array_views * init_cr_array_views(struct f_ctl_cr_array *f_cr_array);
void dealloc_cr_array_views(struct cr_array_views *cr_array_vws);


struct f_platform_control * init_f_platform_control(void *(*c_load_self)(void *f_parent), void *f_parent);
void dealloc_f_ctl_chara_array(struct f_ctl_chara_array *f_carray);

void cb_chara_ctl_item(GtkEntry *entry, gpointer data);
void cb_check_toggle(GtkWidget *widget, gpointer iflag_ptr);

GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window);

struct f_MHD_sph_resolution_control * init_f_MHD_sph_resolution_control(void *(*c_load_self)(void *f_parent), 
																		void *f_parent);
GtkWidget * draw_sph_resolution_vbox(struct f_MHD_sph_resolution_control *f_spctl,
									 struct f_MHD_sph_resolution_views *f_spctl_vws, GtkWidget *window);


#endif    /* CTL_DATA_PLATFORMS_GTK_ */
