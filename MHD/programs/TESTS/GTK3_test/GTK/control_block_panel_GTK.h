/*
//  control_block_panel_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_BLOCK_PANEL_GTK_H_
#define CONTROL_BLOCK_PANEL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_c_lists.h"
#include "t_ctl_array_single_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_block_GTK.h"
#include "t_ctl_array_single_items_c.h"
#include "kemoview_gtk_routines.h"
#include "control_boxes_single_items_GTK.h"

struct f_sph_vol_spectr_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_volume_spec_file_ctl;
	struct f_ctl_chara_item *f_volume_ave_file_ctl;
	struct f_ctl_chara_item *f_volume_spec_format_ctl;
	struct f_ctl_chara_item *f_degree_v_spectra_switch;
	struct f_ctl_chara_item *f_order_v_spectra_switch;
	struct f_ctl_chara_item *f_diff_v_lm_spectra_switch;
	struct f_ctl_chara_item *f_axis_v_power_switch;
	struct f_ctl_real_item *f_inner_radius_ctl;
	struct f_ctl_real_item *f_outer_radius_ctl;
};

extern void * c_sph_monitor_ctl_v_pwr_name(void *f_smonitor_ctl);
extern int    c_sph_monitor_num_vspec_ctl(void *f_smonitor_ctl);
extern void * c_sph_monitor_vspec_ctl(int idx, void *f_smonitor_ctl);
extern void * c_append_sph_mntr_vspec_ctl(int idx, char *block_name,
                                          void *f_smonitor_ctl);
extern void * c_delete_sph_mntr_vspec_ctl(int idx, void *f_smonitor_ctl);

extern void * c_sph_v_spectr_ctl_block_name(void *f_v_pwr_item);
extern void * c_sph_v_spectr_ctl_iflag(void *f_v_pwr_item);
extern void * c_sph_volume_spec_file_ctl(void *f_v_pwr_item);
extern void * c_sph_volume_ave_file_ctl(void *f_v_pwr_item);
extern void * c_sph_volume_spec_format_ctl(void *f_v_pwr_item);
extern void * c_sph_degree_v_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_order_v_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_diff_v_lm_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_axis_v_power_switch(void *f_v_pwr_item);
extern void * c_sph_v_spec_inner_radius_ctl(void *f_v_pwr_item);
extern void * c_sph_v_spec_outer_radius_ctl(void *f_v_pwr_item);

/* prototypes */

struct f_sph_vol_spectr_ctls * init_f_sph_vol_spectr_ctls(int idx, void *f_parent);
void * dealloc_f_sph_vol_spectr_ctls(void *f_item);

GtkWidget * draw_sph_each_vspec_ctl_vbox(struct f_sph_vol_spectr_ctls *f_v_pwr_item, GtkWidget *window);

GtkWidget * add_block_list_box_w_addbottun(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out);

#endif /* CONTROL_BLOCK_PANEL_GTK_H_ */
