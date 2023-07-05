/*
//  sph_data_on_circles_block_panel_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef SPH_DATA_ON_CIRCLES_BLOCK_PANEL_GTK_H_
#define SPH_DATA_ON_CIRCLES_BLOCK_PANEL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_c_lists.h"
#include "t_ctl_array_single_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_block_GTK.h"
#include "t_ctl_array_single_items_c.h"
#include "kemoview_gtk_routines.h"
#include "control_boxes_single_items_GTK.h"

struct f_sph_field_on_circle_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_circle_field_file_ctl;
	struct f_ctl_chara_item *f_circle_spectr_file_ctl;
	struct f_ctl_chara_item *f_circle_file_format_ctl;
	struct f_ctl_chara_item *f_pick_circle_coord_ctl;\
	
	struct f_ctl_int_item   *f_nphi_mid_eq_ctl;
	struct f_ctl_real_item  *f_pick_s_ctl;
	struct f_ctl_real_item  *f_pick_z_ctl;
};

extern void * c_data_on_circles_block_name(void *f_circ_ctls);
extern int    c_data_on_circles_num(void *f_circ_ctls);
extern void * c_data_on_circles_meq_ctl(int idx, void *f_circ_ctls);
extern void * c_append_circles_meq_ctl(int idx, char *c_name, void *f_circ_ctls);
extern void * c_delete_circles_meq_ctl(int idx, void *f_circ_ctls);

extern void * c_data_on_circle_block_name(void *f_circ_ctls);
extern void * c_data_on_circle_iflag(void *f_circ_ctls);
extern void * c_data_on_circle_field_file(void *f_circ_ctls);
extern void * c_data_on_circle_spectr_file(void *f_circ_ctls);
extern void * c_data_on_circle_file_fmt_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_coord_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_nphi_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_pick_s_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_pick_z_ctl(void *f_circ_ctls);

/* prototypes */

struct f_sph_field_on_circle_ctls * init_f_sph_field_on_circle_ctls(int idx, void *f_parent);
void * dealloc_f_sph_field_on_circle_ctls(void *f_item);

GtkWidget * draw_sph_each_fld_on_circle_ctl_vbox(struct f_sph_field_on_circle_ctls *f_circ_ctls, GtkWidget *window);

GtkWidget * add_fld_on_circle_ctl_vbox(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out);

#endif /* SPH_DATA_ON_CIRCLES_BLOCK_PANEL_GTK_H_ */
