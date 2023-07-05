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
#include "c_ctl_data_4_sph_monitor.h"

/* prototypes */

GtkWidget * draw_sph_each_vspec_ctl_vbox(struct f_sph_vol_spectr_ctls *f_v_pwr_item, GtkWidget *window);

GtkWidget * add_block_list_box_w_addbottun(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out);

#endif /* CONTROL_BLOCK_PANEL_GTK_H_ */
