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
#include "c_ctl_data_4_sph_monitor.h"

/* prototypes */

GtkWidget * draw_sph_each_fld_on_circle_ctl_vbox(struct f_sph_field_on_circle_ctls *f_circ_ctls, GtkWidget *window);

GtkWidget * add_fld_on_circle_ctl_vbox(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out);

#endif /* SPH_DATA_ON_CIRCLES_BLOCK_PANEL_GTK_H_ */
