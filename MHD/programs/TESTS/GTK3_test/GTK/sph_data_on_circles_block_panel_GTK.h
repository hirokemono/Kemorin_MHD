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
#include "c_ctl_data_sph_monitor_arrays.h"

struct sph_d_circle_widgets{
	struct void_clist * expand_dcirc_list;
	GtkWidget *vbox_dcirc_items;
	GtkWidget *d_circ_tree_view;
    GtkWidget *vbox_dcirc;
};

/* prototypes */

GtkWidget * draw_sph_d_circle_ctl_vbox(struct void_clist *f_circ_ctls,
									   struct sph_d_circle_widgets *dcirc_Widgets,
									   GtkWidget *window);

#endif /* SPH_DATA_ON_CIRCLES_BLOCK_PANEL_GTK_H_ */
