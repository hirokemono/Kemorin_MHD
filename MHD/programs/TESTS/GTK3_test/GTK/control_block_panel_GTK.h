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
#include "c_ctl_data_sph_monitor_arrays.h"

struct sph_vspectr_widgets{
	struct void_clist * expand_v_pwr_list;
	GtkWidget *vbox_vpwr_items;
	GtkWidget *v_pwr_tree_view;
    GtkWidget *vbox_vpwr;
};

/* prototypes */

GtkWidget * draw_sph_vol_spectr_ctl_vbox(struct void_clist *f_v_pwr, 
										 struct sph_vspectr_widgets *vpwr_Widgets,
										 GtkWidget *window);

#endif /* CONTROL_BLOCK_PANEL_GTK_H_ */
