/*
//  control_panel_4_sph_monitor_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef CONTROL_PANEL_4_SPH_MONITOR_GTK_H_
#define CONTROL_PANEL_4_SPH_MONITOR_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "c_ctl_data_4_sph_monitor.h"
#include "control_panel_real_GTK.h"
#include "control_panel_int_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_block_panel_GTK.h"
#include "sph_data_on_circles_block_panel_GTK.h"

struct f_sph_monitor_widgets{
	struct sph_d_circle_widgets *dcirc_Widgets;
	struct sph_vspectr_widgets * vpwr_Widgets;
	
	GtkWidget *f_pick_radius_ctl_tree;
	GtkWidget *f_idx_pick_layer_ctl_tree;
	GtkWidget *f_idx_pick_sph_ctl_tree;
	GtkWidget *f_idx_pick_sph_l_ctl_tree;
	GtkWidget *f_idx_pick_sph_m_ctl_tree;
	
	GtkWidget *f_idx_gauss_ctl_tree;
	GtkWidget *f_idx_gauss_l_ctl_tree;
	GtkWidget *f_idx_gauss_m_ctl_tree;
	
	GtkWidget *f_layer_radius_ctl_tree;
	GtkWidget *f_idx_spec_layer_ctl_tree;
	
	GtkWidget *f_fdip_truncation_ctl_tree;
};


/* prototypes */

GtkWidget * draw_MHD_sph_monitor_ctls_vbox(struct f_MHD_sph_monitor_ctls *f_smonitor_ctl,
										   struct f_sph_monitor_widgets *f_lp_vws, 
										   GtkWidget *window);


#endif /* CONTROL_PANEL_4_SPH_MONITOR_GTK_H_ */
