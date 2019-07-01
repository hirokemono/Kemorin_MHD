/*
//  ctl_panel_SPH_MHD_model_GTK.h
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#ifndef ctl_panel_SPH_MHD_model_GTK__
#define ctl_panel_SPH_MHD_model_GTK__

#include <gtk/gtk.h>
#include "t_control_data_sph_grid_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "control_elements_IO_GTK.h"

/* prototypes */

void expander_switch_cb(GObject *switch_3, GParamSpec *pspec, gpointer data);
void expander_action_cb(GObject *switch_3, gpointer data);
void add_control_block_box(char *c_label, int *iflag_box, 
			GtkWidget *hbox, GtkWidget *expander_b);

GtkWidget * make_parallel_shell_hbox(struct parallel_sph_shell_control_c *shell_ctl);

#endif /* ctl_panel_SPH_MHD_model_GTK__ */
