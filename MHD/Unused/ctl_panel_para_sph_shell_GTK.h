/*
//  ctl_panel_para_sph_shell_GTK.h
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#ifndef ctl_panel_para_sph_shell_GTK__
#define ctl_panel_para_sph_shell_GTK__

#include "calypso_GTK.h"
#include "t_control_data_sph_grid_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "control_elements_IO_GTK.h"

/* prototypes */

GtkWidget *make_parallel_shell_hbox(const char *label_hd, char *shell_ctl_file_name, 
			struct parallel_sph_shell_control_c *shell_ctl, GtkWidget *save_bottun);

#endif /* ctl_panel_para_sph_shell_GTK__ */
