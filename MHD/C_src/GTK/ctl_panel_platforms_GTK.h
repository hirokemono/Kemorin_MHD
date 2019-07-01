/*
//  ctl_panel_platforms_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/
#ifndef ctl_panel_platforms_GTK__
#define ctl_panel_platforms_GTK__

#include <gtk/gtk.h>
#include "t_ctl_data_4_platforms_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "control_elements_IO_GTK.h"

/* prototypes */

GtkWidget * make_platoform_hbox(const char *label_hd, struct platform_data_control_c *files_c);

#endif /* ctl_panel_platforms_GTK__ */
