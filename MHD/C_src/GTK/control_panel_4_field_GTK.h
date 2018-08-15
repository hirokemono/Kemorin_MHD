/*
//  control_panel_4_field_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#ifndef control_panel_4_field_GTK_h_
#define control_panel_4_field_GTK_h_

#include <stdlib.h>
#include <gtk/gtk.h>

#include "t_ctl_data_4_fields_c.h"
#include "tree_view_4_field_GTK.h"

/* prototypes */

void add_field_selection_box(struct field_views *fields_vws, GtkWidget *vbox);
void add_field_combobox_vbox(struct field_views *fields_vws, GtkWidget *vbox);


#endif /* control_panel_4_field_GTK_h_ */
