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

struct field_views{
    GtkWidget *used_tree_view;
    GtkWidget *unused_field_tree_view;
    
    struct field_ctl_c *fld_ctl_gtk;
	struct all_field_ctl_c **all_fld_ctl;
};

/* prototypes */

void init_field_views_GTK(struct field_ctl_c *fld_ctl_ref, struct field_views *fields_vws);
void dealloc_field_views_GTK(struct field_views *fields_vws);

void add_field_selection_box(struct field_views *fields_vws, GtkWidget *vbox);
void add_field_combobox_vbox(struct field_views *fields_vws, GtkWidget *vbox);

void create_field_tree_view(struct all_field_ctl_c **all_fld_ctl, 
			struct field_views *fields_vws);
void create_unused_field_tree_view(struct all_field_ctl_c **all_fld_ctl, 
			struct field_views *fields_vws);


#endif /* control_panel_4_field_GTK_h_ */
