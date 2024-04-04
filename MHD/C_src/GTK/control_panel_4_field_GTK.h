/*
//  control_panel_4_field_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#ifndef control_panel_4_field_GTK_h_
#define control_panel_4_field_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "skip_comment_c.h"
#include "kemoview_gtk_routines.h"
#include "t_ctl_data_4_fields_c.h"
#include "tree_view_4_field_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_ctl_array_chara3_items_c.h"

/* prototypes */

void add_field_selection_box(struct field_views *fields_vws, 
                             GtkWidget *window, GtkWidget *vbox_out);
void add_field_combobox_vbox(struct field_views *fields_vws, GtkWidget *vbox_out);
void add_all_field_combobox_vbox(char *field_ctl_label, char *comp_ctl_label, 
                                 struct field_views *fields_vws, GtkWidget *vbox_out);


#endif /* control_panel_4_field_GTK_h_ */
