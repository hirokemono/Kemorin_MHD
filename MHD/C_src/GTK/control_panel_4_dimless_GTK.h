/*
//  control_panel_4_dimless_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef control_panel_4_dimless_GTK_h_
#define control_panel_4_dimless_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_ctl_data_4_fields_c.h"
#include "tree_view_4_force_GTK.h"

/* prototypes */

void add_dimless_selection_box(struct dimless_views *dless_vws, GtkWidget *vbox);
void add_dimless_combobox_vbox(struct dimless_views *dless_vws, GtkWidget *vbox);


#endif /* control_panel_4_dimless_GTK_h */
