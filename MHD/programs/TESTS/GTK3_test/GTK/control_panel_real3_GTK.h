/*
//  control_panel_real3_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_REAL3_GTK_H_
#define CONTROL_PANEL_REAL3_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_real3_IO.h"
#include "t_ctl_array_real3_items_c.h"
#include "tree_view_real3_GTK.h"
#include "control_combobox_GTK.h"

/* prototypes */

GtkWidget * r3_list_combobox_expander(struct real3_clist *ctl_clist,
                                      GtkWidget *clist_tree_view, GtkWidget *window);

#endif /* CONTROL_PANEL_REAL3_GTK_H_ */
