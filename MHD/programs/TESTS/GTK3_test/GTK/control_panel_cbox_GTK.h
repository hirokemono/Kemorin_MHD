/*
//  control_panel_cbox_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_CBOX_GTK_H_
#define CONTROL_PANEL_CBOX_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_GTK.h"
#include "control_combobox_GTK.h"

/* prototypes */

GtkWidget * add_c_list_combobox(struct chara_clist *c_clist_gtk,
                                struct chara_clist *input_list, 
                                GtkWidget *c_tree_view);


GtkWidget * c_list_combobox_expander(struct chara_clist *c_clist_gtk, 
                                     struct chara_clist *input_list, 
                                     GtkWidget *clist_tree_view,
                                     GtkWidget *items_tree_view,
                                     GtkWidget *window);

#endif /* CONTROL_PANEL_CBOX_GTK_H_ */
