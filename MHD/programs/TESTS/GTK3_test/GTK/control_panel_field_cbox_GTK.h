/*
//  control_panel_field_cbox_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#ifndef CONTROL_PANEL_FIELD_CBOX_GTK_H_
#define CONTROL_PANEL_FIELD_CBOX_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_ctl_data_4_fields_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "control_boxes_single_items_GTK.h"

/* prototypes */

GtkWidget *create_field_label_tree(struct chara_int2_clist *field_list);
GtkWidget *create_compoonent_label_tree(struct chara2_int_clist *dir_list);

GtkWidget *draw_field_combobox_hbox(struct chara_int2_clist *field_list,
                                    struct chara_ctl_item *f_citem, GtkWidget *window);
GtkWidget *draw_component_combobox_hbox(struct chara2_int_clist *dir_list,
                                        struct chara_ctl_item *f_citem, GtkWidget *window);


#endif /* CONTROL_PANEL_FIELD_CBOX_GTK_H_ */
