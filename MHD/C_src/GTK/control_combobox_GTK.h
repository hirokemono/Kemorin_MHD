/*
//  control_combobox_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "calypso_GTK.h"
#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_chara_IO.h"
#include "t_control_label_from_f.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_GTK.h"

#ifndef CONTROL_COMBOBOX_GTK_
#define CONTROL_COMBOBOX_GTK_



/*  prototype */

GtkWidget * create_fixed_label_tree(struct chara_clist *c1_clist);
GtkWidget * create_control_flags_tree_view(struct control_labels_f *flag_list);
void add_control_combobox_vbox_old(char *c_charavalue,
                               struct control_labels_f *flag_list,
							   GtkWidget *ctl_flags_tree_view, GtkWidget *vbox_out);

void add_control_combobox_vbox(struct chara_ctl_item *f_citem,
                               struct control_labels_f *flag_list,
							   GtkWidget *ctl_flags_tree_view, GtkWidget *vbox_out);


#endif    /* T_CONTROL_LABEL_FROM_F_ */
