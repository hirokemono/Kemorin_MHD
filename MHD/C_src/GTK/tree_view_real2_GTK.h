/*
//  tree_view_real2_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef tree_view_real2_GTK_h_
#define tree_view_real2_GTK_h_

#include <stdlib.h>
#include <gtk/gtk.h>

#include "t_control_real2_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

void set_last_field_to_label(GtkTreeSelection *selection, gpointer user_data);


int append_r2_item_to_tree(int index, char *c_tbl, char *c_math, double r_data, 
                           GtkTreeModel *child_model);
int append_r2_list_from_ctl(int index, struct chara_real_ctl_list *head, 
                            GtkTreeView *r2_tree_view);

void cr_tree_value_edited(gchar *path_str, gchar *new_text, 
                          GtkTreeView *r2_tree_view, struct chara_real_clist *cr_clist);
int add_r2_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                              struct chara_real_clist *cr_clist);
void delete_r2_list_items_GTK(GtkTreeView *tree_view_to_del,
                              struct chara_real_clist *cr_clist);


void create_real2_tree_view(GtkTreeView *r2_tree_view,
                                GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2);

void add_real2_list_box_w_addbottun(GtkTreeView *r2_tree_view, 
                                         GtkWidget *button_add, GtkWidget *button_delete, 
                                         GtkWidget *vbox);


#endif /* tree_view_real2_GTK_h_ */
