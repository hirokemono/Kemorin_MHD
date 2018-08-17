/*
//  tree_view_chara2_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#ifndef tree_view_chara2_real_GTK_h_
#define tree_view_chara2_real_GTK_h_

#include <stdio.h>

#include <stdlib.h>
#include <gtk/gtk.h>

#include "t_control_chara2_real_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_real_GTK.h"

void set_last_field_to_label(GtkTreeSelection *selection, gpointer user_data);


void append_combobox_item_to_tree(char *c_tbl, GtkTreeModel *child_model);
int append_c2r_item_to_tree(int index, char *c1_tbl, char *c2_tbl, double r_data, 
                           GtkTreeModel *child_model);
int append_c2r_list_from_ctl(int index, struct chara2_real_ctl_list *head, 
                            GtkTreeView *c2r_tree_view);

void c2r_tree_name_edited(gchar *path_str, gchar *new_text,
                         GtkTreeView *c2r_tree_view, struct chara2_real_ctl_list *c2r_list_head);
void c2r_tree_type_edited(gchar *path_str, gchar *new_text,
                         GtkTreeView *c2r_tree_view, struct chara2_real_ctl_list *c2r_list_head);
void c2r_tree_value_edited(gchar *path_str, gchar *new_text, 
                          GtkTreeView *c2r_tree_view, struct chara2_real_ctl_list *c2r_list_head);
int add_c2r_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                              struct chara2_real_ctl_list *c2r_list_head);
int add_c2r_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
                                  GtkTreeView *tree_view_to_add, struct chara2_real_ctl_list *c2r_list_head);
int add_c2r_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
                                        GtkTreeView *tree_view_to_add, struct chara2_real_ctl_list *c2r_list_head);
void delete_c2r_list_items_GTK(GtkTreeView *tree_view_to_del,
                              struct chara2_real_ctl_list *c2r_list_head);


void create_text2_real_tree_view(GtkListStore *cbox_child_model, GtkTreeView *c2r_tree_view,
                                 GtkCellRenderer *renderer_text, GtkCellRenderer *renderer_cbox, 
                                 GtkCellRenderer *renderer_spin);
void create_cbox_text_real_tree_view(GtkListStore *cbox_child_model, GtkTreeView *c2r_tree_view,
                                 GtkCellRenderer *renderer_cbox, GtkCellRenderer *renderer_text, 
                                 GtkCellRenderer *renderer_spin);

void add_chara2_real_list_box_w_addbottun(GtkTreeView *c2r_tree_view, 
                                         GtkWidget *button_add, GtkWidget *button_delete, 
                                         GtkWidget *vbox);

void add_chara2_real_list_box_w_combobox(GtkTreeView *c2r_tree_view, 
                                        GtkWidget *combobox_add, GtkWidget *button_delete, 
                                        GtkWidget *vbox);

#endif /* tree_view_chara2_real_GTK_h_ */
