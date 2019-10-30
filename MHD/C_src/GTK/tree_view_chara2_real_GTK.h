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

#include "calypso_GTK.h"
#include "t_control_chara2_real_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"


int append_c2r_item_to_tree(const int index, const char *c1_tbl, const char *c2_tbl, const double r_data, 
                           GtkTreeModel *c2r_tree_model);
int append_c2r_list_from_ctl(int index, struct chara2_real_ctl_list *head, 
                            GtkTreeView *c2r_tree_view);

void c2r_tree_1st_text_edited(gchar *path_str, gchar *new_text,
                         GtkTreeView *c2r_tree_view, struct chara2_real_clist *c2r_clst);
void c2r_tree_2nd_text_edited(gchar *path_str, gchar *new_text,
                         GtkTreeView *c2r_tree_view, struct chara2_real_clist *c2r_clst);
void c2r_tree_value_edited(gchar *path_str, gchar *new_text, 
                          GtkTreeView *c2r_tree_view, struct chara2_real_clist *c2r_clst);
int add_c2r_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                              struct chara2_real_clist *c2r_clst);
int add_c2r_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
                                  GtkTreeView *tree_view_to_add, struct chara2_real_clist *c2r_clst);
int add_c2r_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
                                        GtkTreeView *tree_view_to_add, struct chara2_real_clist *c2r_clst);
void delete_c2r_list_items_GTK(GtkTreeView *tree_view_to_del,
                              struct chara2_real_clist *c2r_clst);


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
