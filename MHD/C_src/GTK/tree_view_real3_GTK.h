/*
//  tree_view_real3_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/25.
*/

#ifndef tree_view_real3_GTK_h_
#define tree_view_real3_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_real3_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

struct r3_clist_view{
    int index_bc;
    GtkWidget *tree_view;
    
    struct real3_clist *r3_clist_gtk;
};

/* prototypes */

void init_r3_clist_views(struct real3_clist *r3_clist, struct r3_clist_view *cmap_vws);
int append_r3_item_to_tree(int index, double r1_data, double r2_data, double r3_data, 
                           GtkTreeModel *child_model);
int append_r3_list_from_ctl(int index, struct real3_clist *r3_clist,
                            GtkTreeView *r3_tree_view);


void r3_tree_value1_edited(gchar *path_str, gchar *new_text,
                           GtkTreeView *r3_tree_view, struct real3_clist *r3_clist);
void r3_tree_value2_edited(gchar *path_str, gchar *new_text,
                           GtkTreeView *r3_tree_view, struct real3_clist *r3_clist);
void r3_tree_value3_edited(gchar *path_str, gchar *new_text,
                           GtkTreeView *r3_tree_view, struct real3_clist *r3_clist);

GList * set_selected_r3_list_items(GtkTreeView *r3_tree_view, double org_value[3]);
int add_r3_list_items(GtkTreeView *r3_tree_view,
                      struct real3_clist *r3_clist);
void delete_r3_list_items(GtkTreeView *r3_tree_view, struct real3_clist *r3_clist);

void create_real3_tree_view(GtkTreeView *r3_tree_view, struct real3_clist *r3_clist, 
                            GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2,
                            GtkCellRenderer *renderer_spin3);
void add_real3_list_box(GtkTreeView *r3_tree_view, struct real3_clist *r3_clist, 
                        GtkWidget *button_add, GtkWidget *button_delete, GtkWidget *vbox);


#endif /* tree_view_real3_GTK_h_ */
