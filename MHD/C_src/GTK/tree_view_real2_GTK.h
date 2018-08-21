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

struct r2_clist_view{
    int index_bc;
    GtkTreeView *tree_view;
    
    struct real2_clist *r2_clist_gtk;
};

/* prototypes */

void init_r2_clist_views(struct real2_clist *r2_clist, struct r2_clist_view *cmap_vws);

int append_r2_item_to_tree(int index, double r1_data, double r2_data, 
                           GtkTreeModel *child_model);

void create_real2_tree_view(struct r2_clist_view *r2_vws);
void add_real2_list_box_w_addbottun(struct r2_clist_view *r2_vws, GtkWidget *vbox);


#endif /* tree_view_real2_GTK_h_ */
