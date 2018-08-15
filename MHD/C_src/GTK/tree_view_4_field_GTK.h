/*
//  tree_view_4_field_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#ifndef tree_view_4_field_GTK_h_
#define tree_view_4_field_GTK_h_

#include <stdlib.h>
#include <gtk/gtk.h>

#include "t_ctl_data_4_fields_c.h"
#include "tree_views_4_fixed_lists_GTK.h"

#define COLUMN_FIELD_INDEX    0
#define COLUMN_FIELD_NAME     1
#define COLUMN_FIELD_MATH     2
#define COLUMN_VIZ_FLAG       3
#define COLUMN_MONITOR_FLAG   4
#define COLUMN_NUM_COMP       5
#define COLUMN_QUADRATURE     6

struct field_views{
    GtkWidget *used_tree_view;
    GtkWidget *unused_field_tree_view;
    
    GtkWidget *scalar_label_view;
    GtkWidget *vector_label_view;
    GtkWidget *sym_tensor_label_view;
    GtkWidget *xyz_dir_label_view;
    GtkWidget *surface_eq_view;
	
	struct field_ctl_c *fld_ctl_gtk;
    struct all_field_ctl_c **all_fld_tbl;
};

/* prototypes */

void init_field_views_GTK(struct field_ctl_c *fld_ctl_ref, struct field_views *fields_vws);
void dealloc_field_views_GTK(struct field_views *fields_vws);

void append_model_data(int index_field, struct all_field_ctl_c **all_fld_tbl, GtkTreeModel *child_model);

void create_field_tree_view(struct all_field_ctl_c **all_fld_tbl, 
                            struct field_views *fields_vws);
void create_unused_field_tree_view(struct all_field_ctl_c **all_fld_tbl, 
                                   struct field_views *fields_vws);
void create_direction_label_tree(GtkWidget *label_tree);

void create_direction_tree_views(struct field_views *fields_vws);

#endif /* tree_view_4_field_GTK_h_ */
