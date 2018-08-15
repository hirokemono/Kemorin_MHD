/*
//  tree_views_4_fixed_lists_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef tree_views_4_fixed_lists_GTK_h_
#define tree_views_4_fixed_lists_GTK_h_

#include <gtk/gtk.h>

#include "m_direction_labels_c.h"

#define COLUMN_FIELD_INDEX    0
#define COLUMN_FIELD_NAME     1
#define COLUMN_FIELD_MATH     2

void create_fixed_label_tree(GtkWidget *label_tree);

void append_scalar_componnet_label(GtkWidget *label_tree);
void append_vector_componnet_label(GtkWidget *label_tree);
void append_sym_tensor_componnet_label(GtkWidget *label_tree);

void append_xyz_componnet_label(GtkWidget *label_tree);
void append_surface_equation_label(GtkWidget *label_tree);

void append_force_label(GtkWidget *label_tree);
void append_basic_force_label(GtkWidget *label_tree);

void append_defaule_coefs_label(GtkWidget *label_tree);
void append_gravity_type_label(GtkWidget *label_tree);

#endif /* tree_views_4_fixed_lists_GTK_h_ */
