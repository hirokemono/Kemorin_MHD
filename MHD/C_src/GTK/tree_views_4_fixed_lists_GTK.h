/*
//  tree_views_4_fixed_lists_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef tree_views_4_fixed_lists_GTK_h_
#define tree_views_4_fixed_lists_GTK_h_

#include "calypso_GTK.h"
#include "t_control_label_from_f.h"
#include "m_direction_labels_c.h"

void set_last_field_to_label(GtkTreeSelection *selection, gpointer user_data);
void block_changed_signal(GObject *instance);
void unblock_changed_signal(GObject *instance);

void add_sorting_signal_w_label(GtkTreeView *tree_view, GtkWidget *hbox);



GtkWidget * create_fixed_label_w_math_tree(void);
GtkWidget * create_fixed_label_w_index_tree(void);
void create_fixed_constant_tree(GtkWidget *label_tree);

void append_vector_componnet_label(struct flag_with_math_f *scalar_components_flag, GtkWidget *label_tree);
void append_sym_tensor_componnet_label(GtkWidget *label_tree);

void append_xyz_componnet_label(GtkWidget *label_tree);
void append_surface_equation_label(GtkWidget *label_tree);

void append_force_label(GtkWidget *label_tree);
void append_basic_force_label(GtkWidget *label_tree);
void append_gravity_type_label(GtkWidget *label_tree);

#endif /* tree_views_4_fixed_lists_GTK_h_ */
