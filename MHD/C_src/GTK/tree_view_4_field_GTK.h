/*
//  tree_view_4_field_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#ifndef tree_view_4_field_GTK_h_
#define tree_view_4_field_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "tree_view_chara_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"

struct field_views{
	GtkWidget *used_tree_view;
	GtkWidget **unused_field_tree_view;
	
	GtkWidget *field_group_tree_view;
	GtkWidget **all_field_tree_view;
	
	GtkWidget *scalar_label_view;
	GtkWidget *vector_label_view;
	GtkWidget *sym_tensor_label_view;
//	GtkWidget *xyz_dir_label_view;
//	GtkWidget *surface_eq_view;
	
	struct field_ctl_c *fld_ctl_gtk;
	struct all_field_ctl_c *all_fld_list;

    struct component_flags_f *comp_flags;
    
	int selected_field_id;
	int selected_field_grp_id;
	int selected_component_id;
	struct chara_ctl_item *selected_field_ctl;
	struct chara_ctl_item *selected_component_ctl;
};

/* prototypes */

struct field_views * init_field_views_GTK(struct field_ctl_c *fld_ctl_ref);
void dealloc_field_views_GTK(struct field_views *fields_vws);

void append_field_model_data(int index_field, struct all_field_ctl_c *all_fld_list,
			GtkListStore *child_model);

GtkWidget * create_field_tree_view(struct all_field_ctl_c *all_fld_list, struct field_ctl_c *fld_ctl_gtk);
GtkWidget ** create_unused_field_tree_views(struct all_field_ctl_c *all_fld_list);

GtkWidget ** create_all_field_tree_views(struct all_field_ctl_c *all_fld_list);
GtkWidget * create_field_group_tree_view(struct all_field_ctl_c *all_fld_list);

void create_direction_tree_views(struct field_views *fields_vws);


int find_field_address(const char *field_in, struct field_names_f *fld_list);
int find_field_group(const int i_field, struct field_names_f *fld_list);
int find_comp_address(char *comp_in, int i_field, struct field_names_f *fld_list,
					  struct component_flags_f *comp_flags);
#endif /* tree_view_4_field_GTK_h_ */
