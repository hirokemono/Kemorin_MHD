/*
//  tree_view_4_viewer_mesh.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_VIEWER_MESH_
#define TREE_VIEW_4_VIEWER_MESH_

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "t_control_chara_int_IO.h"
#include "t_control_chara_int3_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"
#include "tree_view_chara_int3_GTK.h"
#include "quicksort_c.h"

#define COLUMN_MESH_INDEX    0
#define COLUMN_MESH_NAME     1
#define COLUMN_MESH_THIRD    2
#define COLUMN_MESH_FORTH    3
#define COLUMN_MESH_FIFTH    4


struct ci_clist_view{
    int index;
    GtkWidget *tree_view;
    
    struct chara_int_clist *ci_clist_gtk;
};

struct ci3_clist_view{
    int index;
    GtkWidget *tree_view;
    
    struct chara_int3_clist *ci3_clist_gtk;
};

struct group_gtk_menu{
	struct ci3_clist_view *group_vws;

    GtkWidget *combobox_patch_color;
	GtkWidget *combobox_grid_color;
	GtkWidget *combobox_node_color;
	GtkWidget *button_patch_color;
	GtkWidget *button_grid_color;
	GtkWidget *button_node_color;
	GtkWidget *spin_opacity;
	
	GdkRGBA gcolor;
};

struct nod_grp_gtk_menu{
	struct ci_clist_view *nod_grp_vws;
	
	GtkWidget *combobox_node_color;
	GtkWidget *button_node_color;
	GdkRGBA gcolor;
};

struct kemoview_mesh_view{
	struct group_gtk_menu *domain_group_gmenu;
	struct nod_grp_gtk_menu  *node_group_gmenu;
	struct group_gtk_menu *ele_group_gmenu;
	struct group_gtk_menu *surf_group_gmenu;
	
	GtkWidget *spin_dist;
	GtkWidget *spin_node_size, *spin_node_digit;
	GtkWidget *spin_num_loop;
	GtkWidget *combobox_color_mode;
};

/* prototypes */

void init_mesh_views_4_viewer(struct kemoview_mesh_view *mesh_vws);
void dealloc_mesh_views_4_viewer(struct kemoview_mesh_view *mesh_vws);

void append_grp_model_data(int index, struct ci3_clist_view *grp_vws, 
			GtkListStore *child_model);
void append_node_grp_model_data(int index, struct ci_clist_view *nod_grp_vws,
			GtkListStore *child_model);

int toggle_draw_patch_switch(gchar *path_str, gpointer user_data, 
			int *index1_for_toggle);
int toggle_draw_grid_switch(gchar *path_str, gpointer user_data, 
			int *index2_for_toggle);
int toggle_draw_node_switch(gchar *path_str, gpointer user_data, 
			int *index3_for_toggle);
int toggle_draw_nod_grp_node_switch(gchar *path_str, gpointer user_data, 
			int *index1_for_toggle);

int set_all_draw_flags(int iflag, int iflag_column, gpointer user_data);
int set_all_node_draw_flags(int iflag, gpointer user_data);

#endif /* tree_view_4_pvr_colormap_h_ */
