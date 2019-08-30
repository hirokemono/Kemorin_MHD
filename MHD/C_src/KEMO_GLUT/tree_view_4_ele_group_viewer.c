/*
//  tree_view_4_ele_group_viewer.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_ele_group_viewer.h"


static void toggle_draw_ele_grp_patch_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	int index1_for_toggle;
	int index_grp = toggle_draw_patch_switch(path_str, user_data, &index1_for_toggle);
    kemoview_set_draw_elegrp_patch(index1_for_toggle, index_grp);
}

static void toggle_draw_ele_grp_grid_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	int index2_for_toggle;
	int index_grp = toggle_draw_grid_switch(path_str, user_data, &index2_for_toggle);
    kemoview_set_draw_elegrp_grid(index2_for_toggle, index_grp);
}

static void toggle_draw_ele_grp_node_switch(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
	int index3_for_toggle;
	int index_grp = toggle_draw_node_switch(path_str, user_data, &index3_for_toggle);
    kemoview_set_draw_elegrp_node(index3_for_toggle, index_grp);
}

static void create_element_group_columns(struct ci3_clist_view *ele_grp_vws)
{
    GtkCellRenderer *textRenderer1;
    GtkCellRenderer *textRenderer2;
    GtkCellRenderer *toggleRenderer1;
    GtkCellRenderer *toggleRenderer2;
    GtkCellRenderer *toggleRenderer3;
	
	GtkTreeViewColumn *column_1st;
    GtkTreeViewColumn *column_2nd;
    GtkTreeViewColumn *column_3rd;
    GtkTreeViewColumn *column_4th;
    GtkTreeViewColumn *column_5th;
	
    /* First raw */
	column_1st = create_each_column_no_sort(ele_grp_vws->tree_view, 
				"Index", COLUMN_MESH_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_MESH_INDEX);
    
    /* Second row */
	column_2nd = create_each_column_no_sort(ele_grp_vws->tree_view, 
				"Group name", COLUMN_MESH_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_MESH_NAME);
   
    /* Third row */
	column_3rd = create_each_column_no_sort(ele_grp_vws->tree_view,
				"Patch", COLUMN_MESH_THIRD);
	toggleRenderer1 = create_each_toggle_renderer(column_3rd, 60, COLUMN_MESH_THIRD);
	g_signal_connect(G_OBJECT(toggleRenderer1), "toggled", 
				G_CALLBACK(toggle_draw_ele_grp_patch_switch), (gpointer) ele_grp_vws);
    
    /* Forth row */
	column_4th = create_each_column_no_sort(ele_grp_vws->tree_view,
				"Grid", COLUMN_MESH_FORTH);
	toggleRenderer2 = create_each_toggle_renderer(column_4th, 60, COLUMN_MESH_FORTH);
	g_signal_connect(G_OBJECT(toggleRenderer2), "toggled",
				G_CALLBACK(toggle_draw_ele_grp_grid_switch), (gpointer) ele_grp_vws);
	
    /* Fifth row */
	column_5th = create_each_column_no_sort(ele_grp_vws->tree_view,
				"Node", COLUMN_MESH_FIFTH);
	toggleRenderer3 = create_each_toggle_renderer(column_5th, 60, COLUMN_MESH_FIFTH);
	g_signal_connect(G_OBJECT(toggleRenderer3), "toggled",
				G_CALLBACK(toggle_draw_ele_grp_node_switch), (gpointer) ele_grp_vws);
};

static void create_ele_group_view(struct ci3_clist_view *ele_grp_vws)
{
    int i;
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(5, G_TYPE_INT, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    ele_grp_vws->tree_view = gtk_tree_view_new();
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(ele_grp_vws->tree_view), model);
	
	create_element_group_columns(ele_grp_vws);
    
    /* Mode selection */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(ele_grp_vws->tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(ele_grp_vws->tree_view), COLUMN_MESH_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_MESH_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<count_chara_int3_clist(ele_grp_vws->ci3_clist_gtk);i++){
		append_grp_model_data(i, ele_grp_vws, child_model);
    }
    
}

void add_ele_group_draw_box(struct ci3_clist_view *ele_grp_vws, GtkWidget *vbox)
{
	GtkWidget *scrolled_window;
	
	GtkWidget *vbox_iso, *hbox_iso;
	GtkWidget *expander_iso,  *scroll_iso, *Frame_iso;
	
	create_ele_group_view(ele_grp_vws);
	
	/* Delete data bottun */
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_window), ele_grp_vws->tree_view);
	
	/* Set signals for sorting */
	vbox_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_iso), scrolled_window, TRUE, TRUE, 0);
	add_sorting_signal_w_label(GTK_TREE_VIEW(ele_grp_vws->tree_view), vbox_iso);
	
	Frame_iso = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_iso), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_iso), vbox_iso);
	
	hbox_iso = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_iso), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_iso), Frame_iso, TRUE, TRUE, 0);
	
	scroll_iso = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_iso),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll_iso, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll_iso), hbox_iso);
	
	expander_iso = gtk_expander_new_with_mnemonic("Element group");
	gtk_container_add(GTK_CONTAINER(expander_iso), scroll_iso);
	
	gtk_box_pack_start(GTK_BOX(vbox), expander_iso, TRUE, FALSE, 0);
};
