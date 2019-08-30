/*
//  tree_view_4_surf_group_viewer.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_surf_group_viewer.h"

static void toggle_draw_surf_grp_patch_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	int index1_for_toggle;
	int index_grp = toggle_draw_patch_switch(path_str, user_data, &index1_for_toggle);
    kemoview_set_draw_surfgrp_patch(index1_for_toggle, index_grp);
}

static void toggle_draw_surf_grp_grid_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	int index2_for_toggle;
	int index_grp = toggle_draw_grid_switch(path_str, user_data, &index2_for_toggle);
    kemoview_set_draw_surfgrp_grid(index2_for_toggle, index_grp);
}

static void toggle_draw_surf_grp_node_switch(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
	int index3_for_toggle;
	int index_grp = toggle_draw_node_switch(path_str, user_data, &index3_for_toggle);
    kemoview_set_draw_surfgrp_node(index3_for_toggle, index_grp);
}


static void draw_all_sf_grp_patch_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_THIRD, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_surfgrp_patch(IONE, i);
	};
}

static void draw_all_sf_grp_grids_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FORTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_surfgrp_grid(IONE, i);
	};
}

static void draw_all_sf_grp_nodes_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FIFTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_surfgrp_node(IONE, i);
	};
}

static void hide_all_sf_grp_patch_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IZERO, COLUMN_MESH_THIRD, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_surfgrp_patch(IZERO, i);
	};
}

static void hide_all_sf_grp_grids_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FORTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_surfgrp_grid(IZERO, i);
	};
}

static void hide_all_sf_grp_nodes_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IZERO, COLUMN_MESH_FIFTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_surfgrp_node(IZERO, i);
	};
}


static void surf_grp_patch_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_sfcolor);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_sfcolor);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	if (index_mode == SINGLE_COLOR){
//		kemoview_gtk_surfcolorsel(user_data);
		kemoview_set_surf_grp_color_flag(SURFSOLID_TOGGLE, index_mode);
	} else {
		kemoview_set_surf_grp_color_flag(SURFSOLID_TOGGLE, index_mode);
	};
	
//	draw_mesh_w_menu();
	return;
};

static void surf_grp_grid_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_sfcolor);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_sfcolor);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	if (index_mode == SINGLE_COLOR){
//		kemoview_gtk_surfcolorsel(user_data);
		kemoview_set_surf_grp_color_flag(SURFGRID_TOGGLE, index_mode);
	} else {
		kemoview_set_surf_grp_color_flag(SURFGRID_TOGGLE, index_mode);
	};
	
//	draw_mesh_w_menu();
	return;
};

static void surf_grp_node_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_sfcolor);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_sfcolor);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	if (index_mode == SINGLE_COLOR){
//		kemoview_gtk_surfcolorsel(user_data);
		kemoview_set_surf_grp_color_flag(SURFNOD_TOGGLE, index_mode);
	} else {
		kemoview_set_surf_grp_color_flag(SURFNOD_TOGGLE, index_mode);
	};
	
//	draw_mesh_w_menu();
	return;
};

static void set_surf_grp_opacity_CB(GtkWidget *entry, gpointer user_data)
{
	float colorcode4[4];
	kemoview_get_surf_grp_color_code(SURFSOLID_TOGGLE, colorcode4);
	colorcode4[3] = (float) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_surf_grp_color_code(SURFSOLID_TOGGLE, colorcode4);
	return;
}
static void set_single_surf_grp_patch_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent = GTK_WINDOW(user_data);
	
	kemoview_get_surf_grp_color_code(SURFSOLID_TOGGLE, colorcode4);
	int iflag_set = kemoview_gtk_colorsel_CB(parent, colorcode4);
	if(iflag_set > 0) {kemoview_set_surf_grp_color_code(SURFSOLID_TOGGLE, colorcode4);};
	return;
};
static void set_single_surf_grp_grids_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent = GTK_WINDOW(user_data);
	
	kemoview_get_surf_grp_color_code(SURFGRID_TOGGLE, colorcode4);
	int iflag_set = kemoview_gtk_colorsel_CB(parent, colorcode4);
	if(iflag_set > 0) {kemoview_set_surf_grp_color_code(SURFGRID_TOGGLE, colorcode4);};
	return;
};
static void set_single_surf_grp_nodes_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent = GTK_WINDOW(user_data);
	
	kemoview_get_surf_grp_color_code(SURFNOD_TOGGLE, colorcode4);
	int iflag_set = kemoview_gtk_colorsel_CB(parent, colorcode4);
	if(iflag_set > 0) {kemoview_set_surf_grp_color_code(SURFNOD_TOGGLE, colorcode4);};
	return;
};


static void create_surface_group_columns(struct ci3_clist_view *surf_grp_vws)
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
	column_1st = create_each_column_no_sort(surf_grp_vws->tree_view, 
				"Index", COLUMN_MESH_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_MESH_INDEX);
    
    /* Second row */
	column_2nd = create_each_column_no_sort(surf_grp_vws->tree_view, 
				"Group name", COLUMN_MESH_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_MESH_NAME);
   
    /* Third row */
	column_3rd = create_each_column_no_sort(surf_grp_vws->tree_view,
				"Patch", COLUMN_MESH_THIRD);
	toggleRenderer1 = create_each_toggle_renderer(column_3rd, 60, COLUMN_MESH_THIRD);
	g_signal_connect(G_OBJECT(toggleRenderer1), "toggled", 
				G_CALLBACK(toggle_draw_surf_grp_patch_switch), (gpointer) surf_grp_vws);
    
    /* Forth row */
	column_4th = create_each_column_no_sort(surf_grp_vws->tree_view,
				"Grid", COLUMN_MESH_FORTH);
	toggleRenderer2 = create_each_toggle_renderer(column_4th, 60, COLUMN_MESH_FORTH);
	g_signal_connect(G_OBJECT(toggleRenderer2), "toggled",
				G_CALLBACK(toggle_draw_surf_grp_grid_switch), (gpointer) surf_grp_vws);
	
    /* Fifth row */
	column_5th = create_each_column_no_sort(surf_grp_vws->tree_view,
				"Node", COLUMN_MESH_FIFTH);
	toggleRenderer3 = create_each_toggle_renderer(column_5th, 60, COLUMN_MESH_FIFTH);
	g_signal_connect(G_OBJECT(toggleRenderer3), "toggled",
				G_CALLBACK(toggle_draw_surf_grp_node_switch), (gpointer) surf_grp_vws);
};

static void create_surface_group_view(struct ci3_clist_view *surf_grp_vws)
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
    surf_grp_vws->tree_view = gtk_tree_view_new();
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(surf_grp_vws->tree_view), model);
	
	create_surface_group_columns(surf_grp_vws);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(surf_grp_vws->tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(surf_grp_vws->tree_view), COLUMN_MESH_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_MESH_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<count_chara_int3_clist(surf_grp_vws->ci3_clist_gtk);i++){
		append_grp_model_data(i, surf_grp_vws, child_model);
    }
    
}

void add_surf_group_draw_box(struct ci3_clist_view *surf_grp_vws, 
			GtkWidget *window_mesh, GtkWidget *vbox)
{
	GtkWidget *scrolled_table;
	
	GtkWidget *button_draw_patch, *button_draw_grid, *button_draw_node;
	GtkWidget *button_hide_patch, *button_hide_grid, *button_hide_node;
	GtkWidget *hbox_draw, *hbox_hide;
	
	GtkWidget *hbox_one_opacity, *hbox_org_opacity;
	GtkWidget *hbox_patch_color, *hbox_grid_color, *hbox_node_color;
	GtkWidget *button_patch_color, *button_grid_color, *button_node_color;
	GdkRGBA gcolor;
	float color4[4];
	
	GtkWidget *spin_opacity;
	GtkAdjustment *adj_opacity;
	char current_opacity_text[30];
	
	GtkWidget *combobox_patch_color;
	GtkWidget *label_tree_patch_color;
	GtkCellRenderer *renderer_patch_color;
	GtkTreeModel *model_patch_color;
	GtkTreeModel *child_model_patch_color;
	
	GtkWidget *combobox_grid_color;
	GtkWidget *label_tree_grid_color;
	GtkCellRenderer *renderer_grid_color;
	GtkTreeModel *model_grid_color;
	GtkTreeModel *child_model_grid_color;
	
	GtkWidget *combobox_node_color;
	GtkWidget *label_tree_node_color;
	GtkCellRenderer *renderer_node_color;
	GtkTreeModel *model_node_color;
	GtkTreeModel *child_model_node_color;
	
	int index;
	int iflag_color;
	
	
	GtkWidget *vbox_table, *hbox_table;
	GtkWidget *vbox_sf_grp;
	GtkWidget *expander,  *scroll, *Frame;
	
	create_surface_group_view(surf_grp_vws);
	
	/* Delete data bottun */
	scrolled_table = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_table),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_table, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_table), surf_grp_vws->tree_view);
	
	/* Set signals for sorting */
	vbox_table = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_table), scrolled_table, TRUE, TRUE, 0);
	add_sorting_signal_w_label(GTK_TREE_VIEW(surf_grp_vws->tree_view), vbox_table);
	
	
    button_draw_patch = gtk_button_new_with_label("Draw patch");
    g_signal_connect(G_OBJECT(button_draw_patch), "clicked", 
                     G_CALLBACK(draw_all_sf_grp_patch_CB), (gpointer) surf_grp_vws);
    button_draw_grid = gtk_button_new_with_label("Draw grids");
    g_signal_connect(G_OBJECT(button_draw_grid), "clicked", 
                     G_CALLBACK(draw_all_sf_grp_grids_CB), (gpointer) surf_grp_vws);
    button_draw_node = gtk_button_new_with_label("Draw nodes");
    g_signal_connect(G_OBJECT(button_draw_node), "clicked", 
                     G_CALLBACK(draw_all_sf_grp_nodes_CB), (gpointer) surf_grp_vws);
	
    button_hide_patch = gtk_button_new_with_label("Hide patch");
    g_signal_connect(G_OBJECT(button_hide_patch), "clicked", 
                     G_CALLBACK(hide_all_sf_grp_patch_CB), (gpointer) surf_grp_vws);
    button_hide_grid = gtk_button_new_with_label("Hide grids");
    g_signal_connect(G_OBJECT(button_hide_grid), "clicked", 
                     G_CALLBACK(hide_all_sf_grp_grids_CB), (gpointer) surf_grp_vws);
    button_hide_node = gtk_button_new_with_label("Hide nodes");
    g_signal_connect(G_OBJECT(button_hide_node), "clicked", 
                     G_CALLBACK(hide_all_sf_grp_nodes_CB), (gpointer) surf_grp_vws);
	
	
	label_tree_patch_color = create_fixed_label_w_index_tree();
	model_patch_color = gtk_tree_view_get_model (label_tree_patch_color);  
	child_model_patch_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_patch_color));
	index = 0;
	index = append_ci_item_to_tree(index, "White",           WHITE_SURFACE, child_model_patch_color);
	index = append_ci_item_to_tree(index, "Single color",    SINGLE_COLOR, child_model_patch_color);
	index = append_ci_item_to_tree(index, "Color by domain", DOMAIN_COLOR, child_model_patch_color);
	index = append_ci_item_to_tree(index, "Color by group",  GROUP_COLOR, child_model_patch_color);
	
	combobox_patch_color = gtk_combo_box_new_with_model(child_model_patch_color);
	renderer_patch_color = gtk_cell_renderer_text_new();
	iflag_color = kemoview_get_surf_grp_color_flag(SURFSOLID_TOGGLE);
	if(iflag_color == GROUP_COLOR){
		gtk_combo_box_set_active(combobox_patch_color, 3);
	} else 	if(iflag_color == DOMAIN_COLOR){
		gtk_combo_box_set_active(combobox_patch_color, 2);
	} else 	if(iflag_color == SINGLE_COLOR){
		gtk_combo_box_set_active(combobox_patch_color, 1);
	} else {
		gtk_combo_box_set_active(combobox_patch_color, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_patch_color), renderer_patch_color, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_patch_color), renderer_patch_color,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_patch_color), "changed", 
				G_CALLBACK(surf_grp_patch_colormode_CB), (gpointer) window_mesh);
	
	
	label_tree_grid_color = create_fixed_label_w_index_tree();
	model_grid_color = gtk_tree_view_get_model (label_tree_grid_color);  
	child_model_grid_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_grid_color));
	index = 0;
	index = append_ci_item_to_tree(index, "Black",           BLACK_LINE, child_model_grid_color);
	index = append_ci_item_to_tree(index, "Single color",    SINGLE_COLOR, child_model_grid_color);
	index = append_ci_item_to_tree(index, "Color by domain", DOMAIN_COLOR, child_model_grid_color);
	index = append_ci_item_to_tree(index, "Color by group",  GROUP_COLOR, child_model_grid_color);
	
	combobox_grid_color = gtk_combo_box_new_with_model(child_model_grid_color);
	renderer_grid_color = gtk_cell_renderer_text_new();
	iflag_color = kemoview_get_surf_grp_color_flag(SURFGRID_TOGGLE);
	if(iflag_color == GROUP_COLOR){
		gtk_combo_box_set_active(combobox_grid_color, 3);
	} else 	if(iflag_color == DOMAIN_COLOR){
		gtk_combo_box_set_active(combobox_grid_color, 2);
	} else 	if(iflag_color == SINGLE_COLOR){
		gtk_combo_box_set_active(combobox_grid_color, 1);
	} else {
		gtk_combo_box_set_active(combobox_grid_color, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_grid_color), renderer_grid_color, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_grid_color), renderer_grid_color,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_grid_color), "changed", 
				G_CALLBACK(surf_grp_grid_colormode_CB), (gpointer) window_mesh);
	
	
	label_tree_node_color = create_fixed_label_w_index_tree();
	model_node_color = gtk_tree_view_get_model (label_tree_node_color);  
	child_model_node_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_node_color));
	index = 0;
	index = append_ci_item_to_tree(index, "White",           WHITE_SURFACE, child_model_node_color);
	index = append_ci_item_to_tree(index, "Single color",    SINGLE_COLOR, child_model_node_color);
	index = append_ci_item_to_tree(index, "Color by domain", DOMAIN_COLOR, child_model_node_color);
	index = append_ci_item_to_tree(index, "Color by group",  GROUP_COLOR, child_model_node_color);
	
	combobox_node_color = gtk_combo_box_new_with_model(child_model_node_color);
	renderer_node_color = gtk_cell_renderer_text_new();
	iflag_color = kemoview_get_surf_grp_color_flag(SURFNOD_TOGGLE);
	if(iflag_color == GROUP_COLOR){
		gtk_combo_box_set_active(combobox_node_color, 3);
	} else 	if(iflag_color == DOMAIN_COLOR){
		gtk_combo_box_set_active(combobox_node_color, 2);
	} else 	if(iflag_color == SINGLE_COLOR){
		gtk_combo_box_set_active(combobox_node_color, 1);
	} else {
		gtk_combo_box_set_active(combobox_node_color, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_node_color), renderer_node_color, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_node_color), renderer_node_color,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_node_color), "changed", 
				G_CALLBACK(surf_grp_node_colormode_CB), (gpointer) window_mesh);
	
	
	kemoview_get_surf_grp_color_code(SURFSOLID_TOGGLE, color4);
	set_color_to_GTK(color4, &gcolor);
	sprintf(current_opacity_text, "    %f    ", color4[3]);
	adj_opacity = gtk_adjustment_new(color4[3], 0.0, 1.0, 0.01, 0.01, 0.0);
	spin_opacity = gtk_spin_button_new(GTK_ADJUSTMENT(adj_opacity), 0, 2);
	g_signal_connect(spin_opacity, "value-changed", 
				G_CALLBACK(set_surf_grp_opacity_CB), NULL);
	
	button_patch_color = gtk_color_button_new_with_rgba(&gcolor);
    g_signal_connect(G_OBJECT(button_patch_color), "clicked", 
                     G_CALLBACK(set_single_surf_grp_patch_color_CB), (gpointer) window_mesh);
	
	kemoview_get_surf_grp_color_code(SURFGRID_TOGGLE, color4);
	set_color_to_GTK(color4, &gcolor);
	button_grid_color = gtk_color_button_new_with_rgba(&gcolor);
    g_signal_connect(G_OBJECT(button_grid_color), "clicked", 
				G_CALLBACK(set_single_surf_grp_grids_color_CB), (gpointer) window_mesh);
	
	kemoview_get_surf_grp_color_code(SURFNOD_TOGGLE, color4);
	set_color_to_GTK(color4, &gcolor);
	button_node_color = gtk_color_button_new_with_rgba(&gcolor);
    g_signal_connect(G_OBJECT(button_node_color), "clicked", 
				G_CALLBACK(set_single_surf_grp_nodes_color_CB), (gpointer) window_mesh);
	
	
	Frame = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame), vbox_table);
	
	hbox_table = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_table), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_table), Frame, TRUE, TRUE, 0);
	
	hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw all: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), button_draw_patch, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), button_draw_grid, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), button_draw_node, TRUE, FALSE, 0);
	
	hbox_hide = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_hide), gtk_label_new("Hide all: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_hide), button_hide_patch, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_hide), button_hide_grid, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_hide), button_hide_node, TRUE, FALSE, 0);
	
	hbox_org_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_opacity), gtk_label_new("Current opacity: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_opacity), gtk_label_new(current_opacity_text), TRUE, TRUE, 0);
	hbox_one_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), gtk_label_new("Opacity: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), spin_opacity, TRUE, TRUE, 0);
	
	hbox_patch_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_patch_color), gtk_label_new("Patch color: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_patch_color), combobox_patch_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_patch_color), button_patch_color, TRUE, FALSE, 0);
	hbox_grid_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_grid_color), gtk_label_new("Grid color: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_grid_color), combobox_grid_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_grid_color), button_grid_color, TRUE, FALSE, 0);
	hbox_node_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), gtk_label_new("Node color: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), combobox_node_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), button_node_color, TRUE, FALSE, 0);
	
	
	vbox_sf_grp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_table, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_draw, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_hide, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_org_opacity, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_one_opacity, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_patch_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_grid_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf_grp), hbox_node_color, TRUE, FALSE, 0);
	
	scroll = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll), vbox_sf_grp);
	
	expander = gtk_expander_new_with_mnemonic("Surface group");
	gtk_container_add(GTK_CONTAINER(expander), scroll);
	
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, FALSE, 0);
};
