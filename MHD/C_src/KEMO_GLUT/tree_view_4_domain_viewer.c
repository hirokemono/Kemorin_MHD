/*
//  tree_view_4_domain_viewer.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_domain_viewer.h"



static void toggle_draw_domain_patch_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	int index1_for_toggle;
	int index_grp = toggle_draw_patch_switch(path_str, user_data, &index1_for_toggle);
    kemoview_set_draw_domain_patch(index1_for_toggle, index_grp);
}

static void toggle_draw_domain_grid_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	int index2_for_toggle;
	int index_grp = toggle_draw_grid_switch(path_str, user_data, &index2_for_toggle);
    kemoview_set_draw_domain_grid(index2_for_toggle, index_grp);
}

static void toggle_draw_domain_node_switch(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
	int index3_for_toggle;
	int index_grp = toggle_draw_node_switch(path_str, user_data, &index3_for_toggle);
    kemoview_set_draw_domain_nod(index3_for_toggle, index_grp);
}


static void draw_all_domain_patch_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_THIRD, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_domain_patch(IONE, i);
	};
}

static void draw_all_domain_grids_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FORTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_domain_grid(IONE, i);
	};
}

static void draw_all_domain_nodes_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FIFTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_domain_nod(IONE, i);
	};
}

static void hide_all_domain_patch_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IZERO, COLUMN_MESH_THIRD, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_domain_patch(IZERO, i);
	};
}

static void hide_all_domain_grids_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FORTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_domain_grid(IZERO, i);
	};
}

static void hide_all_domain_nodes_CB(GtkButton *button, gpointer user_data)
{
	int i;
	int num = set_all_draw_flags(IZERO, COLUMN_MESH_FIFTH, user_data);
	for(i=0;i<num;i++){
		kemoview_set_draw_domain_nod(IZERO, i);
	};
}


static void psf_domain_patch_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
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
		kemoview_set_domain_color_flag(SURFSOLID_TOGGLE, index_mode);
	} else {
		kemoview_set_domain_color_flag(SURFSOLID_TOGGLE, index_mode);
	};
	
//	draw_mesh_w_menu();
	return;
};

static void psf_domain_grid_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
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
		kemoview_set_domain_color_flag(SURFGRID_TOGGLE, index_mode);
	} else {
		kemoview_set_domain_color_flag(SURFGRID_TOGGLE, index_mode);
	};
	
//	draw_mesh_w_menu();
	return;
};

static void psf_domain_node_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
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
		kemoview_set_domain_color_flag(SURFNOD_TOGGLE, index_mode);
	} else {
		kemoview_set_domain_color_flag(SURFNOD_TOGGLE, index_mode);
	};
	
//	draw_mesh_w_menu();
	return;
};

static void set_single_domain_patch_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent = GTK_WINDOW(user_data);
	
	int iflag_set = kemoview_gtk_colorsel_CB(parent, colorcode4);
	if(iflag_set > 0) {kemoview_set_domain_color_code(SURFSOLID_TOGGLE, colorcode4);};
	return;
};
static void set_single_domain_grids_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent = GTK_WINDOW(user_data);
	
	int iflag_set = kemoview_gtk_colorsel_CB(parent, colorcode4);
	if(iflag_set > 0) {kemoview_set_domain_color_code(SURFGRID_TOGGLE, colorcode4);};
	return;
};
static void set_single_domain_nodes_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent = GTK_WINDOW(user_data);
	
	int iflag_set = kemoview_gtk_colorsel_CB(parent, colorcode4);
	if(iflag_set > 0) {kemoview_set_domain_color_code(SURFNOD_TOGGLE, colorcode4);};
	return;
};


static void create_domain_group_columns(struct ci3_clist_view *domain_vws)
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
	column_1st = create_each_column_no_sort(domain_vws->tree_view, 
				"Index", COLUMN_MESH_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 40, COLUMN_MESH_INDEX);
    
    /* Second row */
	column_2nd = create_each_column_no_sort(domain_vws->tree_view, 
				"Domain", COLUMN_MESH_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_MESH_NAME);
   
    /* Third row */
	column_3rd = create_each_column_no_sort(domain_vws->tree_view,
				"Patch", COLUMN_MESH_THIRD);
	toggleRenderer1 = create_each_toggle_renderer(column_3rd, 30, COLUMN_MESH_THIRD);
	g_signal_connect(G_OBJECT(toggleRenderer1), "toggled", 
				G_CALLBACK(toggle_draw_domain_patch_switch), (gpointer) domain_vws);
    
    /* Forth row */
	column_4th = create_each_column_no_sort(domain_vws->tree_view,
				"Grid", COLUMN_MESH_FORTH);
	toggleRenderer2 = create_each_toggle_renderer(column_4th, 30, COLUMN_MESH_FORTH);
	g_signal_connect(G_OBJECT(toggleRenderer2), "toggled",
				G_CALLBACK(toggle_draw_domain_grid_switch), (gpointer) domain_vws);
	
    /* Fifth row */
	column_5th = create_each_column_no_sort(domain_vws->tree_view,
				"Node", COLUMN_MESH_FIFTH);
	toggleRenderer3 = create_each_toggle_renderer(column_5th, 30, COLUMN_MESH_FIFTH);
	g_signal_connect(G_OBJECT(toggleRenderer3), "toggled",
				G_CALLBACK(toggle_draw_domain_node_switch), (gpointer) domain_vws);
};

static void create_domain_group_view(struct ci3_clist_view *domain_vws)
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
    domain_vws->tree_view = gtk_tree_view_new();
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(domain_vws->tree_view), model);
	
	create_domain_group_columns(domain_vws);
    
    /* Mode selection */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(domain_vws->tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(domain_vws->tree_view), COLUMN_MESH_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_MESH_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<count_chara_int3_clist(domain_vws->ci3_clist_gtk);i++){
		append_grp_model_data(i, domain_vws, child_model);
    }
    
}

void add_domain_draw_box(struct ci3_clist_view *domain_vws, 
			GtkWidget *window_mesh, GtkWidget *vbox)
{
	GtkWidget *scrolled_table;
	
	GtkWidget *button_draw_patch, *button_draw_grid, *button_draw_node;
	GtkWidget *button_hide_patch, *button_hide_grid, *button_hide_node;
	GtkWidget *hbox_draw, *hbox_hide;
	
	GtkWidget *hbox_patch_color, *hbox_grid_color, *hbox_node_color;
	GtkWidget *button_patch_color, *button_grid_color, *button_node_color;
	GdkRGBA gcolor;
	float color4[4];
	
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
	GtkWidget *vbox_domain;
	GtkWidget *expander,  *scroll, *Frame;
	
	create_domain_group_view(domain_vws);
	
	/* Delete data bottun */
	scrolled_table = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_table),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_table, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_table), domain_vws->tree_view);
	
	/* Set signals for sorting */
	vbox_table = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_table), scrolled_table, TRUE, TRUE, 0);
	add_sorting_signal_w_label(GTK_TREE_VIEW(domain_vws->tree_view), vbox_table);
	
    button_draw_patch = gtk_button_new_with_label("Draw patch");
    g_signal_connect(G_OBJECT(button_draw_patch), "clicked", 
                     G_CALLBACK(draw_all_domain_patch_CB), (gpointer) domain_vws);
    button_draw_grid = gtk_button_new_with_label("Draw grids");
    g_signal_connect(G_OBJECT(button_draw_grid), "clicked", 
                     G_CALLBACK(draw_all_domain_grids_CB), (gpointer) domain_vws);
    button_draw_node = gtk_button_new_with_label("Draw nodes");
    g_signal_connect(G_OBJECT(button_draw_node), "clicked", 
                     G_CALLBACK(draw_all_domain_nodes_CB), (gpointer) domain_vws);
	
    button_hide_patch = gtk_button_new_with_label("Hide patch");
    g_signal_connect(G_OBJECT(button_hide_patch), "clicked", 
                     G_CALLBACK(hide_all_domain_patch_CB), (gpointer) domain_vws);
    button_hide_grid = gtk_button_new_with_label("Hide grids");
    g_signal_connect(G_OBJECT(button_hide_grid), "clicked", 
                     G_CALLBACK(hide_all_domain_grids_CB), (gpointer) domain_vws);
    button_hide_node = gtk_button_new_with_label("Hide nodes");
    g_signal_connect(G_OBJECT(button_hide_node), "clicked", 
                     G_CALLBACK(hide_all_domain_nodes_CB), (gpointer) domain_vws);
	
	
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
	iflag_color = kemoview_get_domain_color_flag(SURFSOLID_TOGGLE);
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
				G_CALLBACK(psf_domain_patch_colormode_CB), (gpointer) window_mesh);
	
	
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
	iflag_color = kemoview_get_domain_color_flag(SURFGRID_TOGGLE);
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
				G_CALLBACK(psf_domain_grid_colormode_CB), (gpointer) window_mesh);
	
	
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
	iflag_color = kemoview_get_domain_color_flag(SURFNOD_TOGGLE);
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
				G_CALLBACK(psf_domain_node_colormode_CB), (gpointer) window_mesh);
	
	kemoview_get_domain_color_code(SURFSOLID_TOGGLE, color4);
	set_color_to_GTK(color4, &gcolor);
	button_patch_color = gtk_color_button_new_with_rgba(&gcolor);
    g_signal_connect(G_OBJECT(button_patch_color), "clicked", 
                     G_CALLBACK(set_single_domain_patch_color_CB), (gpointer) window_mesh);
	
	kemoview_get_domain_color_code(SURFGRID_TOGGLE, color4);
	set_color_to_GTK(color4, &gcolor);
	button_grid_color = gtk_color_button_new_with_rgba(&gcolor);
    g_signal_connect(G_OBJECT(button_grid_color), "clicked", 
                     G_CALLBACK(set_single_domain_grids_color_CB), (gpointer) window_mesh);
	
	kemoview_get_domain_color_code(SURFNOD_TOGGLE, color4);
	set_color_to_GTK(color4, &gcolor);
	button_node_color = gtk_color_button_new_with_rgba(&gcolor);
    g_signal_connect(G_OBJECT(button_node_color), "clicked", 
                     G_CALLBACK(set_single_domain_nodes_color_CB), (gpointer) window_mesh);
	
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
	
	vbox_domain = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_domain), hbox_table, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_domain), hbox_draw, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_domain), hbox_hide, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_domain), hbox_patch_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_domain), hbox_grid_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_domain), hbox_node_color, TRUE, FALSE, 0);
	
	scroll = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll), vbox_domain);
	
	expander = gtk_expander_new_with_mnemonic("Domain");
	gtk_container_add(GTK_CONTAINER(expander), scroll);
	
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, FALSE, 0);
};

