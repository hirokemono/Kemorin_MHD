/*
//  tree_view_4_nod_group_viewer.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_nod_group_viewer.h"

static void toggle_draw_node_group_CB(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	struct ci_clist_view *nod_grp_vws 
			= (struct ci_clist_view *) g_object_get_data(G_OBJECT(user_data), "nod_grp_view");
	struct mesh_menu_val *mesh_m 
			= (struct mesh_menu_val *) g_object_get_data(G_OBJECT(user_data), "mesh_menu");
	int index1_for_toggle;
	int index_grp = toggle_draw_nod_grp_node_switch(path_str, nod_grp_vws, &index1_for_toggle);
    set_draw_nodgrp_node(index1_for_toggle, index_grp, mesh_m);
	
	draw_full();
}

static void draw_all_nod_grp_nodes_CB(GtkButton *button, gpointer user_data)
{
	struct ci_clist_view *nod_grp_vws 
			= (struct ci_clist_view *) g_object_get_data(G_OBJECT(user_data), "nod_grp_view");
	struct mesh_menu_val *mesh_m 
			= (struct mesh_menu_val *) g_object_get_data(G_OBJECT(user_data), "mesh_menu");
	int i;
	int num = set_all_node_draw_flags(IONE, nod_grp_vws);
	for(i=0;i<num;i++){
		set_draw_nodgrp_node(IONE, i, mesh_m);
	};
	
	draw_full();
}

static void hide_all_nod_grp_nodes_CB(GtkButton *button, gpointer user_data)
{
	struct ci_clist_view *nod_grp_vws 
			= (struct ci_clist_view *) g_object_get_data(G_OBJECT(user_data), "nod_grp_view");
	struct mesh_menu_val *mesh_m 
			= (struct mesh_menu_val *) g_object_get_data(G_OBJECT(user_data), "mesh_menu");
	int i;
	int num = set_all_node_draw_flags(IZERO, nod_grp_vws);
	for(i=0;i<num;i++){
		set_draw_nodgrp_node(IZERO, i, mesh_m);
	};
	
	draw_full();
}

static void nod_grp_node_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
	struct mesh_menu_val *mesh_m 
			= (struct mesh_menu_val *) g_object_get_data(G_OBJECT(user_data), "mesh_menu");
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	if (index_mode == SINGLE_COLOR){
		select_node_grp_node_color(index_mode, mesh_m);
	} else {
		select_node_grp_node_color(index_mode, mesh_m);
	};
	
	draw_full();
	return;
};

static void set_single_nod_grp_nodes_color_CB(GtkButton *button, gpointer user_data)
{
	struct mesh_menu_val *mesh_m 
			= (struct mesh_menu_val *) g_object_get_data(G_OBJECT(user_data), "mesh_menu");
	GtkWindow *parent = GTK_WINDOW(g_object_get_data(G_OBJECT(user_data), "parent"));
	float colorcode4[4];
	
	send_node_grp_color_code(mesh_m, colorcode4);
	int iflag_set = kemoview_gtk_colorsel_CB(parent, colorcode4);
	if(iflag_set > 0) {set_node_grp_color_code(colorcode4, mesh_m);};
	return;
};



static void create_node_group_columns(struct mesh_menu_val *mesh_m, struct ci_clist_view *nod_grp_vws)
{
    GtkCellRenderer *textRenderer1;
    GtkCellRenderer *textRenderer2;
    GtkCellRenderer *toggleRenderer1;
	
	GtkTreeViewColumn *column_1st;
    GtkTreeViewColumn *column_2nd;
    GtkTreeViewColumn *column_3rd;
	
	GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "nod_grp_view", (gpointer) nod_grp_vws);
	g_object_set_data(G_OBJECT(entry_file), "mesh_menu", (gpointer) mesh_m);
	
    /* First raw */
	column_1st = create_each_column_no_sort(nod_grp_vws->tree_view, 
				"Index", COLUMN_MESH_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_MESH_INDEX);
    
    /* Second row */
	column_2nd = create_each_column_no_sort(nod_grp_vws->tree_view, 
				"Group name", COLUMN_MESH_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_MESH_NAME);
   
    /* Third row */
	column_3rd = create_each_column_no_sort(nod_grp_vws->tree_view,
				"Node", COLUMN_MESH_THIRD);
	toggleRenderer1 = create_each_toggle_renderer(column_3rd, 60, COLUMN_MESH_THIRD);
	g_signal_connect(G_OBJECT(toggleRenderer1), "toggled", 
				G_CALLBACK(toggle_draw_node_group_CB), (gpointer) entry);
};

static void create_node_group_view(struct mesh_menu_val *mesh_m, struct ci_clist_view *nod_grp_vws)
{
    int i;
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    nod_grp_vws->tree_view = gtk_tree_view_new();
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(nod_grp_vws->tree_view), model);
	
	create_node_group_columns(mesh_m, nod_grp_vws);
    
    /* Mode selection */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(nod_grp_vws->tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(nod_grp_vws->tree_view), COLUMN_MESH_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_MESH_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<count_chara_int_clist(nod_grp_vws->ci_clist_gtk);i++){
		append_node_grp_model_data(i, nod_grp_vws, child_model);
    }
    
}

void add_nod_group_draw_box(struct mesh_menu_val *mesh_m, struct ci_clist_view *nod_grp_vws,
			GtkWidget *window_mesh, GtkWidget *vbox)
{
	GtkWidget *scrolled_table;
	
	GtkWidget *button_draw_node, *button_hide_node;
	GtkWidget *hbox_draw, *hbox_hide;
	
	GtkWidget *hbox_node_color;
	GtkWidget *button_node_color;
	GdkRGBA gcolor;
	float color4[4];
	
	GtkWidget *combobox_node_color;
	GtkWidget *label_tree_node_color;
	GtkCellRenderer *renderer_node_color;
	GtkTreeModel *model_node_color;
	GtkTreeModel *child_model_node_color;
	
	int index;
	int iflag_color;
	
	GtkWidget *vbox_table, *hbox_table;
	GtkWidget *vbox_nod_grp;
	GtkWidget *expander,  *scroll, *Frame;
	
	GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "nod_grp_view", (gpointer) nod_grp_vws);
	g_object_set_data(G_OBJECT(entry), "mesh_menu", (gpointer) mesh_m);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_mesh);
	
	create_node_group_view(mesh_m, nod_grp_vws);
	
	/* Delete data bottun */
	scrolled_table = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_table),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_table, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_table), nod_grp_vws->tree_view);
	
	/* Set signals for sorting */
	vbox_table = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_table), scrolled_table, TRUE, TRUE, 0);
	add_sorting_signal_w_label(GTK_TREE_VIEW(nod_grp_vws->tree_view), vbox_table);
	
	button_draw_node = gtk_button_new_with_label("Draw nodes");
    g_signal_connect(G_OBJECT(button_draw_node), "clicked", 
                     G_CALLBACK(draw_all_nod_grp_nodes_CB), (gpointer) entry);
    button_hide_node = gtk_button_new_with_label("Hide nodes");
    g_signal_connect(G_OBJECT(btton_hide_node), "clicked", 
                     G_CALLBACK(hide_all_nod_grp_nodes_CB), (gpointer) entry);
	
	
	
	label_tree_node_color = create_fixed_label_w_index_tree();
	model_node_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_node_color));  
	child_model_node_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_node_color));
	index = 0;
	index = append_ci_item_to_tree(index, "White",           WHITE_SURFACE, child_model_node_color);
	index = append_ci_item_to_tree(index, "Single color",    SINGLE_COLOR, child_model_node_color);
	index = append_ci_item_to_tree(index, "Color by domain", DOMAIN_COLOR, child_model_node_color);
	index = append_ci_item_to_tree(index, "Color by group",  GROUP_COLOR, child_model_node_color);
	
	combobox_node_color = gtk_combo_box_new_with_model(child_model_node_color);
	renderer_node_color = gtk_cell_renderer_text_new();
	iflag_color = kemoview_get_node_grp_color_flag();
	if(iflag_color == GROUP_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_node_color), 3);
	} else 	if(iflag_color == DOMAIN_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_node_color), 2);
	} else 	if(iflag_color == SINGLE_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_node_color), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_node_color), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_node_color), renderer_node_color, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_node_color), renderer_node_color,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_node_color), "changed", 
				G_CALLBACK(nod_grp_node_colormode_CB), (gpointer) entry);
	
	kemoview_get_node_grp_color_code(color4);
	set_color_to_GTK(color4, &gcolor);
	button_node_color = gtk_color_button_new_with_rgba(&gcolor);
    g_signal_connect(G_OBJECT(button_node_color), "clicked", 
				G_CALLBACK(set_single_nod_grp_nodes_color_CB), (gpointer) entry);
	
	
	Frame = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame), vbox_table);
	
	hbox_table = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_table), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_table), Frame, TRUE, TRUE, 0);
	
	hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw all: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), button_draw_node, TRUE, FALSE, 0);
	
	hbox_hide = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_hide), gtk_label_new("Hide all: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_hide), button_hide_node, TRUE, FALSE, 0);
	
	hbox_node_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), gtk_label_new("Node color: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), combobox_node_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), button_node_color, TRUE, FALSE, 0);
	
	vbox_nod_grp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_nod_grp), hbox_table, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_nod_grp), hbox_draw, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_nod_grp), hbox_hide, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_nod_grp), hbox_node_color, TRUE, FALSE, 0);
	
	scroll = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll), vbox_nod_grp);
	
	expander = gtk_expander_new_with_mnemonic("Node group");
	gtk_container_add(GTK_CONTAINER(expander), scroll);
	
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, FALSE, 0);
};
