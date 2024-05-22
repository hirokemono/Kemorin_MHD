/*
 *  kemoview_gtk_mesh_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_mesh_menu.h"

static void close_mesh_CB(GtkButton *button, gpointer user_data){
    GtkWidget *meshWin = GTK_WIDGET(user_data);
    struct kemoview_mesh_view *mesh_vws
        = (struct kemoview_mesh_view *) g_object_get_data(G_OBJECT(user_data), "mesh_menu");
    struct kemoviewer_type *kemo_sgl
        = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

    kemoview_close_mesh_view(kemo_sgl);
    init_mesh_window(kemo_sgl, mesh_vws, meshWin);
    draw_full(kemo_sgl);
    return;
};

static void subdoain_distance_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double dist = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(dist >= 0.0) kemoview_set_domain_distance(dist, kemo_sgl);
	kemoview_draw_with_modified_domain_distance(kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void node_size_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double current_size;
	int i_digit;
	double floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));

	kemoview_get_node_diamater(kemo_sgl, &current_size, &i_digit);
	kemoview_set_node_diamater(floatvalue, i_digit, kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void node_digit_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double current_size;
	int i_digit;
	int intvalue = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	
	kemoview_get_node_diamater(kemo_sgl, &current_size, &i_digit);
	kemoview_set_node_diamater(current_size, intvalue, kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void num_color_loop_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	int nloop = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_set_num_of_color_loop(nloop, kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void set_mesh_color_mode_CB(GtkComboBox *combobox_sfcolor, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_sfcolor);
    GtkTreeIter iter;
    
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
	kemoview_set_mesh_color_mode(index_mode, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
};

void set_gtk_mesh_menu(struct kemoviewer_type *kemo_sgl,
                       struct kemoview_mesh_view *mesh_vws){
	double current_size;
	int i_digit;

	int iflag_mode = kemoview_get_mesh_color_mode(kemo_sgl);
	int current_num_loop = kemoview_get_num_of_color_loop(kemo_sgl);
	double current_distance = kemoview_get_domain_distance(kemo_sgl);
	kemoview_get_node_diamater(kemo_sgl, &current_size, &i_digit);

	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_dist), current_distance);
	
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_node_size), current_size);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_node_digit), i_digit);
	
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_num_loop), current_num_loop);
	
	if(iflag_mode == GRAYSCALE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(mesh_vws->combobox_color_mode), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(mesh_vws->combobox_color_mode), 0);
	};
	return;
};

static void init_gtk_mesh_menu(struct kemoviewer_type *kemo_sgl,
                               struct kemoview_mesh_view *mesh_vws,
                               GtkWidget *window){
    GtkAdjustment *adj_dist;
    GtkAdjustment *adj_node_size, *adj_node_digit;
    GtkAdjustment *adj_num_loop;
    
    GtkWidget *label_tree_color_mode;
    GtkCellRenderer *renderer_color_mode;
    GtkTreeModel *model_color_mode;
    GtkTreeModel *child_model_color_mode;
    
    int index = 0;
    
    /* Set buttons   */
    
    adj_dist = gtk_adjustment_new(0.0, 0.0, 10.0, 0.005, 0.005, 0.0);
    mesh_vws->spin_dist = gtk_spin_button_new(GTK_ADJUSTMENT(adj_dist), 0, 3);
    g_signal_connect(mesh_vws->spin_dist, "value-changed",
                     G_CALLBACK(subdoain_distance_CB), (gpointer) kemo_sgl);
    
    adj_node_size = gtk_adjustment_new(1, 0, 9, 1, 1, 0);
    mesh_vws->spin_node_size = gtk_spin_button_new(GTK_ADJUSTMENT(adj_node_size), 0, 3);
    g_signal_connect(mesh_vws->spin_node_size, "value-changed",
                     G_CALLBACK(node_size_CB), (gpointer) kemo_sgl);
    
    adj_node_digit = gtk_adjustment_new(-3, -10, 10, 1, 1, 0);
    mesh_vws->spin_node_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_node_digit), 0, 0);
    g_signal_connect(mesh_vws->spin_node_digit, "value-changed",
                     G_CALLBACK(node_digit_CB), (gpointer) kemo_sgl);
    
    adj_num_loop = gtk_adjustment_new(5, 0, 100, 1, 1, 0.0);
    mesh_vws->spin_num_loop = gtk_spin_button_new(GTK_ADJUSTMENT(adj_num_loop), 0, 0);
    g_signal_connect(mesh_vws->spin_num_loop, "value-changed",
                     G_CALLBACK(num_color_loop_CB), (gpointer) kemo_sgl);
    
    label_tree_color_mode = create_fixed_label_w_index_tree();
    model_color_mode = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_color_mode));
    child_model_color_mode = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_color_mode));
    index = 0;
    index = append_ci_item_to_tree(index, "Rainbow",   RAINBOW_COLOR, child_model_color_mode);
    index = append_ci_item_to_tree(index, "Grayscale", GRAYSCALE, child_model_color_mode);
    
    renderer_color_mode = gtk_cell_renderer_text_new();
    mesh_vws->combobox_color_mode = gtk_combo_box_new_with_model(child_model_color_mode);
    gtk_combo_box_set_active(GTK_COMBO_BOX(mesh_vws->combobox_color_mode), 0);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(mesh_vws->combobox_color_mode), renderer_color_mode, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(mesh_vws->combobox_color_mode), renderer_color_mode,
                                   "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(mesh_vws->combobox_color_mode), "changed",
                     G_CALLBACK(set_mesh_color_mode_CB), (gpointer) kemo_sgl);
    
    set_gtk_mesh_menu(kemo_sgl, mesh_vws);
    
    init_domain_draw_expander(kemo_sgl, window, mesh_vws->domain_group_gmenu);
    init_nod_group_draw_expander(kemo_sgl,  window, mesh_vws->node_group_gmenu);
    init_ele_group_draw_box(kemo_sgl,  window, mesh_vws->ele_group_gmenu);
    init_surf_group_draw_box(kemo_sgl, window, mesh_vws->surf_group_gmenu);
    
    set_domain_draw_box(kemo_sgl, mesh_vws->domain_group_gmenu);
    set_nod_group_draw_box(kemo_sgl, mesh_vws->node_group_gmenu);
    set_ele_group_draw_box(kemo_sgl, mesh_vws->ele_group_gmenu);
    set_surf_group_draw_box(kemo_sgl, mesh_vws->surf_group_gmenu);
    
    return;
}

static GtkWidget * pack_gtk_mesh_menu(struct kemoview_mesh_view *mesh_vws,
                                      GtkWidget *window){
    GtkWidget *frame;

    GtkWidget *hbox_distance = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_distance), gtk_label_new("Subdomain distance: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_distance), mesh_vws->spin_dist, TRUE, TRUE, 0);
    
    GtkWidget *hbox_node_size = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_node_size), gtk_label_new("Node size: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_node_size), mesh_vws->spin_node_size, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_node_size), gtk_label_new("x 10^ "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_node_size), mesh_vws->spin_node_digit, TRUE, TRUE, 0);
    
    GtkWidget *hbox_color_mode = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_color_mode), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_color_mode), mesh_vws->combobox_color_mode, FALSE, FALSE, 0);
    
    GtkWidget *hbox_num_loop = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_num_loop), gtk_label_new("Number of color loop: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_num_loop), mesh_vws->spin_num_loop, TRUE, TRUE, 0);
    
    GtkWidget *box_domain =    pack_domain_menu_box(mesh_vws->domain_group_gmenu);
    GtkWidget *box_node_grp =  pack_nod_group_menu_box(mesh_vws->node_group_gmenu);
    GtkWidget *box_ele_grp =   pack_ele_group_menu_box(mesh_vws->ele_group_gmenu);
    GtkWidget *box_surf_grp =  pack_surf_group_menu_box(mesh_vws->surf_group_gmenu);

    GtkWidget *expander_domain = wrap_into_scroll_expansion_gtk("Domain", 480, 640,
                                                                window, box_domain);
    GtkWidget *expander_node = wrap_into_scroll_expansion_gtk("Node group", 480, 480,
                                                              window, box_node_grp);
    GtkWidget *expander_ele =  wrap_into_scroll_expansion_gtk("Element group", 480, 640,
                                                              window, box_ele_grp);
    GtkWidget *expander_surf = wrap_into_scroll_expansion_gtk("Surface group", 480, 640,
                                                              window, box_surf_grp);

    GtkWidget *vbox_mesh = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), mesh_vws->closeMeshButton,
                       FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), hbox_distance, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), hbox_node_size, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), hbox_color_mode, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), hbox_num_loop, FALSE, FALSE, 0);
    
    gtk_box_pack_start(GTK_BOX(vbox_mesh), expander_domain, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), expander_node, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), expander_ele, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mesh), expander_surf, FALSE, FALSE, 0);

    frame = wrap_into_frame_gtk("Mesh", vbox_mesh);
    return frame;
}

static void set_mesh_menu_box(struct kemoviewer_type *kemo_sgl,
                              struct kemoview_mesh_view *mesh_vws,
                              GtkWidget *meshWin){
    init_mesh_views_4_viewer(kemo_sgl, mesh_vws);
    
    /*  Set buttons */
    g_object_set_data(G_OBJECT(meshWin), "mesh_menu", (gpointer) mesh_vws);
    g_object_set_data(G_OBJECT(meshWin), "kemoview", (gpointer) kemo_sgl);

    mesh_vws->closeMeshButton = gtk_button_new_with_label("Close mesh");
    g_signal_connect(G_OBJECT(mesh_vws->closeMeshButton), "clicked",
                     G_CALLBACK(close_mesh_CB), (gpointer) meshWin);
    
    init_gtk_mesh_menu(kemo_sgl, mesh_vws, meshWin);
    return;
}

void init_mesh_window(struct kemoviewer_type *kemo_sgl,
                      struct kemoview_mesh_view *mesh_vws,
                      GtkWidget *meshWin){
    if(mesh_vws->iflag_meshBox > 0){
        gtk_widget_destroy(meshWin);
        gtk_window_get_position(GTK_WINDOW(meshWin),
                                &mesh_vws->ij_Win[0],
                                &mesh_vws->ij_Win[1]);
        dealloc_mesh_views_4_viewer(mesh_vws);
    };
    mesh_vws->iflag_meshBox = kemoview_get_draw_mesh_flag(kemo_sgl);
    if(mesh_vws->iflag_meshBox == 0) return;

    meshWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(meshWin), "Mesh");
//    gtk_window_move (GTK_WINDOW(meshWin),
//                     mesh_vws->ij_Win[0],
//                     (mesh_vws->ij_Win[1]+28));
    gtk_widget_set_size_request(meshWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(meshWin), 5);
    
    set_mesh_menu_box(kemo_sgl, mesh_vws, meshWin);
    GtkWidget *frame_mesh = pack_gtk_mesh_menu(mesh_vws, meshWin);
    
    gtk_container_add(GTK_CONTAINER(meshWin), frame_mesh);
    gtk_widget_show_all(meshWin);
    return;
}

