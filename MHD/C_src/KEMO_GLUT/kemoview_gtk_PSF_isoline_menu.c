/*
 *  kemoview_gtk_PSF_isoline_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_isoline_menu.h"

static void psf_grid_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer data){
	kemoview_select_PSF_draw_switch(PSFGRID_TOGGLE);
	return;
};
static void psf_zero_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer data){
	kemoview_select_PSF_draw_switch(ZEROGRID_TOGGLE);
	return;
};

static void psf_surf_colormode_CB(GtkComboBox *combobox_gdcolor, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_gdcolor);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_gdcolor);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	if (index_mode == RAINBOW_PSF_LINE)    {kemoview_set_PSF_isoline_color_mode(WHITE_LINE);}
	else if (index_mode == WHITE_PSF_LINE) {kemoview_set_PSF_isoline_color_mode(WHITE_LINE);}
    else if (index_mode == BLACK_PSF_LINE) {kemoview_set_PSF_isoline_color_mode(RAINBOW_LINE);}
	
//	draw_mesh_w_menu();
	return;
};

static void set_nline_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	int gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_num_isoline(gtk_intvalue);
	return;
}

void add_gtk_isoline_menu(struct colormap_view *color_vws, 
			GtkWidget *window_cmap, GtkWidget *box){
	GtkWidget *hbox_draw, *hbox_zero, *hbox_color;
	GtkWidget *hbox_22, *hbox_23;
	
	GtkWidget *expander_iso,  *scroll_iso, *Frame_iso;
	GtkWidget *hbox_iso,  *vbox_iso;
	
	GtkWidget *switch_1, *switch_zero;
	
	GtkWidget *combobox_gdcolor;
	GtkWidget *label_tree_gdcolor;
	GtkCellRenderer *renderer_gdcolor;
	GtkTreeModel *model_gdcolor;
	GtkTreeModel *child_model_gdcolor;
	
	int index = 0;
	int iflag_sfcolor;
	
	GtkWidget *spin_nline;
	GtkAdjustment *adj_nline;
	char current_nline_txt[30];
	
	switch_1 = gtk_switch_new();
	if(kemoview_get_PSF_draw_flags(PSFGRID_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_1), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_1), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_1), "notify::active",
				G_CALLBACK(psf_grid_switch_CB), NULL);
	
	switch_zero = gtk_switch_new();
	if(kemoview_get_PSF_draw_flags(ZEROGRID_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_zero), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_zero), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_zero), "notify::active",
				G_CALLBACK(psf_zero_switch_CB), NULL);
	
	
	label_tree_gdcolor = create_fixed_label_w_index_tree();
	model_gdcolor = gtk_tree_view_get_model (label_tree_gdcolor);  
	child_model_gdcolor = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_gdcolor));
	index = 0;
	index = append_ci_item_to_tree(index, "Color lines", RAINBOW_PSF_LINE, child_model_gdcolor);
	index = append_ci_item_to_tree(index, "White lines", WHITE_PSF_LINE, child_model_gdcolor);
	index = append_ci_item_to_tree(index, "Black lines", BLACK_PSF_LINE, child_model_gdcolor);
	
	combobox_gdcolor = gtk_combo_box_new_with_model(child_model_gdcolor);
	renderer_gdcolor = gtk_cell_renderer_text_new();
	iflag_sfcolor = kemoview_get_PSF_patch_color_mode();
	if(iflag_sfcolor == BLACK_PSF_LINE){
		gtk_combo_box_set_active(combobox_gdcolor, 2);
	} else 	if(iflag_sfcolor == WHITE_PSF_LINE){
		gtk_combo_box_set_active(combobox_gdcolor, 1);
	} else {
		gtk_combo_box_set_active(combobox_gdcolor, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_gdcolor), renderer_gdcolor, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_gdcolor), renderer_gdcolor,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_gdcolor), "changed", 
				G_CALLBACK(psf_surf_colormode_CB), (gpointer) window_cmap);
	
	
	int current_nline = kemoview_get_PSF_num_isoline();
	sprintf(current_nline_txt, "    %d    ", current_nline);
	adj_nline = gtk_adjustment_new ((double) current_nline, 0, 200, 1, 1, 0.0);
	spin_nline = gtk_spin_button_new(GTK_ADJUSTMENT(adj_nline), 0, 2);
	g_signal_connect(spin_nline, "value-changed", G_CALLBACK(set_nline_CB), (gpointer) color_vws);
	
	hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw isolines: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), switch_1, FALSE, FALSE, 0);
	
	hbox_zero = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_zero), gtk_label_new("Draw zero line: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_zero), switch_zero, FALSE, FALSE, 0);
	
	hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), combobox_gdcolor, FALSE, FALSE, 0);
	
	hbox_23 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_23), gtk_label_new("Current num. of lines: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_23), gtk_label_new(current_nline_txt), TRUE, TRUE, 0);
	hbox_22 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_22), gtk_label_new("Num. of lines: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), spin_nline, TRUE, TRUE, 0);
	
	vbox_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_draw, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_zero, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_color, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_23, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_22, TRUE, TRUE, 0);
	
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
	
	expander_iso = gtk_expander_new_with_mnemonic("Isolines");
	gtk_container_add(GTK_CONTAINER(expander_iso), scroll_iso);
	
	gtk_box_pack_start(GTK_BOX(box), expander_iso, TRUE, FALSE, 0);
	return;
}
