/*
 *  kemoview_gtk_PSF_surface_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_surface_menu.h"

GtkWidget *window_csel;

static void set_PSFcolor_GTK(GtkColorChooser *colordialog)
{
	GdkRGBA gcolor;
	gdouble dcolor[4];
	
	gtk_color_chooser_get_rgba(colordialog, &gcolor);
	gtk_widget_destroy(window_csel);
	
	dcolor[0] = gcolor.red;
	dcolor[1] = gcolor.green;
	dcolor[2] = gcolor.blue;
	dcolor[3] = (gdouble) kemoview_get_PSF_max_opacity();
	kemoview_set_PSF_single_color(dcolor);
	kemoview_set_PSF_patch_color_mode(SINGLE_COLOR);
	draw_mesh_keep_menu();
	return;
}

static void kemoview_gtk_surfcolorsel(gpointer user_data){
	int response;
	GtkColorChooser *chooser;
	GtkWidget  *parent;
	
	parent = (GtkWidget *) user_data;
	
	window_csel = gtk_color_chooser_dialog_new("Choose surface color", parent);
	gtk_widget_show_all(window_csel);
	
	response = gtk_dialog_run(GTK_DIALOG(window_csel));
	if (response == GTK_RESPONSE_OK){
		chooser = GTK_COLOR_CHOOSER(window_csel);
		set_PSFcolor_GTK(chooser);
		g_print ("color selected \n");
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		gtk_widget_destroy(window_csel);
	}
	return;
}

static int load_texture_file_gtk(GtkWidget*parent, struct kv_string *file_prefix){
    struct kv_string *stripped_ext;
	int id_img;
	struct kv_string *filename= kemoview_read_file_panel(parent);
	
	if(filename->string[0] == '\0') return 0;
	
    stripped_ext = kemoview_alloc_kvstring();
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	
	id_img = kemoview_set_image_file_format_id(stripped_ext);
    kemoview_free_kvstring(stripped_ext);
    kemoview_free_kvstring(filename);
	return id_img;
}

static void load_texture_handler(gpointer user_data){
	int id_image;
    struct kv_string *image_prefix = kemoview_alloc_kvstring();
	GtkWidget  *parent = (GtkWidget *) user_data;
	id_image = load_texture_file_gtk(parent, image_prefix);
	
	if(id_image == SAVE_PNG || id_image == SAVE_BMP){
		kemoview_set_texture_to_PSF(id_image, image_prefix);
		kemoview_set_PSF_patch_color_mode(TEXTURED_SURFACE);
        kemoview_free_kvstring(image_prefix);
	};
	
//	glutSetWindow(winid);
//	draw_mesh_w_menu();
	return;
};

static void psf_surf_colormode_CB(GtkComboBox *combobox_cmap, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_cmap);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	if (index_mode == WHITE_SURFACE)     {kemoview_set_PSF_patch_color_mode(WHITE_SURFACE);}
	else if (index_mode == SINGLE_COLOR) {kemoview_gtk_surfcolorsel(user_data);}
    else if (index_mode == RAINBOW_PSF_SURF) {kemoview_set_PSF_patch_color_mode(RAINBOW_SURFACE);}
	else if (index_mode == TEXTURE_PSF_SURF) {load_texture_handler(user_data);};
	
//	draw_mesh_w_menu();
	return;
};

static void set_psf_opacity_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_constant_opacity(gtk_floatvalue);
	return;
}

static void MinChange(GtkWidget *entry, gpointer data)
{
	int icomp = kemoview_get_PSF_draw_data_address();
	double data_max = kemoview_get_PSF_max_data(icomp);
	
	double data_min = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_linear_colormap(data_min, data_max);
}
static void MaxChange(GtkWidget *entry, gpointer data)
{
	int icomp = kemoview_get_PSF_draw_data_address();
	double data_min = kemoview_get_PSF_min_data(icomp);
	
	double data_max = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_linear_colormap(data_min, data_max);
}

void add_gtk_psf_surface_menu(struct colormap_view *color_vws, 
							  GtkWidget *window_cmap, GtkWidget *box){
	GtkWidget *hbox_color;
	GtkWidget *hbox_one_opacity, *hbox_org_opacity;
	GtkWidget *hbox_range, *hbox_org_range;
	
	GtkWidget *expander_psf,  *scroll_psf, *Frame_psf;
	GtkWidget *hbox_psf,  *vbox_psf;
	
	GtkWidget *switch_1;
	int iflag_vect;
	
	GtkWidget *combobox_sfcolor;
	GtkWidget *label_tree_sfcolor;
	GtkCellRenderer *renderer_sfcolor;
	GtkTreeModel *model_sfcolor;
	GtkTreeModel *child_model_sfcolor;
	
	int index = 0;
	int iflag_sfcolor;
	
	GtkWidget *spin_opacity1;
	GtkAdjustment *adj_opacity1;
	double current_value;
	char current_text[30];
	
	GtkWidget *spin_min, *spin_max;
	GtkAdjustment *adj_min, *adj_max;
	int icomp;
	double data_min, data_max;
	double range_min, range_max, delta;
	char min_text[30], max_text[30];
	
	GtkWidget *spin_vect_width;
	GtkAdjustment *adj_vect_width;
	double current_vec_width;
	char current_vec_width_txt[30];
	
	label_tree_sfcolor = create_fixed_label_w_index_tree();
	model_sfcolor = gtk_tree_view_get_model (label_tree_sfcolor);  
	child_model_sfcolor = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_sfcolor));
	index = 0;
	index = append_ci_item_to_tree(index, "Contour", RAINBOW_PSF_SURF, child_model_sfcolor);
	index = append_ci_item_to_tree(index, "White", WHITE_SURFACE, child_model_sfcolor);
	index = append_ci_item_to_tree(index, "Single color", SINGLE_COLOR, child_model_sfcolor);
	index = append_ci_item_to_tree(index, "Texture", TEXTURE_PSF_SURF, child_model_sfcolor);
	
	combobox_sfcolor = gtk_combo_box_new_with_model(child_model_sfcolor);
	renderer_sfcolor = gtk_cell_renderer_text_new();
	iflag_sfcolor = kemoview_get_PSF_patch_color_mode();
	if(iflag_sfcolor == TEXTURE_PSF_SURF){
		gtk_combo_box_set_active(combobox_sfcolor, 3);
	} else 	if(iflag_sfcolor == SINGLE_COLOR){
		gtk_combo_box_set_active(combobox_sfcolor, 2);
	} else 	if(iflag_sfcolor == WHITE_SURFACE){
		gtk_combo_box_set_active(combobox_sfcolor, 1);
	} else {
		gtk_combo_box_set_active(combobox_sfcolor, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_sfcolor), renderer_sfcolor, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_sfcolor), renderer_sfcolor,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_sfcolor), "changed", 
				G_CALLBACK(psf_surf_colormode_CB), (gpointer) window_cmap);
	
	current_value = kemoview_get_PSF_max_opacity();
	sprintf(current_text, "    %e    ", current_value);
	adj_opacity1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin_opacity1 = gtk_spin_button_new(GTK_ADJUSTMENT(adj_opacity1), 0, 2);
	g_signal_connect(spin_opacity1, "value-changed", G_CALLBACK(set_psf_opacity_CB), (gpointer) color_vws);
	
	icomp = kemoview_get_PSF_draw_data_address();
	range_min = kemoview_get_PSF_min_data(icomp);
	range_max = kemoview_get_PSF_max_data(icomp);
	data_min = kemoview_get_PSF_color_table_min();
	data_max = kemoview_get_PSF_color_table_max();
	delta = range_max - range_min;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	adj_min = gtk_adjustment_new (data_min, (range_min-1.0e3*delta), (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
	adj_max = gtk_adjustment_new (data_max, (range_min*1.0e3),  (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
	spin_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min),0,2);
	spin_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max),0,2);
	g_signal_connect(spin_min, "value-changed", G_CALLBACK(MinChange), NULL);
	g_signal_connect(spin_max, "value-changed", G_CALLBACK(MaxChange), NULL);
	
	
	hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), combobox_sfcolor, FALSE, FALSE, 0);
	
	hbox_org_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_opacity), gtk_label_new("Current opacity: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_opacity), gtk_label_new(current_text), TRUE, TRUE, 0);
	hbox_one_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), gtk_label_new("Opacity: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), spin_opacity1, TRUE, TRUE, 0);
	
	hbox_org_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_range), gtk_label_new(min_text), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_range), gtk_label_new(max_text), TRUE, TRUE, 0);
	hbox_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range), spin_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range), spin_max, TRUE, TRUE, 0);
	
	vbox_psf = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_color, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_org_opacity, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_one_opacity, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_psf), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_org_range, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_range, TRUE, TRUE, 0);
	
	Frame_psf = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_psf), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_psf), vbox_psf);
	
	hbox_psf = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_psf), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_psf), Frame_psf, TRUE, TRUE, 0);
	
	scroll_psf = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_psf),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll_psf, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll_psf), hbox_psf);
	
	expander_psf = gtk_expander_new_with_mnemonic("Surface");
	gtk_container_add(GTK_CONTAINER(expander_psf), scroll_psf);
	
	gtk_box_pack_start(GTK_BOX(box), expander_psf, TRUE, FALSE, 0);
	return;
}
