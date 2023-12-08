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

static void psf_surface_switch_CB(GObject *switch_draw, GParamSpec *pspec, gpointer user_data){
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	kemoview_select_PSF_draw_switch(kemo_sgl, PSFSOLID_TOGGLE);
    draw_full(kemo_sgl);
	return;
};

static void psf_colorbar_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer user_data){
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	kemoview_select_PSF_draw_switch(kemo_sgl, COLORBAR_TOGGLE);
    draw_full(kemo_sgl);
	return;
};

static void set_PSFcolor_GTK(struct kemoviewer_type *kemo_sgl,
                             GtkColorChooser *colordialog)
{
	GdkRGBA gcolor;
	gdouble dcolor[4];
	
	gtk_color_chooser_get_rgba(colordialog, &gcolor);
	gtk_widget_destroy(window_csel);
	
	dcolor[0] = gcolor.red;
	dcolor[1] = gcolor.green;
	dcolor[2] = gcolor.blue;
	dcolor[3] = (gdouble) kemoview_get_each_PSF_colormap_range(kemo_sgl, ISET_OPACITY_MAX);
	kemoview_set_PSF_single_color(kemo_sgl, dcolor);
	kemoview_set_PSF_color_param(PSFSOLID_TOGGLE, SINGLE_COLOR, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}

static void kemoview_gtk_surfcolorsel(struct kemoviewer_type *kemo_sgl,
                                      GtkWindow *parent_window){
	int response;
	GtkColorChooser *chooser;
	
	window_csel = gtk_color_chooser_dialog_new("Choose surface color", parent_window);
	gtk_widget_show_all(window_csel);
	
	response = gtk_dialog_run(GTK_DIALOG(window_csel));
	if (response == GTK_RESPONSE_OK){
		chooser = GTK_COLOR_CHOOSER(window_csel);
		set_PSFcolor_GTK(kemo_sgl, chooser);
		g_print ("color selected \n");
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		gtk_widget_destroy(window_csel);
	}
	return;
}

static int load_texture_file_gtk(GtkWidget *window, struct kv_string *file_prefix){
    struct kv_string *stripped_ext;
	int id_img;
	struct kv_string *filename= kemoview_read_file_panel(window);
	
	if(filename->string[0] == '\0') return 0;
	
    stripped_ext = kemoview_alloc_kvstring();
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	
	id_img = kemoview_set_image_file_format_id(stripped_ext);
    kemoview_free_kvstring(stripped_ext);
    kemoview_free_kvstring(filename);
	return id_img;
}

static void load_texture_handler(struct kemoviewer_type *kemo_sgl,
                                 struct kemoviewer_gl_type *kemo_gl,
                                 GtkWidget *window){
	int id_image;
    struct kv_string *image_prefix = kemoview_alloc_kvstring();
	id_image = load_texture_file_gtk(window, image_prefix);
	
	if(id_image == SAVE_PNG || id_image == SAVE_BMP){
        kemoview_release_PSF_gl_texture(kemo_sgl, kemo_gl);
        kemoview_update_PSF_textured_id(kemo_sgl);
		kemoview_set_texture_to_PSF(id_image, image_prefix, kemo_sgl, kemo_gl);
		kemoview_set_PSF_color_param(PSFSOLID_TOGGLE, TEXTURED_SURFACE, kemo_sgl);
        kemoview_free_kvstring(image_prefix);
	};
	
	return;
};

static void psf_surf_colormode_CB(GtkComboBox *combobox_sfcolor,
                                  gpointer user_data)
{
    GtkWidget *parent_window = (GtkWidget *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	if (index_mode == WHITE_SURFACE){
		kemoview_set_PSF_color_param(PSFSOLID_TOGGLE, WHITE_SURFACE, kemo_sgl);
	}else if (index_mode == SINGLE_COLOR) {
		kemoview_gtk_surfcolorsel(kemo_sgl, parent_window);
	}else if (index_mode == CHANGE_PSF_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_sfcolor), 2);
	}else if (index_mode == RAINBOW_PSF_SURF){
		kemoview_set_PSF_color_param(PSFSOLID_TOGGLE, RAINBOW_SURFACE, kemo_sgl);
	}else if (index_mode == TEXTURE_PSF_SURF){
		load_texture_handler(kemo_sgl, kemo_gl, parent_window);
	};
	
    draw_full(kemo_sgl);
	return;
};

static void set_psf_opacity_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_constant_opacity(gtk_floatvalue, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}

static void MinRangeValueChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	double value_min, value_max;
	int i_min_digit, i_max_digit;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                      &value_min, &i_min_digit);
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX, 
                                      &value_max, &i_max_digit);
	kemoview_set_PSF_linear_colormap(gtk_floatvalue, i_min_digit, 
									 value_max, i_max_digit,
                                     kemo_sgl);
    draw_full(kemo_sgl);
}
static void MinRangeDigitChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	int gtk_intvalue = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	double value_min, value_max;
	int i_min_digit, i_max_digit;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN, 
                                      &value_min, &i_min_digit);
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX, 
                                      &value_max, &i_max_digit);
	kemoview_set_PSF_linear_colormap(value_min, gtk_intvalue, 
									 value_max, i_max_digit,
                                     kemo_sgl);
    draw_full(kemo_sgl);
}
static void MaxRangeValueChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	double value_min, value_max;
	int i_min_digit, i_max_digit;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                      &value_min, &i_min_digit);
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                      &value_max, &i_max_digit);
	kemoview_set_PSF_linear_colormap(value_min, i_min_digit, 
									 gtk_floatvalue, i_max_digit,
                                     kemo_sgl);
    draw_full(kemo_sgl);
}
static void MaxRangeDigitChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	int gtk_intvalue = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	double value_min, value_max;
	int i_min_digit, i_max_digit;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                      &value_min, &i_min_digit);
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX, 
                                      &value_max, &i_max_digit);
	kemoview_set_PSF_linear_colormap(value_min, i_min_digit, 
									 value_max, gtk_intvalue,
                                     kemo_sgl);
    draw_full(kemo_sgl);
}

void set_gtk_surface_menu_values(struct kemoviewer_type *kemo_sgl,
                                 struct psf_surface_gtk_menu *psf_surface_menu){
	int iflag_sfcolor;
	int icomp, i_digit;
	double current_value;
	double value_min, value_max;
	char min_text[30], max_text[30];
	
	if(kemoview_get_PSF_draw_flags(kemo_sgl, PSFSOLID_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(psf_surface_menu->switch_draw), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(psf_surface_menu->switch_draw), TRUE);
	};
	if(kemoview_get_PSF_draw_flags(kemo_sgl, COLORBAR_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(psf_surface_menu->switch_bar), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(psf_surface_menu->switch_bar), TRUE);
	};

	iflag_sfcolor = kemoview_get_PSF_color_param(kemo_sgl, PSFSOLID_TOGGLE);
	if(iflag_sfcolor == TEXTURE_PSF_SURF){
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_surface_menu->combobox_sfcolor), 4);
	} else 	if(iflag_sfcolor == SINGLE_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_surface_menu->combobox_sfcolor), 2);
	} else 	if(iflag_sfcolor == WHITE_SURFACE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_surface_menu->combobox_sfcolor), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_surface_menu->combobox_sfcolor), 0);
	};

	icomp = kemoview_get_each_PSF_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
	value_min = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MIN, icomp);
	value_max = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MAX, icomp);
	sprintf(min_text, "Min(%1.2e): ", value_min);
	sprintf(max_text, "Max(%1.2e): ", value_max);
	gtk_label_set_text(GTK_LABEL(psf_surface_menu->label_range_min), min_text);
	gtk_label_set_text(GTK_LABEL(psf_surface_menu->label_range_max), max_text);
	
	current_value = kemoview_get_each_PSF_colormap_range(kemo_sgl, ISET_OPACITY_MAX);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_surface_menu->spin_opacity1), current_value);
	
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                      &current_value, &i_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_surface_menu->spin_range_min), current_value);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_surface_menu->spin_digit_min), i_digit);
	
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                      &current_value, &i_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_surface_menu->spin_range_max), current_value);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_surface_menu->spin_digit_max), i_digit);
};

GtkWidget * init_gtk_psf_surface_menu_expander(struct kemoviewer_type *kemo_sgl,
                                               struct kemoviewer_gl_type *kemo_gl,
                                               GtkWidget *window, 
                                               struct colormap_view *color_vws, 
                                               struct psf_surface_gtk_menu *psf_surface_menu){
	GtkWidget *expander_surf;
	
	psf_surface_menu->switch_draw = gtk_switch_new();
	gtk_switch_set_active(GTK_SWITCH(psf_surface_menu->switch_draw), TRUE);
	g_signal_connect(G_OBJECT(psf_surface_menu->switch_draw), "notify::active",
				G_CALLBACK(psf_surface_switch_CB), (gpointer) kemo_sgl);
	
	psf_surface_menu->switch_bar = gtk_switch_new();
	gtk_switch_set_active(GTK_SWITCH(psf_surface_menu->switch_bar), FALSE);
	g_signal_connect(G_OBJECT(psf_surface_menu->switch_bar), "notify::active",
				G_CALLBACK(psf_colorbar_switch_CB), (gpointer) kemo_sgl);
	
	
	GtkWidget *label_tree_sfcolor = create_fixed_label_w_index_tree();
	GtkTreeModel *model_sfcolor = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_sfcolor));  
	GtkTreeModel *child_model_sfcolor = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_sfcolor));
	int index = 0;
	index = append_ci_item_to_tree(index, "Contour", RAINBOW_PSF_SURF, child_model_sfcolor);
	index = append_ci_item_to_tree(index, "White", WHITE_SURFACE, child_model_sfcolor);
	index = append_ci_item_to_tree(index, "Single color", SINGLE_COLOR, child_model_sfcolor);
	index = append_ci_item_to_tree(index, "Change color", CHANGE_PSF_COLOR, child_model_sfcolor);
	index = append_ci_item_to_tree(index, "Texture", TEXTURE_PSF_SURF, child_model_sfcolor);
	
	GtkCellRenderer *renderer_sfcolor = gtk_cell_renderer_text_new();
	psf_surface_menu->combobox_sfcolor = gtk_combo_box_new_with_model(child_model_sfcolor);
	gtk_combo_box_set_active(GTK_COMBO_BOX(psf_surface_menu->combobox_sfcolor), 0);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(psf_surface_menu->combobox_sfcolor), renderer_sfcolor, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(psf_surface_menu->combobox_sfcolor), renderer_sfcolor,
				"text", COLUMN_FIELD_NAME, NULL);
    g_object_set_data(G_OBJECT(window), "kemoview",  (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(window), "kemoview_gl",  (gpointer) kemo_gl);
	g_signal_connect(G_OBJECT(psf_surface_menu->combobox_sfcolor), "changed",
				G_CALLBACK(psf_surf_colormode_CB), (gpointer) window);
	
	psf_surface_menu->label_range_min = gtk_label_new(" ");
	psf_surface_menu->label_range_max = gtk_label_new(" ");
	
	GtkAdjustment *adj_opacity1 = gtk_adjustment_new(1.0, 0.0, 1.0, 0.01, 0.01, 0.0);
	psf_surface_menu->spin_opacity1 = gtk_spin_button_new(GTK_ADJUSTMENT(adj_opacity1), 0, 2);
	g_signal_connect(psf_surface_menu->spin_opacity1, "value-changed",
					 G_CALLBACK(set_psf_opacity_CB), (gpointer) kemo_sgl);
	
	GtkAdjustment *adj_range_min = gtk_adjustment_new (1, -9.999, 9.999, 0.1, 0.1, 0.0);
	GtkAdjustment *adj_digit_min = gtk_adjustment_new (0, -20, 20, 1, 1, 0);
	psf_surface_menu->spin_range_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_range_min),0,2);
	psf_surface_menu->spin_digit_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit_min),0,0);
	g_signal_connect(psf_surface_menu->spin_range_min, "value-changed", 
					 G_CALLBACK(MinRangeValueChange_CB), (gpointer) kemo_sgl);
	g_signal_connect(psf_surface_menu->spin_digit_min, "value-changed",
					 G_CALLBACK(MinRangeDigitChange_CB), (gpointer) kemo_sgl);
	
	GtkAdjustment *adj_range_max = gtk_adjustment_new (1, -9.999, 9.999, 0.1, 0.1, 0.0);
	GtkAdjustment *adj_digit_max = gtk_adjustment_new (0, -20, 20, 1, 1, 0);
	psf_surface_menu->spin_range_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_range_max),0,2);
	psf_surface_menu->spin_digit_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit_max),0,0);
	g_signal_connect(psf_surface_menu->spin_range_max, "value-changed",
					 G_CALLBACK(MaxRangeValueChange_CB), (gpointer) kemo_sgl);
	g_signal_connect(psf_surface_menu->spin_digit_max, "value-changed",
					 G_CALLBACK(MaxRangeDigitChange_CB), (gpointer) kemo_sgl);
	
	GtkWidget *hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw surface: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), psf_surface_menu->switch_draw, FALSE, FALSE, 0);
	
	GtkWidget *hbox_bar = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_bar), gtk_label_new("Draw color bar: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_bar), psf_surface_menu->switch_bar, FALSE, FALSE, 0);
	
	GtkWidget *hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), psf_surface_menu->combobox_sfcolor, FALSE, FALSE, 0);
	
	GtkWidget *hbox_one_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), gtk_label_new("Opacity"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), psf_surface_menu->spin_opacity1, FALSE, FALSE, 0);
	
	GtkWidget *hbox_min_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_min_range), psf_surface_menu->label_range_min, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_min_range), psf_surface_menu->spin_range_min, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_min_range), gtk_label_new("x10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_min_range), psf_surface_menu->spin_digit_min, FALSE, FALSE, 0);
	
	GtkWidget *hbox_max_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_max_range), psf_surface_menu->label_range_max, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_max_range), psf_surface_menu->spin_range_max, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_max_range), gtk_label_new("x10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_max_range), psf_surface_menu->spin_digit_max, FALSE, FALSE, 0);
	
    GtkWidget *patch_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(patch_box), hbox_draw, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(patch_box), hbox_bar, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(patch_box), hbox_color, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(patch_box), hbox_one_opacity, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(patch_box), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(patch_box), hbox_min_range, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(patch_box), hbox_max_range, TRUE, TRUE, 0);
    
    expander_surf = wrap_into_scroll_expansion_gtk("Surface", 420, 320, window, patch_box);
	return expander_surf;
}
