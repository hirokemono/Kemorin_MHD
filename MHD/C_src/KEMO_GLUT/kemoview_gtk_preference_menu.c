/*
 *  kemoview_gtk_preference_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_preference_menu.h"

struct preference_gtk_menu * init_preference_gtk_menu(struct kemoviewer_type *kemoviewer_data){
	struct preference_gtk_menu *pref_gmenu
			= (struct preference_gtk_menu *) malloc(sizeof(struct preference_gtk_menu));
	pref_gmenu->lightparams_vws = init_light_views_4_viewer(kemoviewer_data->kemo_buffers->kemo_lights);
	
	return pref_gmenu;
};
void dealloc_preference_gtk_menu(struct preference_gtk_menu *pref_gmenu){
	dealloc_light_views_4_viewer(pref_gmenu->lightparams_vws);
	free(pref_gmenu);
	return;
};

static void kemoview_gtk_BGcolorsel(GtkButton *button, gpointer data){
	float color[4];
	GtkWindow *window = GTK_WINDOW(data);
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(data), "kemoview");

	int iflag_set = kemoview_gtk_colorsel_CB(window, color);
	if(iflag_set > 0){
        kemoview_set_background_color(color, kemo_sgl);
        kemoview_gl_background_color(kemo_sgl);
    };
	
    draw_full(kemo_sgl);
	return;
}

static void NumThreadsChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int ivalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_number_of_threads(ivalue, kemo_sgl);
    draw_full(kemo_sgl);
    return;
}

static void set_coasttube_CB(GtkComboBox *combobox_coasttube, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_coasttube);
    
    kemoview_set_view_integer(COASTLINE_TUBE, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    return;
};

static void nTubeCornerChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int ivalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_view_integer(NUM_TUBE_CORNERS_FLAG, ivalue, kemo_sgl);
    draw_full(kemo_sgl);
    return;
}

static void coasttube_thickness_CB(GtkWidget *entry, gpointer data)
{
    double current_thick;
    int current_digit;
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    
    double thick_in = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    if(thick_in < 0) return;
    
    kemoview_get_coastline_thickness_w_exp(kemo_sgl, &current_thick, &current_digit);
    kemoview_set_coastline_thickness_w_exp(thick_in, current_digit, kemo_sgl);

    draw_full(kemo_sgl);
}

static void coasttube_digit_CB(GtkWidget *entry, gpointer data)
{
    double current_thick;
    int current_digit;
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    
    int in_digit = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
    kemoview_get_coastline_thickness_w_exp(kemo_sgl, &current_thick, &current_digit);
    kemoview_set_coastline_thickness_w_exp(current_thick, in_digit, kemo_sgl);
    
    draw_full(kemo_sgl);
}

static void set_shading_mode_CB(GtkComboBox *combobox_shading, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_shading);
    
    kemoview_set_object_property_flags(SHADING_SWITCH, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    return;
};

static void set_axisposition_CB(GtkComboBox *combobox_axisposition, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_axisposition);
    
    kemoview_set_object_property_flags(AXIS_POSITION, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    return;
};
static void set_surface_direction_CB(GtkComboBox *combobox_surfdir, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_surfdir);
    
    kemoview_set_object_property_flags(POLYGON_SWITCH, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    return;
};


static void AmbientChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(AMBIENT_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void DiffuseChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(DIFFUSE_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void SpecularChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(SPECULAR_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void ShinenessChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	
	kemoview_set_material_parameter(SHINENESS_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}

static void rot_FPS_view_CB(GtkButton *button, gpointer user_data){
    GtkEntry *FPSentry = GTK_ENTRY(user_data);
    GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct rotation_gtk_menu *rot_gmenu
            = (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

    struct kv_string *image_prefix = kemoview_init_kvstring_by_string("CalypsoView");
    
    gtk_window_set_focus(GTK_WINDOW(window), NULL);
    double AverageFPS = draw_rotate_gl_views(kemo_sgl, NO_SAVE_FILE, image_prefix,
                                             rot_gmenu->iaxis_rot, rot_gmenu->inc_deg,
                                             2);
    kemoview_free_kvstring(image_prefix);


    gchar text_AverageFPS[25];
    sprintf(text_AverageFPS, "%f", (float) AverageFPS);
    gtk_entry_set_text(GTK_ENTRY(FPSentry), text_AverageFPS);
    return;
};



static void set_GTK_preference_menu(struct kemoviewer_type *kemoviewer_data,
                                    struct preference_gtk_menu *pref_gmenu){
	double current_value;
	current_value = kemoview_get_material_parameter(kemoviewer_data, AMBIENT_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_ambient), current_value);
	current_value = kemoview_get_material_parameter(kemoviewer_data, DIFFUSE_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_diffuse), current_value);
	current_value = kemoview_get_material_parameter(kemoviewer_data, SPECULAR_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_specular), current_value);
	current_value = kemoview_get_material_parameter(kemoviewer_data, SHINENESS_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_shineness), current_value);
	return;
}

GtkWidget * init_lighting_frame(struct kemoviewer_type *kemoviewer_data,
                                struct preference_gtk_menu *pref_gmenu){
    GtkWidget * light_vbox;
    
    float color[4];
    kemoview_get_background_color(kemoviewer_data, color);
    
    /* Set buttons   */
    float current_value = 0.0;
    GtkAdjustment *adj1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
    pref_gmenu->spin_ambient = gtk_spin_button_new(GTK_ADJUSTMENT(adj1),0,2);
    
    GtkAdjustment *adj2 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
    pref_gmenu->spin_diffuse = gtk_spin_button_new(GTK_ADJUSTMENT(adj2),0,2);
    
    GtkAdjustment *adj3 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
    pref_gmenu->spin_specular = gtk_spin_button_new(GTK_ADJUSTMENT(adj3),0,2);
    
    GtkAdjustment *adj4 = gtk_adjustment_new(current_value, 0.0, 100.0, 0.1, 0.1, 0.0);
    pref_gmenu->spin_shineness = gtk_spin_button_new( GTK_ADJUSTMENT(adj4),0,2);
    
    g_signal_connect(pref_gmenu->spin_ambient, "value-changed",
                     G_CALLBACK(AmbientChange_CB), (gpointer) kemoviewer_data);
    g_signal_connect(pref_gmenu->spin_diffuse, "value-changed",
                     G_CALLBACK(DiffuseChange_CB), (gpointer) kemoviewer_data);
    g_signal_connect(pref_gmenu->spin_specular, "value-changed",
                     G_CALLBACK(SpecularChange_CB), (gpointer) kemoviewer_data);
    g_signal_connect(pref_gmenu->spin_shineness, "value-changed",
                     G_CALLBACK(ShinenessChange_CB), (gpointer) kemoviewer_data);
    
    set_GTK_preference_menu(kemoviewer_data, pref_gmenu);
    pref_gmenu->Frame_BGsel = init_lightposition_expander(kemoviewer_data,
                                                          pref_gmenu->lightparams_vws);

    pref_gmenu->pref_hbox[0] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[0]), gtk_label_new("Ambient:   "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[0]), pref_gmenu->spin_ambient, FALSE, FALSE, 0);
    
    pref_gmenu->pref_hbox[1] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[1]), gtk_label_new("Diffuse:   "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[1]), pref_gmenu->spin_diffuse, FALSE, FALSE, 0);
    
    pref_gmenu->pref_hbox[2] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[2]), gtk_label_new("Specular:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[2]), pref_gmenu->spin_specular, FALSE, FALSE, 0);
    
    pref_gmenu->pref_hbox[3] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[3]), gtk_label_new("Shineness: "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[3]), pref_gmenu->spin_shineness, FALSE, FALSE, 0);
        
    light_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), pref_gmenu->Frame_BGsel, TRUE, TRUE, 0);
    
    gtk_box_pack_start(GTK_BOX(light_vbox), pref_gmenu->pref_hbox[0], FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), pref_gmenu->pref_hbox[1], FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), pref_gmenu->pref_hbox[2], FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), pref_gmenu->pref_hbox[3], FALSE, FALSE, 0);

    light_vbox = wrap_into_frame_gtk("Light parameters", light_vbox);
    return light_vbox;
}

static void init_preference_vbox(struct kemoviewer_type *kemoviewer_data,
                                 struct rotation_gtk_menu *rot_gmenu,
                                 struct preference_gtk_menu *pref_gmenu,
                                 GtkWidget *window){
    float color[4];
	kemoview_get_background_color(kemoviewer_data, color);
	
	/* Set buttons   */
    pref_gmenu->BGselButton = gtk_button_new_with_label("Set Background");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemoviewer_data);
	g_signal_connect(G_OBJECT(pref_gmenu->BGselButton), "clicked",
                     G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)window);

    
    int current_int = kemoview_get_number_of_threads(kemoviewer_data);
    GtkAdjustment *adj_t = gtk_adjustment_new(current_int, 1, 256, 1, 1, 0.0);
    pref_gmenu->spin_threads = gtk_spin_button_new(GTK_ADJUSTMENT(adj_t),0,2);
    g_signal_connect(pref_gmenu->spin_threads, "value-changed",
                     G_CALLBACK(NumThreadsChange_CB), (gpointer) kemoviewer_data);

    pref_gmenu->nthread_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nthread_hbox), gtk_label_new("# of threads:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nthread_hbox), pref_gmenu->spin_threads, FALSE, FALSE, 0);
    GtkWidget * NumThread_frame = wrap_into_frame_gtk("Thread parameter", pref_gmenu->nthread_hbox);

   
    
    GtkWidget * label_tree_coasttube = create_fixed_label_w_index_tree();
    GtkTreeModel * model_coasttube = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_coasttube));
    GtkTreeModel * child_model_coasttube = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_coasttube));
    int index = 0;
    index = append_ci_item_to_tree(index, "Tube",
                                   ON, child_model_coasttube);
    index = append_ci_item_to_tree(index, "Line",
                                   OFF, child_model_coasttube);

    GtkWidget *combobox_coasttube = gtk_combo_box_new_with_model(child_model_coasttube);
    GtkCellRenderer *renderer_coasttube = gtk_cell_renderer_text_new();
    if(kemoview_get_view_integer(kemoviewer_data, COASTLINE_TUBE) == ON){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_coasttube), 0);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_coasttube), 1);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_coasttube),
                               renderer_coasttube, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_coasttube),
                                   renderer_coasttube, "text",
                                   COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_coasttube), "changed",
                G_CALLBACK(set_coasttube_CB), (gpointer) kemoviewer_data);
    
    GtkWidget *hbox_coasttube = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_coasttube), gtk_label_new("Coastline type: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_coasttube), combobox_coasttube, FALSE, FALSE, 0);
    GtkWidget * coastline_frame = wrap_into_frame_gtk("Coastline type", hbox_coasttube);

    int ncorner = kemoview_get_view_integer(kemoviewer_data, NUM_TUBE_CORNERS_FLAG);
    GtkAdjustment *adj_c = gtk_adjustment_new(ncorner, 1, 24, 1, 1, 0.0);
    pref_gmenu->spin_nTubeCorner = gtk_spin_button_new(GTK_ADJUSTMENT(adj_c),0,2);
    g_signal_connect(pref_gmenu->spin_nTubeCorner, "value-changed",
                     G_CALLBACK(nTubeCornerChange_CB), (gpointer) kemoviewer_data);

    double current_thick;
    int    current_digit;
    kemoview_get_coastline_thickness_w_exp(kemoviewer_data, &current_thick, &current_digit);
    GtkWidget *adj_thick = gtk_adjustment_new(current_thick, 0, 9, 1, 1, 0.0);
    GtkWidget *adj_digit = gtk_adjustment_new(current_digit, -30, 30, 1, 1, 0.0);
    GtkWidget *spin_cline_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 0);
    GtkWidget *spin_cline_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
    g_signal_connect(spin_cline_thick, "value-changed",
                     G_CALLBACK(coasttube_thickness_CB), (gpointer) kemoviewer_data);
    g_signal_connect(spin_cline_digit, "value-changed",
                     G_CALLBACK(coasttube_digit_CB), (gpointer) kemoviewer_data);
    

    pref_gmenu->nTubeCorner_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nTubeCorner_hbox), gtk_label_new("# of tube corners:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nTubeCorner_hbox), pref_gmenu->spin_nTubeCorner, FALSE, FALSE, 0);

    GtkWidget *hbox_thickness = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), spin_cline_thick, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("X 10^"), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), spin_cline_digit, TRUE, TRUE, 0);
    
    GtkWidget *vbox_coasttube = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox_coasttube), pref_gmenu->nTubeCorner_hbox, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_coasttube), hbox_thickness, TRUE, TRUE, 0);
    GtkWidget * Tube_frame = wrap_into_frame_gtk("Tube parameters", vbox_coasttube);

    
    
    GtkWidget * label_tree_shading = create_fixed_label_w_index_tree();
    GtkTreeModel * model_shading = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_shading));
    GtkTreeModel * child_model_shading = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_shading));
    index = 0;
    index = append_ci_item_to_tree(index, "Smooth surface", SMOOTH_SHADE, child_model_shading);
    index = append_ci_item_to_tree(index, "Flat surface", FLAT_SHADE, child_model_shading);
    
    GtkWidget *combobox_shading = gtk_combo_box_new_with_model(child_model_shading);
    GtkCellRenderer *renderer_shading = gtk_cell_renderer_text_new();
    if(kemoview_get_object_property_flags(kemoviewer_data, SHADING_SWITCH) == FLAT_SHADE){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_shading), 1);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_shading), 0);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_shading), renderer_shading, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_shading), renderer_shading,
                "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_shading), "changed",
                G_CALLBACK(set_shading_mode_CB), (gpointer) kemoviewer_data);
    
    GtkWidget *hbox_shading = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_shading), gtk_label_new("Shading mode: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_shading), combobox_shading, FALSE, FALSE, 0);


    GtkWidget *label_tree_surf_dir = create_fixed_label_w_index_tree();
    GtkTreeModel *model_surf_dir = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_surf_dir));
    GtkTreeModel *child_model_surf_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_surf_dir));
    index = 0;
    index = append_ci_item_to_tree(index, "Normal surface", NORMAL_POLYGON, child_model_surf_dir);
    index = append_ci_item_to_tree(index, "Reverse surface", REVERSE_POLYGON, child_model_surf_dir);
    
    GtkWidget *combobox_surf_dir = gtk_combo_box_new_with_model(child_model_surf_dir);
    GtkCellRenderer *renderer_surf_dir = gtk_cell_renderer_text_new();
    if(kemoview_get_object_property_flags(kemoviewer_data, POLYGON_SWITCH) == REVERSE_POLYGON){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_surf_dir), 1);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_surf_dir), 0);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir,
                "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_surf_dir), "changed",
                G_CALLBACK(set_surface_direction_CB), (gpointer) kemoviewer_data);
    
    GtkWidget *hbox_surf_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_surf_dir), gtk_label_new("Surface direction: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_surf_dir), combobox_surf_dir, FALSE, FALSE, 0);

    GtkWidget *vbox_shading = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox_shading), hbox_shading, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_shading), hbox_surf_dir, TRUE, TRUE, 0);
    GtkWidget * Shading_frame = wrap_into_frame_gtk("Shading parameters", vbox_shading);

    
    
    GtkWidget * label_tree_axisposition = create_fixed_label_w_index_tree();
    GtkTreeModel * model_axisposition = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_axisposition));
    GtkTreeModel * child_model_axisposition = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_axisposition));
    index = 0;
    index = append_ci_item_to_tree(index, "Center",
                                   OFF, child_model_axisposition);
    index = append_ci_item_to_tree(index, "Lower left",
                                   ON, child_model_axisposition);

    GtkWidget *combobox_axisposition = gtk_combo_box_new_with_model(child_model_axisposition);
    GtkCellRenderer *renderer_axisposition = gtk_cell_renderer_text_new();
    if(kemoview_get_object_property_flags(kemoviewer_data, AXIS_POSITION) == ON){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_axisposition), 1);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_axisposition), 0);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_axisposition),
                               renderer_axisposition, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_axisposition),
                                   renderer_axisposition, "text",
                                   COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_axisposition), "changed",
                G_CALLBACK(set_axisposition_CB), (gpointer) kemoviewer_data);
    
    GtkWidget *hbox_axisposition = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_axisposition), gtk_label_new("Axis position: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_axisposition), combobox_axisposition, FALSE, FALSE, 0);
    GtkWidget *  Axis_frame = wrap_into_frame_gtk("Axis parameters", hbox_axisposition);

    
    pref_gmenu->fpsTextBox = gtk_entry_new();
    gtk_entry_set_width_chars(GTK_ENTRY(pref_gmenu->fpsTextBox), 12);
    gtk_entry_set_text(GTK_ENTRY(pref_gmenu->fpsTextBox), "0.0");
    g_object_set_data(G_OBJECT(pref_gmenu->fpsTextBox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(pref_gmenu->fpsTextBox), "rotation", (gpointer) rot_gmenu);
    g_object_set_data(G_OBJECT(pref_gmenu->fpsTextBox), "kemoview", (gpointer) kemoviewer_data);

    pref_gmenu->fpsButton = gtk_button_new_with_label("Start FPS test");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemoviewer_data);
    g_signal_connect(G_OBJECT(pref_gmenu->fpsButton), "clicked",
                     G_CALLBACK(rot_FPS_view_CB), (gpointer)pref_gmenu->fpsTextBox);

    pref_gmenu->FPStest_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->FPStest_hbox), pref_gmenu->fpsButton,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->FPStest_hbox), gtk_label_new("Average FPS:"), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->FPStest_hbox), pref_gmenu->fpsTextBox, FALSE, FALSE, 0);
    
    GtkWidget * FPS_frame = wrap_into_frame_gtk("FPS test", pref_gmenu->FPStest_hbox);

    GtkWidget *lighting_frame = init_lighting_frame(kemoviewer_data, pref_gmenu);
    
    pref_gmenu->pref_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), NumThread_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->BGselButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), lighting_frame, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), Tube_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), Shading_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), coastline_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), Axis_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), FPS_frame, FALSE, FALSE, 0);
    return;
}

GtkWidget * init_preference_frame(struct kemoviewer_type *kemoviewer_data,
                                  struct rotation_gtk_menu *rot_gmenu,
                                  struct preference_gtk_menu *pref_gmenu,
                                  GtkWidget *window){
    GtkWidget *frame_pref  = gtk_frame_new("Preferences");
    gtk_frame_set_shadow_type(GTK_FRAME(frame_pref), GTK_SHADOW_IN);
    init_preference_vbox(kemoviewer_data, rot_gmenu, pref_gmenu, window);
    gtk_container_add(GTK_CONTAINER(frame_pref), pref_gmenu->pref_vbox);
    return frame_pref;
}

GtkWidget * init_preference_expander(struct kemoviewer_type *kemoviewer_data,
                                     struct rotation_gtk_menu *rot_gmenu,
                                     struct preference_gtk_menu *pref_gmenu,
                                     GtkWidget *window){
    GtkWidget *expander_pref;
    init_preference_vbox(kemoviewer_data, rot_gmenu, pref_gmenu, window);
    expander_pref = wrap_into_scroll_expansion_gtk("Preferences", 160, 400,
                                                   window, pref_gmenu->pref_vbox);
    return expander_pref;
}
