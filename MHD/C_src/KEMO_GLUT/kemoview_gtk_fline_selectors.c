/*
 *  kemoview_gtk_fline_selectors.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_fline_selectors.h"

void kemoview_psf_select_CB(GtkComboBox *combobox_psfs, int id_model, 
                            struct kemoviewer_gl_type *kemo_gl)
{
    int index_mode = gtk_selected_combobox_index(combobox_psfs);
    if(index_mode < 0){index_mode = 0;};
    kemoview_set_PSF_loaded_params(SET_CURRENT, index_mode, kemo_gl->kemoview_data);

    kemoview_set_VIZ_field_param(IZERO,
                                 SURFACE_RENDERING,
                                 FIELD_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    kemoview_set_VIZ_field_param(IZERO,
                                 SURFACE_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 kemo_gl->kemoview_data);
	return;
};

void kemoview_field_select_CB(GtkComboBox *combobox_field, int id_model, 
                              struct kemoviewer_gl_type *kemo_gl)
{
    int index_mode = gtk_selected_combobox_index(combobox_field);
    int num_fld = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                               id_model, NUM_FIELD_FLAG);
    if(index_mode >= num_fld || index_mode < 0){
        index_mode = 0;
    }
    
    kemoview_set_VIZ_field_param(index_mode,
                                 id_model, FIELD_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    kemoview_set_VIZ_field_param(IZERO,
                                 id_model, COMPONENT_SEL_FLAG,
                                 kemo_gl->kemoview_data);
	return;
};

void kemoview_component_select_CB(GtkComboBox *combobox_comp, int id_model, 
                                  struct kemoviewer_gl_type *kemo_gl)
{
    int index_mode = gtk_selected_combobox_index(combobox_comp);
    int if_psf = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                              id_model,
                                              FIELD_SEL_FLAG);
    int ncomp = (int) kemoview_get_VIZ_num_component(kemo_gl->kemoview_data,
                                                     id_model, if_psf);
    if(index_mode >= ncomp || index_mode < 0){
        index_mode = 0;
    }
	
    kemoview_set_VIZ_field_param(index_mode,
                                 id_model, COMPONENT_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    return;
};



GtkWidget * draw_current_psf_set_hbox(int id_current_psf,
                                      struct kemoviewer_gl_type *kemo_gl,
                                      int *index){
    GtkWidget * combobox_psfs;
    
	struct kv_string *stripped_filehead;
	char label_tmp[512];
	int id_current = kemoview_get_PSF_loaded_params(kemo_gl->kemoview_data,
                                                    SET_CURRENT);
    GtkWidget *psf_label_tree_view = create_fixed_label_w_index_tree();
    GtkTreeModel *model_psfs = gtk_tree_view_get_model(GTK_TREE_VIEW(psf_label_tree_view));
    GtkTreeModel *child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_psfs));
    
    *index = 0;
    int ipsf;
    int index_current = 0;
    int nmax_loaded = kemoview_get_PSF_loaded_params(kemo_gl->kemoview_data, MAX_LOADED);
    for(ipsf=0; ipsf<nmax_loaded; ipsf++){
        if(ipsf == id_current) {index_current = *index;};
        if(kemoview_get_PSF_loaded_flag(kemo_gl->kemoview_data, ipsf) > 0) {
            kemoview_set_PSF_loaded_params(SET_CURRENT, ipsf, kemo_gl->kemoview_data);
            stripped_filehead = kemoview_alloc_kvstring();
            kemoview_get_PSF_file_prefix(kemo_gl, stripped_filehead);
            sprintf(label_tmp, "%d: %s", ipsf, stripped_filehead->string);
            *index = append_ci_item_to_tree(*index, label_tmp,
                                            ipsf, child_model_psfs);
            kemoview_free_kvstring(stripped_filehead);
        };
        kemoview_set_PSF_loaded_params(SET_CURRENT, id_current, kemo_gl->kemoview_data);
    };

    GtkCellRenderer *renderer_psfs = gtk_cell_renderer_text_new();
    combobox_psfs = gtk_combo_box_new_with_model(child_model_psfs);
    gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_psfs), index_current);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs,
                                   "text", COLUMN_FIELD_NAME, NULL);
    return combobox_psfs;
}


GtkWidget * draw_viz_field_gtk_box(struct kemoviewer_gl_type *kemo_gl, int id_model){
	int num_field = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data, id_model,
                                                 NUM_FIELD_FLAG);
	int if_fline =  kemoview_get_VIZ_field_param(kemo_gl->kemoview_data, id_model,
                                                 FIELD_SEL_FLAG);
	int ifld;
	
	GtkWidget *label_tree_field = create_fixed_label_w_index_tree();
	GtkTreeModel *model_field =       gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_field));  
	GtkTreeModel *child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
	int index = 0;
	for(ifld=0;ifld<num_field;ifld++){
        struct kv_string *colorname = kemoview_alloc_kvstring();
        kemoview_get_VIZ_field_name(kemo_gl->kemoview_data, id_model,
                                    colorname, ifld);
		index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
        kemoview_free_kvstring(colorname);
    };
	
    GtkCellRenderer *renderer_field = gtk_cell_renderer_text_new();
	GtkWidget *combobox_field = gtk_combo_box_new();
    gtk_combo_box_set_model(GTK_COMBO_BOX(combobox_field), child_model_field);
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_field), if_fline);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), renderer_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), renderer_field,
                                   "text", COLUMN_FIELD_NAME, NULL);
    return combobox_field;
}


GtkWidget * draw_viz_component_gtk_box(struct kemoviewer_gl_type *kemo_gl, int id_model){
	char comp_name[128];
    
    int if_fline = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                id_model, FIELD_SEL_FLAG);
	int ic_fline = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                id_model, COMPONENT_SEL_FLAG);
	int ncomp = (int) kemoview_get_VIZ_num_component(kemo_gl->kemoview_data,
                                                     id_model, if_fline);
	
    GtkWidget *label_tree_comp = create_fixed_label_w_index_tree();
    GtkTreeModel *model_comp =       gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_comp));
    GtkTreeModel *child_model_comp = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));
    int id_coord = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                id_model, COORDINATE_FLAG);
    int icomp;
    int index = 0;
    for(icomp=0;icomp<ncomp;icomp++){
        set_PSF_component_name(ncomp, id_coord, icomp, comp_name);
        index = append_ci_item_to_tree(index, comp_name, icomp, child_model_comp);
    };
    
    GtkWidget *combobox_comp = gtk_combo_box_new_with_model(child_model_comp);
    GtkCellRenderer *renderer_comp = gtk_cell_renderer_text_new();
    gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_comp), ic_fline);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), renderer_comp, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), renderer_comp,
                                   "text", COLUMN_FIELD_NAME, NULL);
	return combobox_comp;
}

