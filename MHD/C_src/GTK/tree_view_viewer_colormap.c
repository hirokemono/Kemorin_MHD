/*
//  tree_view_viewer_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_viewer_colormap.h"

void init_colormap_views_4_viewer(struct colormap_view *color_vws){
	color_vws->cmap_param = (struct colormap_params *) kemoview_link_active_colormap_param();
	
	color_vws->colormap_mode_gtk = init_chara_ctl_item_c();
	
    if(color_vws->iflag_cmap_loaded > 0){
        dealloc_r2_clist_views(color_vws->cmap_vws);
        dealloc_r2_clist_views(color_vws->opacity_vws);
    }
	color_vws->cmap_vws =    alloc_r2_clist_views();
	color_vws->opacity_vws = alloc_r2_clist_views();
    color_vws->iflag_cmap_loaded = 1;
	
	sprintf(color_vws->cmap_vws->r2_clist_gtk->clist_name, "color map");
    sprintf(color_vws->cmap_vws->r2_clist_gtk->r1_name, "data");
    sprintf(color_vws->cmap_vws->r2_clist_gtk->r2_name, "color");
    sprintf(color_vws->opacity_vws->r2_clist_gtk->clist_name, "opacity map");
    sprintf(color_vws->opacity_vws->r2_clist_gtk->r1_name, "data");
    sprintf(color_vws->opacity_vws->r2_clist_gtk->r2_name, "opacity");
    
	int i, num;
	double value, color;
	copy_colormap_name_to_ctl(color_vws->cmap_param, 
                              color_vws->colormap_mode_gtk);
	num = send_color_table_num_s(color_vws->cmap_param);
	for(i=0;i<num;i++){
		send_color_table_items_s(color_vws->cmap_param, i, &value, &color);
		append_real2_clist(value, color, color_vws->cmap_vws->r2_clist_gtk);
	};
	
	num = send_opacity_table_num_s(color_vws->cmap_param);
	for(i=0;i<num;i++){
		send_opacity_table_items_s(color_vws->cmap_param, i, &value, &color);
		append_real2_clist(value, color, color_vws->opacity_vws->r2_clist_gtk);
	};
	
	return;
};

void load_color_opacity_map_from_list(struct psf_menu_val *psf_current_menu,
			struct colormap_view *color_vws){
	int icomp = psf_current_menu->icomp_draw_psf;
	dup_real2_clist(color_vws->cmap_vws->r2_clist_gtk,
					psf_current_menu->cmap_psf_comp[icomp]->colormap);
	dup_real2_clist(color_vws->opacity_vws->r2_clist_gtk,
					psf_current_menu->cmap_psf_comp[icomp]->opacitymap);
	return;
}

void add_pvr_colormap_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkWidget *Frame_1;
    GtkWidget *vbox_1, *hbox_1;
	
	GtkWidget *combobox_cmap;
	GtkWidget *label_tree;
	GtkCellRenderer *renderer;
	GtkTreeModel *model;
    GtkTreeModel *child_model;
    int index = 0;
	int iflag;

    init_real2_tree_view(color_vws->cmap_vws);
    init_real2_tree_view(color_vws->opacity_vws);
    
	label_tree = create_fixed_label_w_index_tree();
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    index = append_ci_item_to_tree(index, &color_labels[RAINBOW_MODE][0], RAINBOW_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[GRAYSCALE_MODE][0], GRAYSCALE_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[RED_BLUE_MODE][0], RED_BLUE_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[SYM_GRAY_MODE][0], SYM_GRAY_MODE, child_model);
	
	combobox_cmap = gtk_combo_box_new_with_model(child_model);
	renderer = gtk_cell_renderer_text_new();
	iflag = kemoview_get_PSF_color_param(ISET_COLORMAP);
	if(iflag == SYM_GRAY_MODE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_cmap), 3);
	} else if(iflag == RED_BLUE_MODE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_cmap), 2);
	} else if(iflag == GRAYSCALE_MODE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_cmap), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_cmap), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_cmap), renderer, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_cmap), renderer,
				"text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_cmap), "changed", 
                     G_CALLBACK(set_pvr_color_mode_CB), (gpointer) color_vws);
	
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox_1), combobox_cmap, FALSE, FALSE, 0);
	
	add_pvr_cmap_list_box(color_vws, vbox_1);
	add_pvr_omap_list_box(color_vws, vbox_1);
	
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_1), vbox_1, TRUE, TRUE, 0);
	
	color_vws->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(color_vws->scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(color_vws->scrolled_window, 300, 300);
    gtk_widget_set_app_paintable(color_vws->scrolled_window, TRUE);
    gtk_widget_add_events (color_vws->scrolled_window, GDK_BUTTON_PRESS_MASK);
	g_signal_connect(G_OBJECT(color_vws->scrolled_window), "draw", 
				G_CALLBACK(pvr_draw_colorabar_CB), (gpointer) color_vws);
    gtk_box_pack_start(GTK_BOX(hbox_1), color_vws->scrolled_window, TRUE, TRUE, 0);
	
    
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), hbox_1);
	gtk_box_pack_start(GTK_BOX(vbox), Frame_1, TRUE, TRUE, 0);
};


