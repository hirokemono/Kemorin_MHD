/*
//  tree_view_viewer_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_viewer_colormap.h"

static void update_colormap_params_4_viewer(struct kemoviewer_gl_type *kemo_gl,
                                            struct colormap_view *color_vws){
    color_vws->cmap_param
        = (struct colormap_params *) kemoview_link_active_colormap_param(color_vws->iflag_current_model,
                                                                         kemo_gl);
    
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
    
    clear_real2_clist(color_vws->cmap_vws->r2_clist_gtk);
    num = get_color_table_num_s(color_vws->cmap_param);
    for(i=0;i<num;i++){
        get_color_table_items_s(color_vws->cmap_param, i, &value, &color);
        append_real2_clist(value, color, color_vws->cmap_vws->r2_clist_gtk);
    };
    
    clear_real2_clist(color_vws->opacity_vws->r2_clist_gtk);
    num = get_opacity_table_num_s(color_vws->cmap_param);
    for(i=0;i<num;i++){
        get_opacity_table_items_s(color_vws->cmap_param, i, &value, &color);
        append_real2_clist(value, color, color_vws->opacity_vws->r2_clist_gtk);
    };
    
    return;
}

void init_colormap_params_4_viewer(int id_model,
                                   struct kemoviewer_gl_type *kemo_gl,
                                   struct colormap_view *color_vws){
	color_vws->colormap_mode_gtk = init_chara_ctl_item_c();
    color_vws->cmap_vws =    alloc_r2_clist_views();
    color_vws->opacity_vws = alloc_r2_clist_views();
    
    color_vws->iflag_current_model = id_model;
    update_colormap_params_4_viewer(kemo_gl, color_vws);
    return;
};

void load_color_opacity_map_from_list(struct psf_menu_val *psf_current_menu,
			struct colormap_view *color_vws){
	long icomp = psf_current_menu->icomp_draw_viz;
	dup_real2_clist(color_vws->cmap_vws->r2_clist_gtk,
					psf_current_menu->cmap_viz_comp[icomp]->colormap);
	dup_real2_clist(color_vws->opacity_vws->r2_clist_gtk,
					psf_current_menu->cmap_viz_comp[icomp]->opacitymap);
	return;
}

GtkWidget * add_pvr_colormap_list_box(struct kemoviewer_type *kemo_sgl,
                                      struct colormap_view *color_vws){
    GtkWidget *frame_cmap;
	int iflag = kemoview_get_viz_colormap_param(kemo_sgl,
                                                SURFACE_RENDERING,
                                                ISET_COLORMAP);
    color_vws->combobox_cmap = init_combobox_cmap(iflag);
    g_signal_connect(G_OBJECT(color_vws->combobox_cmap), "changed",
                     G_CALLBACK(set_pvr_color_mode_CB), (gpointer) color_vws);
	
	
    color_vws->vbox_cmap = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(color_vws->vbox_cmap), color_vws->combobox_cmap,
                       FALSE, FALSE, 0);
	
	add_pvr_cmap_list_box(color_vws, color_vws->vbox_cmap);
	add_pvr_omap_list_box(color_vws, color_vws->vbox_cmap);
	
    color_vws->hbox_cmap = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(color_vws->hbox_cmap), color_vws->vbox_cmap,
                       TRUE, TRUE, 0);
	
	color_vws->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(color_vws->scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(color_vws->scrolled_window, 300, 300);
//    gtk_widget_set_app_paintable(color_vws->scrolled_window, TRUE);
    gtk_widget_add_events (color_vws->scrolled_window, GDK_BUTTON_PRESS_MASK);
	g_signal_connect(G_OBJECT(color_vws->scrolled_window), "draw", 
				G_CALLBACK(pvr_draw_colorabar_CB), (gpointer) color_vws);
    gtk_box_pack_start(GTK_BOX(color_vws->hbox_cmap), color_vws->scrolled_window,
                       TRUE, TRUE, 0);
	
    
    frame_cmap = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(frame_cmap), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(frame_cmap), color_vws->hbox_cmap);
    return frame_cmap;
};


