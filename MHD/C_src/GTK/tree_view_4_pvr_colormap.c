/*
//  tree_view_4_pvr_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_pvr_colormap.h"

// static cairo_t *cr;

gboolean pvr_draw_colorabar_CB(GtkWidget *widget, cairo_t *cr, gpointer user_data)
{ 
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
	return expose_event_CB(cr, color_vws);
}

static void pvr_colormap_data_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	colormap_data_edited_CB(path_str, new_text, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

static void pvr_colormap_color_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	colormap_color_edited_CB(path_str, new_text, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

static void add_pvr_colormap_list_items_CB(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	add_colormap_list_items_CB(color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

void delete_pvr_colormap_list_items_CB(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	delete_colormap_list_items_CB(color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

static void pvr_opacity_data_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	opacity_data_edited_CB(path_str, new_text, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

static void pvr_opacity_color_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	opacity_color_edited_CB(path_str, new_text, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

static void add_pvr_opacity_list_items_CB(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	add_opacity_list_items_CB(color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};
void delete_pvr_opacity_list_items_CB(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	delete_opacity_list_items_CB(color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

void set_pvr_color_mode_CB(GtkComboBox *combobox_cmap, gpointer user_data)
{
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	int index_mode = set_color_mode_CB(combobox_cmap, color_vws);
    gtk_widget_queue_draw(color_vws->scrolled_window);
    return;
}

void add_pvr_cmap_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	GtkWidget *button_add;
    GtkWidget *button_delete;
	
	color_vws->cmap_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_text_new();
	renderer_spin2 = gtk_cell_renderer_text_new();
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(pvr_colormap_data_edited_CB), (gpointer) color_vws);
	g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
					 G_CALLBACK(pvr_colormap_color_edited_CB), (gpointer) color_vws);
		
	create_real2_tree_view(GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), 
                           color_vws->cmap_vws->r2_clist_gtk, 
                           renderer_spin1, renderer_spin2);
	
	color_vws->cmap_vws->index_bc = append_r2_list_from_ctl(color_vws->cmap_vws->index_bc,
				&color_vws->cmap_vws->r2_clist_gtk->r2_item_head, 
				GTK_TREE_VIEW(color_vws->cmap_vws->tree_view));
	
	button_add = gtk_button_new_with_label("ADD");
    button_delete = gtk_button_new_with_label("Remove");
	
	add_real2_list_box(GTK_TREE_VIEW(color_vws->cmap_vws->tree_view),
				color_vws->cmap_vws->r2_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_pvr_colormap_list_items_CB), (gpointer) color_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_pvr_colormap_list_items_CB), (gpointer) color_vws);
};

void add_pvr_omap_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	GtkWidget *button_add;
    GtkWidget *button_delete;
	
	color_vws->opacity_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_spin_new();
	renderer_spin2 = gtk_cell_renderer_spin_new();
	
	create_real2_tree_view(GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), 
                           color_vws->opacity_vws->r2_clist_gtk, 
                           renderer_spin1, renderer_spin2);
	
    g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
                     G_CALLBACK(pvr_opacity_data_edited_CB), (gpointer) color_vws);
    g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
                     G_CALLBACK(pvr_opacity_color_edited_CB), (gpointer) color_vws);
	
	color_vws->opacity_vws->index_bc = append_r2_list_from_ctl(color_vws->opacity_vws->index_bc,
				&color_vws->opacity_vws->r2_clist_gtk->r2_item_head, 
				GTK_TREE_VIEW(color_vws->opacity_vws->tree_view));
	
	button_add = gtk_button_new_with_label("ADD");
    button_delete = gtk_button_new_with_label("Remove");
	add_real2_list_box(GTK_TREE_VIEW(color_vws->opacity_vws->tree_view),
				color_vws->opacity_vws->r2_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_pvr_opacity_list_items_CB), (gpointer) color_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_pvr_opacity_list_items_CB), (gpointer) color_vws);
};

GtkWidget * add_pvr_colormap_list_box_2(struct colormap_view *color_vws){
    GtkWidget *frame_cmap;
    color_vws->combobox_cmap = init_combobox_cmap(IZERO);
    g_signal_connect(G_OBJECT(color_vws->combobox_cmap), "changed",
                     G_CALLBACK(set_pvr_color_mode_CB), (gpointer) color_vws);
	
	
    color_vws->vbox_cmap = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(color_vws->vbox_cmap), color_vws->combobox_cmap, FALSE, FALSE, 0);
	
	add_pvr_cmap_list_box(color_vws, color_vws->vbox_cmap);
	add_pvr_omap_list_box(color_vws, color_vws->vbox_cmap);
	
    color_vws->hbox_cmap = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(color_vws->hbox_cmap), color_vws->vbox_cmap,
                       TRUE, TRUE, 0);
	
	color_vws->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(color_vws->scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(color_vws->scrolled_window, 300, 300);
    gtk_widget_set_app_paintable(color_vws->scrolled_window, TRUE);
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


