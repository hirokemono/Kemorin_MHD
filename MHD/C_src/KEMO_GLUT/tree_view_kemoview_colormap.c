/*
//  tree_view_kemoview_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_kemoview_colormap.h"

static gboolean kemoview_draw_colorabar_CB(GtkWidget *widget, cairo_t *cr, gpointer user_data)
{ 
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	return expose_event_CB(cr, color_vws);
}

static void kemoview_colormap_data_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	colormap_data_edited_CB(path_str, new_text, color_vws);
	
	double value, color;
	int num = count_real2_clist(color_vws->cmap_vws->r2_clist_gtk);
	
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->cmap_vws->r2_clist_gtk, &value, &color);
		kemoview_set_PSF_color_data(i, value, color);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};

static void kemoview_colormap_color_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	colormap_color_edited_CB(path_str, new_text, color_vws);
	
	double value, color;
	int num = count_real2_clist(color_vws->cmap_vws->r2_clist_gtk);
	
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->cmap_vws->r2_clist_gtk, &value, &color);
		kemoview_set_PSF_color_data(i, value, color);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};

static void add_kemoview_colormap_list_items_CB(GtkButton *button, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	add_colormap_list_items_CB(color_vws);
	
	kemoview_add_PSF_color_list(0.0, 0.0);
	double value, color;
	int num = count_real2_clist(color_vws->cmap_vws->r2_clist_gtk);
	
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->cmap_vws->r2_clist_gtk, &value, &color);
		kemoview_set_PSF_color_data(i, value, color);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};


static void delete_kemoview_colormap_list_items_CB(GtkButton *button, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	delete_colormap_list_items_CB(color_vws);
	double value, color;
	
	int num = count_real2_clist(color_vws->cmap_vws->r2_clist_gtk);
	
	if(num > 2) kemoview_delete_PSF_color_list(num);
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->cmap_vws->r2_clist_gtk, &value, &color);
		kemoview_set_PSF_color_data(i, value, color);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};



static void kemoview_opacity_data_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	opacity_data_edited_CB(path_str, new_text, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
	
	
	double value, opacity;
	int num = count_real2_clist(color_vws->opacity_vws->r2_clist_gtk);
	
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->opacity_vws->r2_clist_gtk, &value, &opacity);
		kemoview_set_PSF_opacity_data(i, value, opacity);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};

static void kemoview_opacity_color_edited_CB(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	opacity_color_edited_CB(path_str, new_text, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
	
	
	double value, opacity;
	int num = count_real2_clist(color_vws->opacity_vws->r2_clist_gtk);
	
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->opacity_vws->r2_clist_gtk, &value, &opacity);
		kemoview_set_PSF_opacity_data(i, value, opacity);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};

static void add_kemoview_opacity_list_items_CB(GtkButton *button, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	add_opacity_list_items_CB(color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
	
	
	double value, opacity;
	int num = count_real2_clist(color_vws->opacity_vws->r2_clist_gtk);
	
	kemoview_add_PSF_opacity_list(0.0, 0.0);
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->opacity_vws->r2_clist_gtk, &value, &opacity);
		kemoview_set_PSF_opacity_data(i, value, opacity);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};

static void delete_kemoview_opacity_list_items_CB(GtkButton *button, gpointer user_data){
	int i;
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	delete_opacity_list_items_CB(color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
	
	
	double value, opacity;
	int num = count_real2_clist(color_vws->opacity_vws->r2_clist_gtk);
	if(num > 2) kemoview_delete_PSF_opacity_list(num);
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, color_vws->opacity_vws->r2_clist_gtk, &value, &opacity);
		kemoview_set_PSF_opacity_data(i, value, opacity);
	};
	gtk_widget_queue_draw(color_vws->scrolled_window);
	draw_full();
};

static void set_kemoview_color_mode_CB(GtkComboBox *combobox_cmap, gpointer user_data)
{
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	int index_mode = set_color_mode_CB(combobox_cmap, color_vws);
	
	kemoview_set_PSF_color_param(ISET_COLORMAP, index_mode);
    gtk_widget_queue_draw(color_vws->scrolled_window);
    return;
}


static void add_kemoview_cmap_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	GtkWidget *button_add;
    GtkWidget *button_delete;
	
	color_vws->cmap_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_text_new();
	renderer_spin2 = gtk_cell_renderer_text_new();
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(kemoview_colormap_data_edited_CB), (gpointer) color_vws);
	g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
					 G_CALLBACK(kemoview_colormap_color_edited_CB), (gpointer) color_vws);
	
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
                     G_CALLBACK(add_kemoview_colormap_list_items_CB), (gpointer) color_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_kemoview_colormap_list_items_CB), (gpointer) color_vws);
};

static void add_kemoview_omap_list_box(struct colormap_view *color_vws,
			GtkWidget *vbox){
	color_vws->opacity_vws->tree_view = gtk_tree_view_new();
	GtkCellRenderer *renderer_spin1 = gtk_cell_renderer_text_new();
	GtkCellRenderer *renderer_spin2 = gtk_cell_renderer_text_new();
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(kemoview_opacity_data_edited_CB), (gpointer) color_vws);
	g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
					 G_CALLBACK(kemoview_opacity_color_edited_CB), (gpointer) color_vws);
		
	create_real2_tree_view(GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), 
                           color_vws->opacity_vws->r2_clist_gtk, 
                           renderer_spin1, renderer_spin2);
	
	color_vws->opacity_vws->index_bc = append_r2_list_from_ctl(color_vws->opacity_vws->index_bc,
				&color_vws->opacity_vws->r2_clist_gtk->r2_item_head, 
				GTK_TREE_VIEW(color_vws->opacity_vws->tree_view));
	
	GtkWidget *button_add = gtk_button_new_with_label("ADD");
	GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	add_real2_list_box(GTK_TREE_VIEW(color_vws->opacity_vws->tree_view),
				color_vws->opacity_vws->r2_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_kemoview_opacity_list_items_CB), (gpointer) color_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_kemoview_opacity_list_items_CB), (gpointer) color_vws);
};

GtkWidget * init_kemoview_colormap_list_vbox(struct colormap_view *color_vws){
	GtkWidget *vbox_cmap;
	
	init_real2_tree_view(color_vws->cmap_vws);
	init_real2_tree_view(color_vws->opacity_vws);
	
	GtkWidget *label_tree = create_fixed_label_w_index_tree();
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	
	int index = 0;
	index = append_ci_item_to_tree(index, &color_labels[RAINBOW_MODE][0], RAINBOW_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[GRAYSCALE_MODE][0], GRAYSCALE_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[RED_BLUE_MODE][0], RED_BLUE_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[SYM_GRAY_MODE][0], SYM_GRAY_MODE, child_model);
	
	GtkWidget *combobox_cmap = gtk_combo_box_new_with_model(child_model);
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
	int iflag = kemoview_get_PSF_color_param(ISET_COLORMAP);
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
                     G_CALLBACK(set_kemoview_color_mode_CB), (gpointer) color_vws);
	
	
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox_1), combobox_cmap, FALSE, TRUE, 0);
	
	add_kemoview_cmap_list_box(color_vws, vbox_1);
	add_kemoview_omap_list_box(color_vws, vbox_1);
	
	GtkWidget *hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_1), vbox_1, FALSE, TRUE, 0);
	
	color_vws->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(color_vws->scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(color_vws->scrolled_window, 140, 300);
    gtk_widget_set_app_paintable(color_vws->scrolled_window, TRUE);
    gtk_widget_add_events (color_vws->scrolled_window, GDK_BUTTON_PRESS_MASK);
	g_signal_connect(G_OBJECT(color_vws->scrolled_window), "draw", 
				G_CALLBACK(kemoview_draw_colorabar_CB), (gpointer) color_vws);
    gtk_box_pack_start(GTK_BOX(hbox_1), color_vws->scrolled_window, TRUE, TRUE, 0);
	
	GtkWidget *Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), hbox_1);
	
	vbox_cmap = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_cmap), Frame_1, TRUE, TRUE, 0);
	return vbox_cmap;
};


