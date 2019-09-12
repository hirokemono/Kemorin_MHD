/*
//  tree_view_4_light_position.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_light_position.h"

void init_light_views_4_ctl(struct real3_clist *light_list, 
			struct lightparams_view *light_vws){
    light_vws->light_rtp_vws = (struct r3_clist_view *) malloc(sizeof(struct r3_clist_view));
	
    init_r3_clist_views(light_list, light_vws->light_rtp_vws);
    return;
}

void init_light_views_4_viewer(struct phong_lights *lights, struct lightparams_view *light_vws){
	int i, num;
	float r,t, p;
	light_vws->light_rtp_vws = (struct r3_clist_view *) malloc(sizeof(struct r3_clist_view));
	light_vws->light_rtp_vws->r3_clist_gtk = (struct real3_clist *) malloc(sizeof(struct real3_clist));
	init_real3_clist(light_vws->light_rtp_vws->r3_clist_gtk);
	
    sprintf(light_vws->light_rtp_vws->r3_clist_gtk->clist_name, "Ligut position");
    sprintf(light_vws->light_rtp_vws->r3_clist_gtk->r1_name, "radius");
    sprintf(light_vws->light_rtp_vws->r3_clist_gtk->r2_name, "elevation");
	sprintf(light_vws->light_rtp_vws->r3_clist_gtk->r3_name, "azimuth");
	
	light_vws->lights_gtk = lights;
	
	num = send_num_light_position(light_vws->lights_gtk);
	for(i=0;i<num;i++){
		send_each_light_rtp(light_vws->lights_gtk, i, &r, &t, &p);
		append_real3_clist((double) r, (double) t, (double) p, light_vws->light_rtp_vws->r3_clist_gtk);
	}
	return;
}

void dealloc_light_views_4_viewer(struct lightparams_view *light_vws){
	free(light_vws->lights_gtk);
	free(light_vws->light_rtp_vws);
	free(light_vws);
    return;
}

static void sync_phong_light_position_from_list(struct lightparams_view *light_vws){
	double r, t, p;
	int i;
	int num = count_real3_clist(light_vws->light_rtp_vws->r3_clist_gtk);
	dealloc_phong_light_list(light_vws->lights_gtk);
	alloc_phong_light_list(light_vws->lights_gtk, num);
	for(i=0;i<num;i++){
		copy_from_real3_clist(light_vws->light_rtp_vws->r3_clist_gtk,
							  i, &r, &t, &p);
		set_each_light_position(light_vws->lights_gtk,
					i, (float) r, (float) t, (float) p);
	};
	return;
};

void light_radius_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
	
	r3_tree_value1_edited(path_str, new_text, 
				GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk);
	write_real3_clist(stdout, 0, "value1 changed", light_vws->light_rtp_vws->r3_clist_gtk);
	sync_phong_light_position_from_list(light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

void light_theta_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
	
	r3_tree_value2_edited(path_str, new_text, 
				GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk);
	write_real3_clist(stdout, 0, "value2 changed", light_vws->light_rtp_vws->r3_clist_gtk);
	sync_phong_light_position_from_list(light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

void light_phi_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
	
	r3_tree_value3_edited(path_str, new_text, 
				GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk);
	write_real3_clist(stdout, 0, "value3 changed", light_vws->light_rtp_vws->r3_clist_gtk);
	sync_phong_light_position_from_list(light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

void add_lightposition_list_items_cb(GtkButton *button, gpointer user_data){
    struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
	
	light_vws->light_rtp_vws->index_bc
			= add_r3_list_items(light_vws->light_rtp_vws->index_bc, 
				GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk);
    write_real3_clist(stdout, 0, "columns added", light_vws->light_rtp_vws->r3_clist_gtk);
	sync_phong_light_position_from_list(light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};
void delete_lightposition_list_items_cb(GtkButton *button, gpointer user_data){
    struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
	
	delete_r3_list_items(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk);
    write_real3_clist(stdout, 0, "columns deleted", light_vws->light_rtp_vws->r3_clist_gtk);
	sync_phong_light_position_from_list(light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

void add_lightposition_list_box(struct lightparams_view *light_vws, GtkWidget *vbox){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	GtkCellRenderer *renderer_spin3;
	GtkWidget *button_add;
    GtkWidget *button_delete;
	
	light_vws->light_rtp_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_spin_new();
	renderer_spin2 = gtk_cell_renderer_spin_new();
	renderer_spin3 = gtk_cell_renderer_spin_new();
	
	create_real3_tree_view(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view), 
                           light_vws->light_rtp_vws->r3_clist_gtk, 
                           renderer_spin1, renderer_spin2, renderer_spin3);
	
    g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
                     G_CALLBACK(light_radius_edited_cb), (gpointer) light_vws);
    g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
                     G_CALLBACK(light_theta_edited_cb), (gpointer) light_vws);
    g_signal_connect(G_OBJECT(renderer_spin3), "edited", 
                     G_CALLBACK(light_phi_edited_cb), (gpointer) light_vws);
	
	light_vws->light_rtp_vws->index_bc
			= append_r3_list_from_ctl(light_vws->light_rtp_vws->index_bc,
				&light_vws->light_rtp_vws->r3_clist_gtk->r3_item_head, 
				GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view));
	
	button_add = gtk_button_new_with_label("Add");
    button_delete = gtk_button_new_with_label("Remove");
	
	add_real3_list_box(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_lightposition_list_items_cb), (gpointer) light_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_lightposition_list_items_cb), (gpointer) light_vws);
};

void add_light_list_box(struct lightparams_view *light_vws, GtkWidget *vbox){
	GtkWidget *Frame_1;
    GtkWidget *vbox_1, *hbox_1;
	
    init_real3_tree_view(light_vws->light_rtp_vws);
    
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_lightposition_list_box(light_vws, vbox_1);
	
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_1), vbox_1, TRUE, TRUE, 0);
	
	light_vws->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(light_vws->scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(light_vws->scrolled_window, 210, 240);
    gtk_widget_set_app_paintable(light_vws->scrolled_window, TRUE);
    gtk_widget_add_events (light_vws->scrolled_window, GDK_BUTTON_PRESS_MASK);
    gtk_box_pack_start(GTK_BOX(hbox_1), light_vws->scrolled_window, TRUE, TRUE, 0);
	
    
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), hbox_1);
	gtk_box_pack_start(GTK_BOX(vbox), Frame_1, TRUE, TRUE, 0);
};


