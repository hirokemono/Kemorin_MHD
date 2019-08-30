/*
 *  kemoview_gtk_mesh_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_mesh_menu.h"

GtkWidget *window_mesh;

static void kemoview_BG_cancel(GtkButton *button, gpointer data){
	gtk_widget_destroy(window_mesh);
	gtk_main_quit();
	return;
};

static void kemoview_BG_close(GtkButton *button, gpointer data){
	kemoview_close_mesh_view();
	gtk_widget_destroy(window_mesh);
	gtk_main_quit();
	return;
};

static void subdoain_distance_CB(GtkWidget *entry, gpointer data)
{
	double dist = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(dist >= 0.0) kemoview_set_domain_distance(dist);
	kemoview_draw_with_modified_domain_distance();
/*	printf("dist %d\n", dist);*/
}

static void node_size_CB(GtkWidget *entry, gpointer data)
{
	double size = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(size >= 0.0) kemoview_set_node_diamater(size);
/*	printf("size %d\n", size);*/
}

void gtk_mesh_menu(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *hbox, *vbox;
	
	GtkWidget *entry;
	GtkWidget *closeMeshButton, *cancelButton;
	
	GtkWidget *hbox_distance, *hbox_org_dist;
	GtkWidget *spin_dist;
	GtkAdjustment *adj_dist;
	double current_distance;
	char current_dist_text[30];
	
	GtkWidget *hbox_node_size, *hbox_org_size;
	GtkWidget *spin_node_size;
	GtkAdjustment *adj_node_size;
	double current_size;
	char current_size_text[30];
	
	
	struct kemoview_mesh_view *mesh_vws;
	
	
	window_mesh = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(window_mesh), "Mesh viewer");
	gtk_widget_set_size_request(window_mesh, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window_mesh), 5);
	g_signal_connect(G_OBJECT(window_mesh), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
		
	/* Set buttons   */
	entry = gtk_entry_new();
	closeMeshButton = gtk_button_new_with_label("Close mesh");
	g_signal_connect(G_OBJECT(closeMeshButton), "clicked", 
				G_CALLBACK(kemoview_BG_close), (gpointer)entry);
	cancelButton = gtk_button_new_with_label("Update");
	g_signal_connect(G_OBJECT(cancelButton), "clicked", 
				G_CALLBACK(kemoview_BG_cancel), (gpointer)entry);
	
	mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
    init_mesh_views_4_viewer(kemoviewer_data->kemo_mesh->mesh_d, mesh_vws);
	
	current_distance = kemoview_get_domain_distance();
	sprintf(current_dist_text, "    %e    ", current_distance);
	adj_dist = gtk_adjustment_new(current_distance, 0.0, 10.0, 0.005, 0.005, 0.0);
	spin_dist = gtk_spin_button_new(GTK_ADJUSTMENT(adj_dist), 0, 3);
	g_signal_connect(spin_dist, "value-changed", G_CALLBACK(subdoain_distance_CB),NULL);
	
	current_size = kemoview_get_node_diamater();
	sprintf(current_size_text, "    %e    ", current_size);
	adj_node_size = gtk_adjustment_new(current_size, 0.0, 10.0, 0.00001, 0.00001, 0.0);
	spin_node_size = gtk_spin_button_new(GTK_ADJUSTMENT(adj_node_size), 0, 3);
	g_signal_connect(spin_node_size, "value-changed", G_CALLBACK(node_size_CB),NULL);
	
	
	hbox_org_dist = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_dist), gtk_label_new("Current distance: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_dist), gtk_label_new(current_dist_text), TRUE, TRUE, 0);
	hbox_distance = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_distance), gtk_label_new("Subdomain distance: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_distance), spin_dist, TRUE, TRUE, 0);
	
	hbox_org_size = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_size), gtk_label_new("Current distance: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_size), gtk_label_new(current_size_text), TRUE, TRUE, 0);
	hbox_node_size = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), gtk_label_new("Node size: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), spin_node_size, TRUE, TRUE, 0);
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(window_mesh), vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_org_dist, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_distance, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_org_size, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_node_size, TRUE, TRUE, 0);
	
	add_domain_draw_box(mesh_vws->domain_vws, window_mesh, vbox);
	add_nod_group_draw_box(mesh_vws->nod_grp_vws, window_mesh, vbox);
	add_ele_group_draw_box(mesh_vws->ele_grp_vws, window_mesh, vbox);
	add_surf_group_draw_box(mesh_vws->surf_grp_vws, window_mesh, vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), closeMeshButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), cancelButton, FALSE, FALSE, 0);
	
	
	gtk_widget_show_all(window_mesh);
	gtk_main();
	dealloc_mesh_views_4_viewer(mesh_vws);
	return;
}
