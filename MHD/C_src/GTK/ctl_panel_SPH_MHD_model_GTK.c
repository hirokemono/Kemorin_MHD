/*
//  ctl_panel_SPH_MHD_model_GTK.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#include "ctl_panel_SPH_MHD_model_GTK.h"

GtkWidget * make_parallel_shell_hbox(struct parallel_sph_shell_control_c *shell_ctl){
	int i;
	char *c_label;
	
	GtkWidget *hbox_2[NLBL_SPH_SHELL_CTL], *hbox_3[NLBL_SPH_SHELL_CTL];
	GtkWidget *expander_SPH_ctl[NLBL_SPH_SHELL_CTL];
	
	GtkWidget *vbox_sph_shell = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);;
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *Frame = gtk_frame_new("");
	
	for (i=0;i<NLBL_SPH_SHELL_CTL;i++){
		hbox_3[i] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
		expander_SPH_ctl[i] = gtk_expander_new("");
	};
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	get_label_sph_shell_ctl(0, c_label);
	add_control_block_box(c_label, &shell_ctl->iflag_FEM_mesh_ctl,
				hbox_3[0], expander_SPH_ctl[0]);
	hbox_2[0] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	free(c_label);
	
	gtk_widget_set_size_request(scrolled_window, 300, 400);
	gtk_scrolled_window_set_max_content_height(scrolled_window, 400);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 10);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	
	
	for(i=0;i<NLBL_SPH_SHELL_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_sph_shell), hbox_3[i], FALSE, FALSE, 0);
		gtk_container_add(GTK_CONTAINER(expander_SPH_ctl[i]), hbox_2[i]);
		gtk_box_pack_start(GTK_BOX(vbox_sph_shell), expander_SPH_ctl[i], TRUE, TRUE, 0);
	};
	gtk_scrolled_window_add_with_viewport (
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_sph_shell);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, FALSE, FALSE, 0);
	
	gtk_frame_set_shadow_type(GTK_FRAME(Frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame), vbox);
	
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(""), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), Frame, TRUE, TRUE, 0);
	return hbox;
}
