/*
//  ctl_panel_para_sph_shell_GTK.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#include "ctl_panel_para_sph_shell_GTK.h"

void expander_switch_cb(GObject *switch_3, GParamSpec *pspec, gpointer data){
    int *iflag = (int *) data;
	
	if(gtk_switch_get_state(switch_3) == TRUE){
		gtk_switch_set_state(switch_3, TRUE);
        *iflag = 1;
	} else {
		gtk_switch_set_state(switch_3, FALSE);
        *iflag = 0;
	};
};

void expander_action_cb(GObject *switch_3, gpointer data){
    struct GtkWidget *expender = (GtkWidget *) switch_3;
    int *iflag = (int *) data;
	
	if(*iflag == 0){
		gtk_expander_set_expanded(GTK_EXPANDER(expender), TRUE);
    };
};

void add_control_block_box(char *c_label, int *iflag_box, 
			GtkWidget *hbox, GtkWidget *expander_b){
	GtkWidget *switch_b = gtk_switch_new();
	gtk_switch_set_active(GTK_SWITCH(switch_b), TRUE);
	if(*iflag_box > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_b), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_b), FALSE);
	};
	g_signal_connect(G_OBJECT(switch_b), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) iflag_box);
	g_signal_connect(G_OBJECT(expander_b), "activate", G_CALLBACK(expander_action_cb), 
					(gpointer) iflag_box);
	
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(c_label), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), switch_b, FALSE, FALSE, 0);
	return;
};

void add_parallel_shell_box(struct parallel_sph_shell_control_c *shell_ctl, GtkWidget *vbox){
	int i;
	char *c_label;
	
	GtkWidget *hbox_2[NLBL_SPH_SHELL_CTL], *vbox_2[NLBL_SPH_SHELL_CTL], *Frame_2[NLBL_SPH_SHELL_CTL];
	GtkWidget *hbox_3[NLBL_SPH_SHELL_CTL];
	GtkWidget *expander_SPH_ctl[NLBL_SPH_SHELL_CTL];
	GtkWidget *vbox_sph_shell;
	GtkWidget *scrolled_window;
	
    int index = 0;
    
	for (i=0;i<NLBL_SPH_SHELL_CTL;i++){
		Frame_2[i] = gtk_frame_new("");
		hbox_2[i] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
		vbox_2[i] = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
		hbox_3[i] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
		expander_SPH_ctl[i] = gtk_expander_new("");
	};
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	get_label_sph_shell_ctl(0, c_label);
	add_control_block_box(c_label, &shell_ctl->iflag_FEM_mesh_ctl,
				hbox_3[0], expander_SPH_ctl[0]);
	get_label_sph_shell_ctl(1, c_label);
	add_control_block_box(c_label, &shell_ctl->iflag_sph_domain,
				hbox_3[1], expander_SPH_ctl[1]);
	get_label_sph_shell_ctl(2, c_label);
	add_control_block_box(c_label, &shell_ctl->iflag_sph_shell,
				hbox_3[2], expander_SPH_ctl[2]);
	free(c_label);
	
	vbox_sph_shell = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(scrolled_window, 300, 400);
	gtk_scrolled_window_set_max_content_height(scrolled_window, 400);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 10);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	
	
	for(i=0;i<NLBL_SPH_SHELL_CTL;i++){
		gtk_frame_set_shadow_type(GTK_FRAME(Frame_2[i]), GTK_SHADOW_IN);
		gtk_container_add(GTK_CONTAINER(Frame_2[i]), vbox_2[i]);
		
		gtk_box_pack_start(GTK_BOX(hbox_2[i]), gtk_label_new("  "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_2[i]), Frame_2[i], TRUE, TRUE, 0);
		
		gtk_box_pack_start(GTK_BOX(vbox_sph_shell), hbox_3[i], FALSE, FALSE, 0);
		gtk_container_add(GTK_CONTAINER(expander_SPH_ctl[i]), hbox_2[i]);
		gtk_box_pack_start(GTK_BOX(vbox_sph_shell), expander_SPH_ctl[i], TRUE, TRUE, 0);
	};
	gtk_scrolled_window_add_with_viewport (
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_sph_shell);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, FALSE, FALSE, 0);
	return;
}
