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

GtkWidget * make_FEM_mesh_ctl_hbox(struct FEM_mesh_control_c *Fmesh){
	int i;
	char *c_label;
	
	GtkWidget *hbox_1[NLBL_FEM_MESH_CTL];
	
	GtkWidget *vbox_fmesh = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *Frame = gtk_frame_new("");
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_FEM_mesh_ctl(0, c_label);
	hbox_1[0] = make_chara_ctl_switch_hbox(c_label, Fmesh->memory_conservation_c);
	
	get_label_FEM_mesh_ctl(1, c_label);
	hbox_1[1] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_mesh_output_switch_c);
	
	get_label_FEM_mesh_ctl(2, c_label);
	hbox_1[2] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_surface_output_switch_c);
	
	get_label_FEM_mesh_ctl(3, c_label);
	hbox_1[3] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_viewer_output_switch_c);
	
	get_label_FEM_mesh_ctl(4, c_label);
	hbox_1[4] = make_integer_hbox(c_label, Fmesh->FEM_sleeve_level_c);
	
	get_label_FEM_mesh_ctl(5, c_label);
	hbox_1[5] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_element_overlap_c);
	
	free(c_label);
	
	for(i=0;i<NLBL_FEM_MESH_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_fmesh), hbox_1[i], FALSE, FALSE, 0);
	};
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(scrolled_window, 300, 400);
	gtk_scrolled_window_set_max_content_height(scrolled_window, 400);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 10);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_add_with_viewport (
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_fmesh);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, FALSE, FALSE, 0);
	
	gtk_frame_set_shadow_type(GTK_FRAME(Frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame), vbox);
	
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(""), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), Frame, TRUE, TRUE, 0);
	return hbox;
}

GtkWidget * make_sph_domains_ctl_hbox(struct sphere_domain_ctl_c *sdctl_c){
	int i;
	char *c_label;
	
	GtkWidget *hbox_1[NLBL_SPHERE_DOMAIN_CTL];
	
	GtkWidget *vbox_fmesh = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *Frame = gtk_frame_new("");
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_sphere_domain_ctl(0, c_label);
	hbox_1[0] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_domain_ctl(1, c_label);
	hbox_1[1] = make_integer_hbox(c_label, sdctl_c->num_radial_domain_c);
	
	get_label_sphere_domain_ctl(2, c_label);
	hbox_1[2] = make_integer_hbox(c_label, sdctl_c->num_horiz_domain_c);
	
	get_label_sphere_domain_ctl(3, c_label);
	hbox_1[3] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_domain_ctl(4, c_label);
	hbox_1[4] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_domain_ctl(5, c_label);
	hbox_1[5] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	free(c_label);
	
	for(i=0;i<NLBL_SPHERE_DOMAIN_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_fmesh), hbox_1[i], FALSE, FALSE, 0);
	};
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(scrolled_window, 300, 400);
	gtk_scrolled_window_set_max_content_height(scrolled_window, 400);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 10);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_add_with_viewport (
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_fmesh);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, FALSE, FALSE, 0);
	
	gtk_frame_set_shadow_type(GTK_FRAME(Frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame), vbox);
	
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(""), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), Frame, TRUE, TRUE, 0);
	return hbox;
}

GtkWidget * make_sph_shell_ctl_hbox(struct sphere_data_ctl_c *spctl_c){
	int i;
	char *c_label;
	
	GtkWidget *hbox_1[NLBL_SPHERE_DATA_CTL];
	
	GtkWidget *vbox_fmesh = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *Frame = gtk_frame_new("");
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
    get_label_sphere_data_ctl(0, c_label);
    hbox_1[0] = make_text_hbox(c_label, spctl_c->sph_coef_type_c);
    
    get_label_sphere_data_ctl(1, c_label);
    hbox_1[1] = make_text_hbox(c_label, spctl_c->sph_grid_type_c);
    
	get_label_sphere_data_ctl(2, c_label);
	hbox_1[2] = make_integer_hbox(c_label, spctl_c->ltr_c);
	
	get_label_sphere_data_ctl(3, c_label);
	hbox_1[3] = make_integer_hbox(c_label, spctl_c->phi_symmetry_c);
	
	get_label_sphere_data_ctl(4, c_label);
	hbox_1[4] = make_integer_hbox(c_label, spctl_c->ngrid_elevation_c);
	
	get_label_sphere_data_ctl(5, c_label);
	hbox_1[5] = make_integer_hbox(c_label, spctl_c->ngrid_azimuth_c);
	
	get_label_sphere_data_ctl(6, c_label);
	hbox_1[6] = make_text_hbox(c_label, spctl_c->radial_grid_type_c);
	
	get_label_sphere_data_ctl(7, c_label);
	hbox_1[7] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(8, c_label);
	hbox_1[8] = make_integer_hbox(c_label, spctl_c->num_fluid_grid_c);
	
	get_label_sphere_data_ctl(9, c_label);
	hbox_1[9] = make_real_hbox(c_label, spctl_c->fluid_core_size_c);
	
	get_label_sphere_data_ctl(10, c_label);
	hbox_1[10] = make_real_hbox(c_label, spctl_c->ICB_to_CMB_ratio_c);
	
	get_label_sphere_data_ctl(11, c_label);
	hbox_1[11] = make_real_hbox(c_label, spctl_c->Min_radius_c);
	
	get_label_sphere_data_ctl(12, c_label);
	hbox_1[12] = make_real_hbox(c_label, spctl_c->ICB_radius_c);
	
	get_label_sphere_data_ctl(13, c_label);
	hbox_1[13] = make_real_hbox(c_label, spctl_c->CMB_radius_c);
	
	get_label_sphere_data_ctl(14, c_label);
	hbox_1[14] = make_real_hbox(c_label, spctl_c->Max_radius_c);
	
	get_label_sphere_data_ctl(15, c_label);
	hbox_1[15] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(16, c_label);
	hbox_1[16] = make_integer_hbox(c_label, spctl_c->num_radial_layer_c);
	
	get_label_sphere_data_ctl(17, c_label);
	hbox_1[17] = make_integer_hbox(c_label, spctl_c->num_med_layer_c);
	
	get_label_sphere_data_ctl(18, c_label);
	hbox_1[18] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(19, c_label);
	hbox_1[19] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	free(c_label);
	
	for(i=0;i<NLBL_SPHERE_DATA_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_fmesh), hbox_1[i], FALSE, FALSE, 0);
	};
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(scrolled_window, 300, 400);
	gtk_scrolled_window_set_max_content_height(scrolled_window, 400);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 10);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_add_with_viewport (
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_fmesh);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, FALSE, FALSE, 0);
	
	gtk_frame_set_shadow_type(GTK_FRAME(Frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame), vbox);
	
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(""), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), Frame, TRUE, TRUE, 0);
	return hbox;
}

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
	hbox_2[0] = make_FEM_mesh_ctl_hbox(shell_ctl->Fmesh_ctl);
	
	get_label_sph_shell_ctl(1, c_label);
	add_control_block_box(c_label, &shell_ctl->iflag_sph_domain,
				hbox_3[1], expander_SPH_ctl[1]);
	hbox_2[1] = make_sph_domains_ctl_hbox(shell_ctl->sdctl_c);
	
	get_label_sph_shell_ctl(2, c_label);
	add_control_block_box(c_label, &shell_ctl->iflag_sph_shell,
				hbox_3[2], expander_SPH_ctl[2]);
	hbox_2[2] = make_sph_shell_ctl_hbox(shell_ctl->spctl_c);
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
