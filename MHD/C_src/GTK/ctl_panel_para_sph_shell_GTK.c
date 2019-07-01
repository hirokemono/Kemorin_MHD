/*
//  ctl_panel_para_sph_shell_GTK.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#include "ctl_panel_para_sph_shell_GTK.h"


GtkWidget * make_FEM_mesh_ctl_hbox(const char *label_hd, struct FEM_mesh_control_c *Fmesh){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_FEM_MESH_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_FEM_mesh_ctl(0, c_label);
	hbox_3[0] = make_chara_ctl_switch_hbox(c_label, Fmesh->memory_conservation_c);
	
	get_label_FEM_mesh_ctl(1, c_label);
	hbox_3[1] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_mesh_output_switch_c);
	
	get_label_FEM_mesh_ctl(2, c_label);
	hbox_3[2] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_surface_output_switch_c);
	
	get_label_FEM_mesh_ctl(3, c_label);
	hbox_3[3] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_viewer_output_switch_c);
	
	get_label_FEM_mesh_ctl(4, c_label);
	hbox_3[4] = make_integer_hbox(c_label, Fmesh->FEM_sleeve_level_c);
	
	get_label_FEM_mesh_ctl(5, c_label);
	hbox_3[5] = make_chara_ctl_switch_hbox(c_label, Fmesh->FEM_element_overlap_c);
	
	free(c_label);
	
	for(i=0;i<NLBL_FEM_MESH_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
	hbox = make_expand_ctl_hbox(label_hd, &Fmesh->iflag_use, 400, vbox_1);
	return hbox;
}

GtkWidget * make_sph_domains_ctl_hbox(const char *label_hd, struct sphere_domain_ctl_c *sdctl_c){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_SPHERE_DOMAIN_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_sphere_domain_ctl(0, c_label);
	hbox_3[0] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_domain_ctl(1, c_label);
	hbox_3[1] = make_integer_hbox(c_label, sdctl_c->num_radial_domain_c);
	
	get_label_sphere_domain_ctl(2, c_label);
	hbox_3[2] = make_integer_hbox(c_label, sdctl_c->num_horiz_domain_c);
	
	get_label_sphere_domain_ctl(3, c_label);
	hbox_3[3] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_domain_ctl(4, c_label);
	hbox_3[4] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_domain_ctl(5, c_label);
	hbox_3[5] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	free(c_label);
	
	for(i=0;i<NLBL_SPHERE_DOMAIN_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
		
	hbox = make_expand_ctl_hbox(label_hd, &sdctl_c->iflag_use, 400, vbox_1);
	return hbox;
}

GtkWidget * make_sph_shell_ctl_hbox(const char *label_hd, struct sphere_data_ctl_c *spctl_c){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_SPHERE_DATA_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
    get_label_sphere_data_ctl(0, c_label);
    hbox_3[0] = make_text_hbox(c_label, spctl_c->sph_coef_type_c);
    
    get_label_sphere_data_ctl(1, c_label);
    hbox_3[1] = make_text_hbox(c_label, spctl_c->sph_grid_type_c);
    
	get_label_sphere_data_ctl(2, c_label);
	hbox_3[2] = make_integer_hbox(c_label, spctl_c->ltr_c);
	
	get_label_sphere_data_ctl(3, c_label);
	hbox_3[3] = make_integer_hbox(c_label, spctl_c->phi_symmetry_c);
	
	get_label_sphere_data_ctl(4, c_label);
	hbox_3[4] = make_integer_hbox(c_label, spctl_c->ngrid_elevation_c);
	
	get_label_sphere_data_ctl(5, c_label);
	hbox_3[5] = make_integer_hbox(c_label, spctl_c->ngrid_azimuth_c);
	
	get_label_sphere_data_ctl(6, c_label);
	hbox_3[6] = make_text_hbox(c_label, spctl_c->radial_grid_type_c);
	
	get_label_sphere_data_ctl(7, c_label);
	hbox_3[7] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(8, c_label);
	hbox_3[8] = make_integer_hbox(c_label, spctl_c->num_fluid_grid_c);
	
	get_label_sphere_data_ctl(9, c_label);
	hbox_3[9] = make_real_hbox(c_label, spctl_c->fluid_core_size_c);
	
	get_label_sphere_data_ctl(10, c_label);
	hbox_3[10] = make_real_hbox(c_label, spctl_c->ICB_to_CMB_ratio_c);
	
	get_label_sphere_data_ctl(11, c_label);
	hbox_3[11] = make_real_hbox(c_label, spctl_c->Min_radius_c);
	
	get_label_sphere_data_ctl(12, c_label);
	hbox_3[12] = make_real_hbox(c_label, spctl_c->ICB_radius_c);
	
	get_label_sphere_data_ctl(13, c_label);
	hbox_3[13] = make_real_hbox(c_label, spctl_c->CMB_radius_c);
	
	get_label_sphere_data_ctl(14, c_label);
	hbox_3[14] = make_real_hbox(c_label, spctl_c->Max_radius_c);
	
	get_label_sphere_data_ctl(15, c_label);
	hbox_3[15] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(16, c_label);
	hbox_3[16] = make_integer_hbox(c_label, spctl_c->num_radial_layer_c);
	
	get_label_sphere_data_ctl(17, c_label);
	hbox_3[17] = make_integer_hbox(c_label, spctl_c->num_med_layer_c);
	
	get_label_sphere_data_ctl(18, c_label);
	hbox_3[18] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(19, c_label);
	hbox_3[19] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	free(c_label);
	
	for(i=0;i<NLBL_SPHERE_DATA_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
	hbox = make_expand_ctl_hbox(label_hd, &spctl_c->iflag_use, 400, vbox_1);
	return hbox;
}

GtkWidget * make_parallel_shell_hbox(const char *label_hd, struct parallel_sph_shell_control_c *shell_ctl){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_SPH_SHELL_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	get_label_sph_shell_ctl(0, c_label);
	hbox_3[0] = make_FEM_mesh_ctl_hbox(c_label, shell_ctl->Fmesh_ctl);
	
	get_label_sph_shell_ctl(1, c_label);
	hbox_3[1] = make_sph_domains_ctl_hbox(c_label, shell_ctl->sdctl_c);
	
	get_label_sph_shell_ctl(2, c_label);
	hbox_3[2] = make_sph_shell_ctl_hbox(c_label, shell_ctl->spctl_c);
	free(c_label);
	
	for(i=0;i<NLBL_SPH_SHELL_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], TRUE, TRUE, 0);
	};
	
	hbox = make_expand_ctl_hbox(label_hd, &shell_ctl->iflag_use_file, 400, vbox_1);
	return hbox;
}
