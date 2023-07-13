/*
//  control_panel_4_MHD_BCs_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#include "control_panel_4_MHD_BCs_GTK.h"

GtkWidget * draw_node_bc_ctl_vbox(struct f_MHD_node_bc_control *f_nbc_ctl,
                                  struct f_MHD_BCs_tree_views *bc_nod_bc_vws, 
                                  GtkWidget *window){
	GtkWidget *vbox_m3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	bc_nod_bc_vws = (struct f_MHD_BCs_tree_views *) malloc(sizeof(struct f_MHD_BCs_tree_views));
	if(bc_nod_bc_vws == NULL){
		printf("malloc error for bc_nod_bc_vws\n");
		exit(0);
	};
	
	/*
    struct chara_clist *f_mom_bc_type = init_f_ctl_chara_array(set_label_sph_momentum_bc_f, 
																f_nbc_ctl->f_self);
    struct chara_clist *f_idct_bc_type = init_f_ctl_chara_array(set_label_sph_induction_bc_f, 
																f_nbc_ctl->f_self);
    struct chara_clist *f_heat_bc_type = init_f_ctl_chara_array(set_label_sph_thermal_bc_f, 
																f_nbc_ctl->f_self);
	*/
	
    struct chara_clist *f_mom_bc_type =  init_f_ctl_chara_array(set_label_momentum_bc_f, 
																f_nbc_ctl->f_self);
    struct chara_clist *f_idct_bc_type = init_f_ctl_chara_array(set_label_induction_bc_f, 
																f_nbc_ctl->f_self);
    struct chara_clist *f_heat_bc_type = init_f_ctl_chara_array(set_label_thermal_bc_f, 
																f_nbc_ctl->f_self);
	
    struct chara_clist *f_potential_bc_type = init_f_ctl_chara_array(set_label_potential_bc_f, 
																	 f_nbc_ctl->f_self);
    struct chara_clist *f_vect_p_bc_type = init_f_ctl_chara_array(set_label_vector_p_bc_f, 
																  f_nbc_ctl->f_self);
    struct chara_clist *f_current_bc_type = init_f_ctl_chara_array(set_label_current_bc_f, 
																   f_nbc_ctl->f_self);
	
	GtkWidget *expand_MHD_node_bcU = boundary_condition_expander(f_nbc_ctl->f_node_bc_U_ctl,
                                                                 f_mom_bc_type,
																 bc_nod_bc_vws->bc_mom_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcB = boundary_condition_expander(f_nbc_ctl->f_node_bc_B_ctl,
                                                                 f_idct_bc_type,
																 bc_nod_bc_vws->bc_induction_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcT = boundary_condition_expander(f_nbc_ctl->f_node_bc_T_ctl,
                                                                 f_heat_bc_type,
																 bc_nod_bc_vws->bc_temp_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcC = boundary_condition_expander(f_nbc_ctl->f_node_bc_C_ctl,
                                                                 f_heat_bc_type,
																 bc_nod_bc_vws->bc_comp_vws,
                                                                 window);
	
	GtkWidget *expand_MHD_node_bcP = boundary_condition_expander(f_nbc_ctl->f_node_bc_P_ctl,
                                                                 f_potential_bc_type,
																 bc_nod_bc_vws->bc_press_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcM = boundary_condition_expander(f_nbc_ctl->f_node_bc_MP_ctl,
                                                                 f_potential_bc_type,
																 bc_nod_bc_vws->bc_magp_vws, window);
	GtkWidget *expand_MHD_node_bcA = boundary_condition_expander(f_nbc_ctl->f_node_bc_A_ctl,
                                                                 
                                                                 f_vect_p_bc_type,
																 bc_nod_bc_vws->f_vecp_bc_type,
                                                                 window);
	GtkWidget *expand_MHD_node_bcJ = boundary_condition_expander(f_nbc_ctl->f_node_bc_J_ctl,
                                                                 f_current_bc_type,
																 bc_nod_bc_vws->f_current_bc_type,
                                                                 window);
	
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcU, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcB, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcT, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcC, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcP, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcM, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcA, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcJ, FALSE, FALSE, 0);
	return vbox_m3;
}

GtkWidget * draw_surf_bc_ctl_vbox(struct f_MHD_surf_bc_control *f_sbc_ctl,
                                  struct f_MHD_BCs_tree_views *bc_surf_bc_vws, 
                                  GtkWidget *window){
	GtkWidget *vbox_m3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	bc_surf_bc_vws = (struct f_MHD_BCs_tree_views *) malloc(sizeof(struct f_MHD_BCs_tree_views));
	if(bc_surf_bc_vws == NULL){
		printf("malloc error for bc_surf_bc_vws\n");
		exit(0);
	};
	
    struct chara_clist *f_mom_bc_type = init_f_ctl_chara_array(surf_bc_label_momentum_bc_f,
                                                               f_sbc_ctl->f_self);
    struct chara_clist *f_idct_bc_type = init_f_ctl_chara_array(surf_bc_label_induction_bc_f, 
																f_sbc_ctl->f_self);
    struct chara_clist *f_heat_bc_type = init_f_ctl_chara_array(surf_bc_label_thermal_bc_f, 
																f_sbc_ctl->f_self);
    struct chara_clist *f_infty_bc_type = init_f_ctl_chara_array(surf_bc_label_infinity_bc_f, 
																 f_sbc_ctl->f_self);
	
    struct chara_clist *f_potential_bc_type = init_f_ctl_chara_array(surf_bc_label_potential_bc_f, 
																	 f_sbc_ctl->f_self);
    struct chara_clist *f_vect_p_bc_type = init_f_ctl_chara_array(surf_bc_label_vector_p_bc_f, 
																  f_sbc_ctl->f_self);
    struct chara_clist *f_current_bc_type = init_f_ctl_chara_array(surf_bc_label_current_bc_f, 
                                                                   f_sbc_ctl->f_self);
	
	GtkWidget *expand_MHD_node_bcU = boundary_condition_expander(f_sbc_ctl->f_surf_bc_ST_ctl,
                                                                 f_mom_bc_type,
                                                                 bc_surf_bc_vws->bc_mom_vws, window);
	GtkWidget *expand_MHD_node_bcB = boundary_condition_expander(f_sbc_ctl->f_surf_bc_BN_ctl,
                                                                 f_idct_bc_type,
																 bc_surf_bc_vws->bc_induction_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcT = boundary_condition_expander(f_sbc_ctl->f_surf_bc_HF_ctl,
                                                                 f_heat_bc_type,
																 bc_surf_bc_vws->bc_temp_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcC = boundary_condition_expander(f_sbc_ctl->f_surf_bc_CF_ctl,
                                                                 f_heat_bc_type,
																 bc_surf_bc_vws->bc_comp_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcF = boundary_condition_expander(f_sbc_ctl->f_surf_bc_INF_ctl,
                                                                 f_infty_bc_type,
																 bc_surf_bc_vws->bc_infinity_vws,
                                                                 window);
	
	GtkWidget *expand_MHD_node_bcP = boundary_condition_expander(f_sbc_ctl->f_surf_bc_PN_ctl,
                                                                 f_potential_bc_type,
																 bc_surf_bc_vws->bc_press_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcM = boundary_condition_expander(f_sbc_ctl->f_surf_bc_MPN_ctl,
                                                                 f_potential_bc_type,
																 bc_surf_bc_vws->bc_magp_vws,
                                                                 window);
	GtkWidget *expand_MHD_node_bcA = boundary_condition_expander(f_sbc_ctl->f_surf_bc_AN_ctl,
                                                                 f_vect_p_bc_type,
																 bc_surf_bc_vws->f_vecp_bc_type,
                                                                 window);
	GtkWidget *expand_MHD_node_bcJ = boundary_condition_expander(f_sbc_ctl->f_surf_bc_JN_ctl,
                                                                 f_current_bc_type,
																 bc_surf_bc_vws->f_current_bc_type,
                                                                 window);

	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcU, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcB, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcT, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcC, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcF, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcP, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcM, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcA, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m3), expand_MHD_node_bcJ, FALSE, FALSE, 0);
	return vbox_m3;
}
