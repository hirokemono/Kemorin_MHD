//
//  control_panels_MHD_model_GTK.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/1/23.
//

#include "control_panels_MHD_model_GTK.h"

extern void * c_link_base_field_names(void *fld_names_c);
extern void * c_link_time_evo_list_to_ctl(void *fld_names_c);
extern void * c_link_force_list_to_ctl(void *fld_names_c);
extern void * c_link_sph_force_list_to_ctl(void *fld_names_c);
extern void * c_link_reftemp_list_to_ctl(void *fld_names_c);
extern void * c_link_sph_reftemp_list_to_ctl(void *fld_names_c);


GtkWidget * MHD_temperature_model_expander(GtkWidget *window, struct f_MHD_temp_model_control *f_reft_ctl, 
                                           struct chara_clist *label_reftemp_list){
    GtkWidget *vbox_tl = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *hbox_tl1 = draw_real_item_entry_hbox(f_reft_ctl->f_low_ctl->f_value);
    GtkWidget *hbox_tl2 = draw_real_item_entry_hbox(f_reft_ctl->f_low_ctl->f_depth);
	gtk_box_pack_start(GTK_BOX(vbox_tl), hbox_tl1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tl), hbox_tl2, FALSE, FALSE, 0);
	GtkWidget *expand_tl = draw_control_block(f_reft_ctl->f_low_ctl->c_block_name, 
                                              f_reft_ctl->f_low_ctl->f_iflag,
                                              window, vbox_tl);
    
    GtkWidget *vbox_th = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *hbox_th1 = draw_real_item_entry_hbox(f_reft_ctl->f_high_ctl->f_value);
    GtkWidget *hbox_th2 = draw_real_item_entry_hbox(f_reft_ctl->f_high_ctl->f_depth);
	gtk_box_pack_start(GTK_BOX(vbox_th), hbox_th1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_th), hbox_th2, FALSE, FALSE, 0);
	GtkWidget *expand_th = draw_control_block(f_reft_ctl->f_high_ctl->c_block_name, 
                                              f_reft_ctl->f_high_ctl->f_iflag,
                                              window, vbox_th);
    
    GtkWidget *vbox_tp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *hbox_tp1 = draw_real_item_entry_hbox(f_reft_ctl->f_takepiro_ctl->f_stratified_sigma_ctl);
    GtkWidget *hbox_tp2 = draw_real_item_entry_hbox(f_reft_ctl->f_takepiro_ctl->f_stratified_width_ctl);
    GtkWidget *hbox_tp3 = draw_real_item_entry_hbox(f_reft_ctl->f_takepiro_ctl->f_stratified_outer_r_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_tp), hbox_tp1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tp), hbox_tp2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tp), hbox_tp3, FALSE, FALSE, 0);
	GtkWidget *expand_tp = draw_control_block(f_reft_ctl->f_takepiro_ctl->c_block_name, 
                                              f_reft_ctl->f_takepiro_ctl->f_iflag,
                                              window, vbox_tp);
    
    
    
    GtkWidget *hbox_t1 = draw_chara_switch_entry_hbox(f_reft_ctl->f_filterd_advect_ctl);
    GtkWidget *hbox_t2 = draw_chara_item_combobox_hbox(label_reftemp_list, f_reft_ctl->f_reference_ctl, window);
    GtkWidget *hbox_t3 = draw_chara_switch_entry_hbox(f_reft_ctl->f_stratified_ctl);
    GtkWidget *hbox_t4 = draw_chara_item_entry_hbox(f_reft_ctl->f_ref_file_ctl);
    GtkWidget *hbox_t5 = draw_real_item_entry_hbox(f_reft_ctl->f_ICB_diffuse_reduction_ctl);
    
	GtkWidget *vbox_tt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), expand_tl, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), expand_th, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), expand_tp, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t1, FALSE, FALSE, 0);
	GtkWidget *expand_t = draw_control_block(f_reft_ctl->c_block_name, f_reft_ctl->f_iflag,
                                             window, vbox_tt);
    return expand_t;
};

GtkWidget *MHD_model_ctl_expander(void *f_parent, struct f_MHD_model_control *f_model_ctl,
                                  struct MHD_model_widgets *model_wgts, GtkWidget *window){
	model_wgts = (struct MHD_model_widgets *) malloc(sizeof(struct MHD_model_widgets));
	if(model_wgts == NULL){
		printf("malloc error for model_wgts\n");
		exit(0);
	};
    
    struct field_views *fields_vws = init_field_views_GTK(f_model_ctl->f_fld_ctl);
	fields_vws->used_tree_view = create_field_tree_view(fields_vws->all_fld_list,
														fields_vws->fld_ctl_gtk);
    fields_vws->unused_field_tree_view = create_unused_field_tree_views(fields_vws->all_fld_list);
    fields_vws->field_group_tree_view = create_field_group_tree_view(fields_vws->all_fld_list);
    fields_vws->all_field_tree_view = create_all_field_tree_views(fields_vws->all_fld_list);
    create_direction_tree_views(fields_vws);
	GtkWidget *vbox_MHD_fields = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_field_selection_box(fields_vws, window, vbox_MHD_fields);
    GtkWidget *expand_MHD_fields = draw_control_block(f_model_ctl->f_fld_ctl->c_block_name,
                                                      f_model_ctl->f_fld_ctl->f_iflag,
                                                      window, vbox_MHD_fields);
    
    
    
    model_wgts->label_time_evo_list = init_f_ctl_chara_array(c_link_time_evo_list_to_ctl, f_parent);
    GtkWidget *vbox_m1 = c_list_combobox_expander(f_model_ctl->f_evo_ctl->f_t_evo_field_ctl,
                                                  model_wgts->label_time_evo_list,
                                                  model_wgts->time_evo_vws, window);
    GtkWidget *expand_MHD_time_evo = draw_control_block(f_model_ctl->f_evo_ctl->c_block_name,
														f_model_ctl->f_evo_ctl->f_iflag,
                                                        window, vbox_m1);
	
    
    GtkWidget *expand_MHD_fluid_area = add_c_list_box_w_addbottun(f_model_ctl->f_earea_ctl->f_evo_fluid_group_ctl, 
                                                                 model_wgts->f_fluid_area_tree_view);
    GtkWidget *expand_MHD_conduct_area = add_c_list_box_w_addbottun(f_model_ctl->f_earea_ctl->f_evo_conduct_group_ctl, 
                                                                    model_wgts->f_conduct_area_tree_view);
    
    GtkWidget *vbox_m2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_m2), expand_MHD_fluid_area, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m2), expand_MHD_conduct_area, FALSE, FALSE, 0);
    
    GtkWidget *expand_MHD_tevo_area = draw_control_block(f_model_ctl->f_earea_ctl->c_block_name,
														f_model_ctl->f_earea_ctl->f_iflag,
                                                        window, vbox_m2);
    
	
	GtkWidget *vbox_m3 = draw_node_bc_ctl_vbox(f_model_ctl->f_nbc_ctl,
											   model_wgts->bc_nod_bc_vws, window);
    GtkWidget *expand_MHD_node_bc = draw_control_block(f_model_ctl->f_nbc_ctl->c_block_name,
                                                      f_model_ctl->f_nbc_ctl->f_iflag,
													   window, vbox_m3);
	
	GtkWidget *vbox_m4 = draw_surf_bc_ctl_vbox(f_model_ctl->f_sbc_ctl,
											   model_wgts->bc_surf_bc_vws, window);
    GtkWidget *expand_MHD_surf_bc = draw_control_block(f_model_ctl->f_sbc_ctl->c_block_name,
                                                      f_model_ctl->f_sbc_ctl->f_iflag,
                                                      window, vbox_m4);
	
	
    model_wgts->label_force_list = init_f_ctl_chara_array(c_link_force_list_to_ctl, f_parent);
    GtkWidget *vbox_fce = c_list_combobox_expander(f_model_ctl->f_frc_ctl->f_force_names,
                                                  model_wgts->label_force_list,
                                                  model_wgts->force_vws,
                                                  window);
    GtkWidget *vbox_MHD_force = draw_control_block(f_model_ctl->f_frc_ctl->c_block_name,
                                                   f_model_ctl->f_frc_ctl->f_iflag,
                                                   window, vbox_fce);
    
	GtkWidget *expand_MHD_dimless = add_dimless_selection_box(f_model_ctl->f_dless_ctl,
                                                              model_wgts->f_dimless_vws, window);
	
	
	GtkWidget *vbox_eqs = draw_MHD_equations_vbox(f_model_ctl->f_eqs_ctl, 
												  model_wgts->f_eqs_vws, window);
	GtkWidget *expand_MHD_eqs = draw_control_block(f_model_ctl->f_eqs_ctl->c_block_name, 
													 f_model_ctl->f_eqs_ctl->f_iflag,
													 window, vbox_eqs);
    
    model_wgts->label_reftemp_list = init_f_ctl_chara_array(c_link_reftemp_list_to_ctl, f_parent);
	GtkWidget *expand_t = MHD_temperature_model_expander(window, f_model_ctl->f_reft_ctl, 
                                                         model_wgts->label_reftemp_list);
	GtkWidget *expand_c = MHD_temperature_model_expander(window, f_model_ctl->f_refc_ctl, 
                                                         model_wgts->label_reftemp_list);
    
    
    model_wgts->label_xyz_dir_list = init_f_ctl_chara_array(c_link_xyz_dir_list_to_ctl, f_parent);
    GtkWidget *hbox_mc1 = draw_chara_switch_entry_hbox(f_model_ctl->f_mcv_ctl->f_filterd_induction_ctl);
    GtkWidget *hbox_mc2 = draw_chara_switch_entry_hbox(f_model_ctl->f_mcv_ctl->f_magneto_cv);
    GtkWidget *expand_mc = cr_list_combobox_expander(f_model_ctl->f_mcv_ctl->f_ext_magne, 
                                                     model_wgts->label_xyz_dir_list,
                                                     model_wgts->ex_magne_vws, window);
    
    GtkWidget *vbox_mcv = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_mcv), hbox_mc1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_mcv), hbox_mc2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_mcv), expand_mc, FALSE, FALSE, 0);
	GtkWidget *expand_MHD_mcv = draw_control_block(f_model_ctl->f_mcv_ctl->c_block_name, 
													 f_model_ctl->f_mcv_ctl->f_iflag,
													 window, vbox_mcv);
    
    
    GtkWidget *expand_msc = add_cr_list_box_w_addbottun(f_model_ctl->f_bscale_ctl->f_mag_to_kin_energy_ctl, 
                                                        model_wgts->f_magnetic_scale_tree);
	GtkWidget *expand_MHD_msc = draw_control_block(f_model_ctl->f_bscale_ctl->c_block_name, 
													 f_model_ctl->f_bscale_ctl->f_iflag,
													 window, expand_msc);
    
    
    GtkWidget *hbox_g1 = draw_chara_switch_entry_hbox(f_model_ctl->f_g_ctl->f_gravity);
    GtkWidget *hbox_g2 = draw_chara_switch_entry_hbox(f_model_ctl->f_g_ctl->f_FEM_gravity_model);
    GtkWidget *expand_gc = cr_list_combobox_expander(f_model_ctl->f_g_ctl->f_gravity_vector, 
                                                     model_wgts->label_xyz_dir_list,
                                                     model_wgts->gravity_vws, window);
    
    GtkWidget *vbox_g = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_g), hbox_g1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_g), hbox_g2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_g), expand_gc, FALSE, FALSE, 0);
	GtkWidget *expand_MHD_g = draw_control_block(f_model_ctl->f_g_ctl->c_block_name, 
                                                 f_model_ctl->f_g_ctl->f_iflag,
                                                 window, vbox_g);
    
    
    
    GtkWidget *expand_cc = cr_list_combobox_expander(f_model_ctl->f_cor_ctl->f_system_rotation, 
                                                     model_wgts->label_xyz_dir_list,
                                                     model_wgts->gravity_vws, window);
    GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_model_ctl->f_cor_ctl->f_FEM_coriolis_model);
    GtkWidget *hbox_c2 = draw_chara_switch_entry_hbox(f_model_ctl->f_cor_ctl->f_FEM_coriolis_implicit);
    
    GtkWidget *vbox_c = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_c), expand_cc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_c), hbox_c1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_c), hbox_c2, FALSE, FALSE, 0);
	GtkWidget *expand_MHD_c = draw_control_block(f_model_ctl->f_cor_ctl->c_block_name, 
                                                 f_model_ctl->f_cor_ctl->f_iflag,
                                                 window, vbox_c);
    
    
    GtkWidget *expand_MHD_s = SGS_MHD_model_expander(window, f_model_ctl->f_sgs_ctl, 
                                                     model_wgts->SGSWgts);
    
    GtkWidget *vbox_m = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_fields, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_time_evo, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_tevo_area, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_node_bc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_surf_bc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), vbox_MHD_force, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_dimless, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_eqs, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_t, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_c, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_mcv, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_msc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_g, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_c, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_s, FALSE, FALSE, 0);
	GtkWidget *expand_MHD_model = draw_control_block(f_model_ctl->c_block_name, 
													 f_model_ctl->f_iflag,
                                                     window, vbox_m);
    return expand_MHD_model;
}
