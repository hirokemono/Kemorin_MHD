/*
//  control_panel_4_SGS_model_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_SGS_model_GTK.h"

static GtkWidget * draw_MHD_SGS_SPH_filter_expand(char *label_name, 
                                                  struct f_MHD_SGS_SPH_filter_control *f_sph_filter_item, 
                                                  GtkWidget *window){
	GtkWidget *vbox_f = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_sph_filter_item->f_sph_filter_type_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_sph_filter_item->f_radial_filter_type_ctl);
    GtkWidget *hbox_3 = draw_int_item_entry_hbox(f_sph_filter_item->f_maximum_moments_ctl);
    GtkWidget *hbox_4 = draw_real_item_entry_hbox(f_sph_filter_item->f_sphere_filter_width_ctl);
    GtkWidget *hbox_5 = draw_real_item_entry_hbox(f_sph_filter_item->f_radial_filter_width_ctl);
    GtkWidget *hbox_6 = draw_int_item_entry_hbox(f_sph_filter_item->f_first_reference_ctl);
    GtkWidget *hbox_7 = draw_int_item_entry_hbox(f_sph_filter_item->f_second_reference_ctl);
    
    gtk_box_pack_start(GTK_BOX(vbox_f), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_f), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_f), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_f), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_f), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_f), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_f), hbox_7,  FALSE, FALSE, 0);
    
    GtkWidget *expand_filter = draw_control_block(duplicate_underscore(label_name),
                                                  f_sph_filter_item->f_iflag,
                                                  window, vbox_f);
    return expand_filter;
};

static GtkWidget * FEM_MHD_SGS_filter_expander(GtkWidget *window, 
                                               struct f_MHD_SGS_filter_file_control *f_ffile_ctl){
    GtkWidget *hbox_sf1 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_head_ctl);
    GtkWidget *hbox_sf2 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_coef_head_ctl);
    GtkWidget *hbox_sf3 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_elen_head_ctl);
    GtkWidget *hbox_sf4 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_moms_head_ctl);
    GtkWidget *hbox_sf5 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_wide_head_ctl);
    GtkWidget *hbox_sf6 = draw_chara_item_entry_hbox(f_ffile_ctl->f_model_coef_ini_head_ctl);
    GtkWidget *hbox_sf7 = draw_chara_item_entry_hbox(f_ffile_ctl->f_commute_coef_ini_head_ctl);
    GtkWidget *hbox_sf8 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_elen_format);
    GtkWidget *hbox_sf9 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_3d_format);
    GtkWidget *hbox_sf10 = draw_chara_item_entry_hbox(f_ffile_ctl->f_filter_wide_format);
    GtkWidget *hbox_sf11 = draw_chara_item_entry_hbox(f_ffile_ctl->f_model_coef_rst_format);
    GtkWidget *hbox_sf12 = draw_chara_item_entry_hbox(f_ffile_ctl->f_commute_coef_rst_format);
    
    GtkWidget *vbox_sf1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf6, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf7, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf8, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf9, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf10, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sf1), hbox_sf12, FALSE, FALSE, 0);
    
    GtkWidget *expand_SGS_1 = draw_control_block(f_ffile_ctl->c_block_name, f_ffile_ctl->f_iflag,
                                                  window, vbox_sf1);
    return expand_SGS_1;
};

static GtkWidget * FEM_MHD_SGS_area_expander(GtkWidget *window, 
                                             struct f_MHD_SGS_layer_control *f_elayer_ctl,
                                             struct f_SGS_model_widgets *SGSWgts){
    GtkWidget *hbox_sl1 = draw_chara_item_entry_hbox(f_elayer_ctl->f_layering_grp_type_ctl);
    GtkWidget *hbox_sl2 = draw_chara_item_entry_hbox(f_elayer_ctl->f_start_layering_grp_name_ctl);
    GtkWidget *hbox_sl3 = draw_chara_item_entry_hbox(f_elayer_ctl->f_start_fl_layer_grp_name_ctl);
    GtkWidget *hbox_sl4 = draw_int_item_entry_hbox(f_elayer_ctl->f_ngrp_SGS_on_sphere_ctl);
    GtkWidget *hbox_sl5 = draw_int_item_entry_hbox(f_elayer_ctl->f_num_layering_grp_ctl);
    GtkWidget *hbox_sl6 = draw_int_item_entry_hbox(f_elayer_ctl->f_num_fl_layer_grp_ctl);
    
	GtkWidget *hbox_sl21 = add_int_list_box_w_addbottun(f_elayer_ctl->f_igrp_stack_layer_ctl,
                                                       SGSWgts->f_igrp_stack_layer_tree);
    GtkWidget *hbox_sl22 = add_c_list_box_w_addbottun(f_elayer_ctl->f_layer_grp_name_ctl, 
                                                      SGSWgts->f_layer_grp_name_tree);
    
    
    GtkWidget *vbox_sl1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl6, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl21, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sl1), hbox_sl22, FALSE, FALSE, 0);
    
	GtkWidget *expand_SGS_2 = draw_control_block(f_elayer_ctl->c_block_name, f_elayer_ctl->f_iflag,
                                                  window, vbox_sl1);
    return expand_SGS_2;
}

static GtkWidget * FEM_MHD_SGS_3Dfilter_expander(GtkWidget *window, 
                                                 struct f_MHD_SGS_3d_filter_control *f_s3df_ctl,
                                                 struct f_SGS_model_widgets *SGSWgts){
    GtkWidget *hbox_sd11 = add_c_list_box_w_addbottun(f_s3df_ctl->f_whole_filter_grp_ctl, 
                                                      SGSWgts->f_whole_filter_grp_tree);
    GtkWidget *hbox_sd12 = add_c_list_box_w_addbottun(f_s3df_ctl->f_fluid_filter_grp_ctl, 
                                                      SGSWgts->f_fluid_filter_grp_tree);
    
    GtkWidget *hbox_sd1 = draw_chara_item_entry_hbox(f_s3df_ctl->f_momentum_filter_ctl);
    GtkWidget *hbox_sd2 = draw_chara_item_entry_hbox(f_s3df_ctl->f_heat_filter_ctl);
    GtkWidget *hbox_sd3 = draw_chara_item_entry_hbox(f_s3df_ctl->f_induction_filter_ctl);
    GtkWidget *hbox_sd4 = draw_chara_item_entry_hbox(f_s3df_ctl->f_compostion_filter_ctl);
    
    
    GtkWidget *vbox_sd3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sd3), hbox_sd11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sd3), hbox_sd12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sd3), hbox_sd1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sd3), hbox_sd2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sd3), hbox_sd3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sd3), hbox_sd4, FALSE, FALSE, 0);
	GtkWidget *expand_SGS_3 = draw_control_block(f_s3df_ctl->c_block_name, 
                                                 f_s3df_ctl->f_iflag,
                                                  window, vbox_sd3);
    return expand_SGS_3;
};

GtkWidget *SGS_MHD_model_expander(GtkWidget *window, struct f_MHD_SGS_model_control *f_sgs_ctl, 
                                  struct f_SGS_model_widgets *SGSWgts){
	SGSWgts = (struct f_SGS_model_widgets *) malloc(sizeof(struct f_SGS_model_widgets));
	if(SGSWgts == NULL){
		printf("malloc error for SGSWgts\n");
		exit(0);
	};
    
    
    GtkWidget *expand_SGS_1 = FEM_MHD_SGS_filter_expander(window, f_sgs_ctl->f_ffile_ctl);
    GtkWidget *expand_SGS_2 = FEM_MHD_SGS_area_expander(window, f_sgs_ctl->f_elayer_ctl, SGSWgts);
    GtkWidget *expand_SGS_3 = FEM_MHD_SGS_3Dfilter_expander(window, f_sgs_ctl->f_s3df_ctl, SGSWgts);
    
	GtkWidget *expand_s3 = draw_array_block_ctl_vbox(f_sgs_ctl->f_sph_filter_ctl,
                                                     c_append_SGS_sph_filter_ctl,
                                                     c_delete_SGS_sph_filter_ctl,
                                                     (void *) init_f_MHD_SGS_SPH_filter_control,
                                                     dealloc_f_MHD_SGS_SPH_filter_control,
                                                     (void *) draw_MHD_SGS_SPH_filter_expand,
                                                     SGSWgts->sfilter_Widgets, window);
    
    GtkWidget *expand_s1 = add_c_list_box_w_addbottun(f_sgs_ctl->f_SGS_terms_ctl, 
                                                      SGSWgts->f_SGS_terms_tree_view);
    GtkWidget *expand_s2 = add_c_list_box_w_addbottun(f_sgs_ctl->f_commutate_fld_ctl, 
                                                      SGSWgts->f_commutate_fld_trww_view);
    
    
    GtkWidget *hbox_s1 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_model_name_ctl);
    GtkWidget *hbox_s2 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_filter_name_ctl);
    GtkWidget *hbox_s3 = draw_chara_item_entry_hbox(f_sgs_ctl->f_DIFF_model_coef_ctl);
    GtkWidget *hbox_s4 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_negative_clip_ctl);
    GtkWidget *hbox_s5 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_marging_ctl);
    GtkWidget *hbox_s6 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_perturbation_ctl);
    
    GtkWidget *hbox_s7 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_model_coef_type_ctl);
    GtkWidget *hbox_s8 = draw_chara_item_entry_hbox(f_sgs_ctl->f_heat_flux_csim_type_ctl);
    GtkWidget *hbox_s9 = draw_chara_item_entry_hbox(f_sgs_ctl->f_comp_flux_csim_type_ctl);
    GtkWidget *hbox_s10 = draw_chara_item_entry_hbox(f_sgs_ctl->f_mom_flux_csim_type_ctl);
    GtkWidget *hbox_s11 = draw_chara_item_entry_hbox(f_sgs_ctl->f_maxwell_csim_type_ctl);
    GtkWidget *hbox_s12 = draw_chara_item_entry_hbox(f_sgs_ctl->f_uxb_csim_type_ctl);
    GtkWidget *hbox_s13 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_model_coef_coord_ctl);
    GtkWidget *hbox_s14 = draw_chara_item_entry_hbox(f_sgs_ctl->f_SGS_buo_Csim_usage_ctl);
    
    GtkWidget *hbox_s21 = draw_int_item_entry_hbox(f_sgs_ctl->f_istep_dynamic_ctl);
    GtkWidget *hbox_s22 = draw_int_item_entry_hbox(f_sgs_ctl->f_min_step_dynamic_ctl);
    GtkWidget *hbox_s23 = draw_int_item_entry_hbox(f_sgs_ctl->f_max_step_dynamic_ctl);
    
    GtkWidget *hbox_s24 = draw_real_item_entry_hbox(f_sgs_ctl->f_stabilize_weight_ctl);
    GtkWidget *hbox_s25 = draw_real_item_entry_hbox(f_sgs_ctl->f_delta_to_shrink_dynamic_ctl);
    GtkWidget *hbox_s26 = draw_real_item_entry_hbox(f_sgs_ctl->f_delta_to_extend_dynamic_ctl);
    
    GtkWidget *hbox_s27 = draw_int_item_entry_hbox(f_sgs_ctl->f_ngrp_radial_ave_ctl);
    GtkWidget *hbox_s28 = draw_int_item_entry_hbox(f_sgs_ctl->f_ngrp_med_ave_ctl);
    
    GtkWidget *hbox_s31 = draw_real_item_entry_hbox(f_sgs_ctl->f_clipping_limit_ctl);
    GtkWidget *hbox_s32 = draw_real_item_entry_hbox(f_sgs_ctl->f_SGS_hf_factor_ctl);
    GtkWidget *hbox_s33 = draw_real_item_entry_hbox(f_sgs_ctl->f_SGS_cf_factor_ctl);
    GtkWidget *hbox_s34 = draw_real_item_entry_hbox(f_sgs_ctl->f_SGS_mf_factor_ctl);
    GtkWidget *hbox_s35 = draw_real_item_entry_hbox(f_sgs_ctl->f_SGS_mxwl_factor_ctl);
    GtkWidget *hbox_s36 = draw_real_item_entry_hbox(f_sgs_ctl->f_SGS_uxb_factor_ctl);
    
    GtkWidget *vbox_s = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_s), expand_s1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), expand_s2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), expand_s3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s1,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s2,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s3,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s4,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s5,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s6,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s7,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s8,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s9,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s10, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s14, FALSE, FALSE, 0);
    
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s21, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s22, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s23, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s24, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s25, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s26, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s27, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s28, FALSE, FALSE, 0);
    
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s31, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s32, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s33, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s34, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s35, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_s36, FALSE, FALSE, 0);
    
    gtk_box_pack_start(GTK_BOX(vbox_s), expand_SGS_1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), expand_SGS_2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), expand_SGS_3, FALSE, FALSE, 0);
    
    GtkWidget *expand_MHD_s = draw_control_block(f_sgs_ctl->c_block_name, 
                                                 f_sgs_ctl->f_iflag,
                                                 window, vbox_s);
    return expand_MHD_s;
};
