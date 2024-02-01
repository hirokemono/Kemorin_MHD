/*
//  control_panel_4_sph_monitor_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_sph_monitor_GTK.h"

static GtkWidget * draw_sph_layer_spectr_ctls_vbox(struct f_MHD_sph_layer_spectr_ctls *f_lp_ctl, 
											struct f_sph_monitor_widgets *f_sph_monitor_vws,
											GtkWidget *window){
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_lp_ctl->f_layered_pwr_spectr_prefix);
    GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_lp_ctl->f_layered_pwr_spectr_prefix);
	GtkWidget *hbox_c3 = draw_chara_item_entry_hbox(f_lp_ctl->f_layered_lorentz_spectr_prefix);
	GtkWidget *hbox_a1 = real_array_vbox_w_addbottun(f_lp_ctl->f_layer_radius_ctl,
													 f_sph_monitor_vws->f_layer_radius_ctl_tree);
	GtkWidget *hbox_a2 = add_int_list_box_w_addbottun(f_lp_ctl->f_idx_spec_layer_ctl,
													  f_sph_monitor_vws->f_idx_spec_layer_ctl_tree);
    GtkWidget *hbox_c3 = draw_chara_switch_entry_hbox(f_lp_ctl->f_degree_spectra_switch);
    GtkWidget *hbox_c4 = draw_chara_switch_entry_hbox(f_lp_ctl->f_order_spectra_switch);
    GtkWidget *hbox_c5 = draw_chara_switch_entry_hbox(f_lp_ctl->f_diff_lm_spectra_switch);
    GtkWidget *hbox_c6 = draw_chara_switch_entry_hbox(f_lp_ctl->f_axis_power_switch);
	
	GtkWidget *vbox_pick = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c5, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c6, FALSE, FALSE, 0);
	
	GtkWidget *expand_lspecs = draw_control_block(f_lp_ctl->c_block_name, f_lp_ctl->f_iflag,
												  window, vbox_pick);
	return expand_lspecs;
};

static GtkWidget * draw_sph_pick_mode_ctls_vbox(struct f_MHD_sph_pick_mode_ctls *f_pspec_ctl,
										 struct f_sph_monitor_widgets *f_sph_monitor_vws,
										 GtkWidget *window){
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_pspec_ctl->f_picked_mode_head_ctl);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_pspec_ctl->f_picked_mode_fmt_ctl);
	
	GtkWidget *hbox_a1 = real_array_vbox_w_addbottun(f_pspec_ctl->f_pick_radius_ctl,
													 f_sph_monitor_vws->f_pick_radius_ctl_tree);
	GtkWidget *hbox_a2 = add_int_list_box_w_addbottun(f_pspec_ctl->f_idx_pick_layer_ctl,
													  f_sph_monitor_vws->f_idx_pick_layer_ctl_tree);
	GtkWidget *hbox_a3 = add_i2_list_box_w_addbottun(f_pspec_ctl->f_idx_pick_sph_ctl,
													 f_sph_monitor_vws->f_idx_pick_sph_ctl_tree);
	GtkWidget *hbox_a4 = add_int_list_box_w_addbottun(f_pspec_ctl->f_idx_pick_sph_l_ctl,
													  f_sph_monitor_vws->f_idx_pick_sph_l_ctl_tree);
	GtkWidget *hbox_a5 = add_int_list_box_w_addbottun(f_pspec_ctl->f_idx_pick_sph_m_ctl,
													  f_sph_monitor_vws->f_idx_pick_sph_m_ctl_tree);
	
	GtkWidget *vbox_pick = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a5, FALSE, FALSE, 0);
	
	GtkWidget *expand_vpwr = draw_control_block(f_pspec_ctl->c_block_name, f_pspec_ctl->f_iflag,
												  window, vbox_pick);
	return expand_vpwr;
};

static GtkWidget * draw_sph_gauss_coef_ctls_vbox(struct f_MHD_sph_gauss_coefs_ctls *f_g_pwr, 
										  struct f_sph_monitor_widgets *f_sph_monitor_vws,
										  GtkWidget *window){
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_g_pwr->f_gauss_coefs_prefix);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_g_pwr->f_gauss_coefs_format);
	GtkWidget *hbox_r1 = draw_real_item_entry_hbox(f_g_pwr->f_gauss_coefs_radius_ctl);
	
	GtkWidget *hbox_a3 = add_i2_list_box_w_addbottun(f_g_pwr->f_idx_gauss_ctl,
													 f_sph_monitor_vws->f_idx_gauss_ctl_tree);
	printf("box f_idx_gauss_l_ctl\n");
	GtkWidget *hbox_a4 = add_int_list_box_w_addbottun(f_g_pwr->f_idx_gauss_l_ctl,
													  f_sph_monitor_vws->f_idx_gauss_l_ctl_tree);
	printf("box f_idx_gauss_m_ctl\n");
	GtkWidget *hbox_a5 = add_int_list_box_w_addbottun(f_g_pwr->f_idx_gauss_m_ctl,
													  f_sph_monitor_vws->f_idx_gauss_m_ctl_tree);
	
	GtkWidget *vbox_pick = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_r1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a5, FALSE, FALSE, 0);
	
	GtkWidget *expand_vpwrs = draw_control_block(f_g_pwr->c_block_name, f_g_pwr->f_iflag,
												  window, vbox_pick);
	return expand_vpwrs;
};

static GtkWidget * draw_sph_dipolarity_ctls_vbox(struct f_MHD_sph_dipolarity_ctls *f_fdip_ctl, 
										  struct f_sph_monitor_widgets *f_sph_monitor_vws,
										  GtkWidget *window){
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_fdip_ctl->f_fdip_file_prefix_ctl);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_fdip_ctl->f_fdip_file_format_ctl);
	
	GtkWidget *hbox_a3 = add_int_list_box_w_addbottun(f_fdip_ctl->f_fdip_truncation_ctl,
													  f_sph_monitor_vws->f_fdip_truncation_ctl_tree);
	
	GtkWidget *vbox_pick = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_a3, FALSE, FALSE, 0);
	
	GtkWidget *expand_fdip = draw_control_block(f_fdip_ctl->c_block_name, f_fdip_ctl->f_iflag,
												  window, vbox_pick);
	return expand_fdip;
};

static GtkWidget * draw_sph_dynamobench_ctls_vbox(struct f_MHD_sph_dynamobench_ctls *f_dbench_ctl, 
										   GtkWidget *window){
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_dbench_ctl->f_dynamobench_file_ctl);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_dbench_ctl->f_dynamobench_format_ctl);
	GtkWidget *hbox_c3 = draw_chara_item_entry_hbox(f_dbench_ctl->f_detailed_dbench_file_ctl);
	GtkWidget *hbox_c4 = draw_chara_item_entry_hbox(f_dbench_ctl->f_dbench_field_file_ctl);
	GtkWidget *hbox_c5 = draw_chara_item_entry_hbox(f_dbench_ctl->f_dbench_spectr_file_ctl);
	GtkWidget *hbox_c6 = draw_int_item_entry_hbox(f_dbench_ctl->f_nphi_mid_eq_ctl);
	
	GtkWidget *vbox_pick = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c5, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_pick), hbox_c6, FALSE, FALSE, 0);
	
	GtkWidget *expand_dbench = draw_control_block(f_dbench_ctl->c_block_name, f_dbench_ctl->f_iflag,
												  window, vbox_pick);
	return expand_dbench;
};

static GtkWidget * draw_sph_each_vspec_ctl_expand(char *label_name, void *block_item, GtkWidget *window){
	struct f_sph_vol_spectr_ctls *f_v_pwr_item = (struct f_sph_vol_spectr_ctls *) block_item;
	GtkWidget *vbox_v_pwr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_ave_file_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_spec_file_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_v_pwr_item->f_sph_volume_lor_spec_file_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_spec_format_ctl);
    GtkWidget *hbox_5 = draw_real_item_entry_hbox(f_v_pwr_item->f_inner_radius_ctl);
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_v_pwr_item->f_outer_radius_ctl);
    GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_degree_v_spectra_switch);
    GtkWidget *hbox_8 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_order_v_spectra_switch);
    GtkWidget *hbox_9 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_diff_v_lm_spectra_switch);
    GtkWidget *hbox_10 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_axis_v_power_switch);
	
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_10, FALSE, FALSE, 0);
	GtkWidget *expand_v_pwr = draw_control_block(duplicate_underscore(label_name),
												 f_v_pwr_item->f_iflag,
												  window, expand_v_pwr);
    return expand_v_pwr;
};

static GtkWidget * draw_sph_each_fld_on_circle_ctl_expand(char *label_name, void *block_item, GtkWidget *window){
	struct f_sph_field_on_circle_ctls *f_circ_ctls = (struct f_sph_field_on_circle_ctls *) block_item;
	GtkWidget *vbox_dcirc = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_circ_ctls->f_circle_field_file_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_circ_ctls->f_circle_spectr_file_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_circ_ctls->f_circle_file_format_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_circ_ctls->f_pick_circle_coord_ctl);
    GtkWidget *hbox_5 = draw_int_item_entry_hbox(f_circ_ctls->f_nphi_mid_eq_ctl);
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_circ_ctls->f_pick_s_ctl);
    GtkWidget *hbox_7 = draw_real_item_entry_hbox(f_circ_ctls->f_pick_z_ctl);
	
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_7,  FALSE, FALSE, 0);
    GtkWidget *expand_dcirc = draw_control_block(duplicate_underscore(label_name), f_circ_ctls->f_iflag,
												  window, vbox_dcirc);
    return expand_dcirc;
};


GtkWidget * draw_MHD_sph_monitor_ctls_vbox(struct f_MHD_sph_monitor_ctls *f_smonitor_ctl,
										   struct f_sph_monitor_widgets *f_lp_vws,
										   GtkWidget *window){
	f_lp_vws = (struct f_sph_monitor_widgets *) malloc(sizeof(struct f_sph_monitor_widgets));
	if(f_lp_vws == NULL){
		printf("malloc error for f_sph_monitor_widgets\n");
		exit(0);
	};
	
	
	GtkWidget *expand_vpwrs = draw_array_block_ctl_vbox(f_smonitor_ctl->f_v_pwr, NULL, 
                                                        c_append_sph_mntr_vspec_ctl,
                                                        c_delete_sph_mntr_vspec_ctl,
                                                        (void *) init_f_sph_vol_spectr_ctls,
                                                        (void *) dealloc_f_sph_vol_spectr_ctls,
                                                        (void *) draw_sph_each_vspec_ctl_expand,
                                                        f_lp_vws->vpwr_Widgets, window);
	GtkWidget *expand_dcircs = draw_array_block_ctl_vbox(f_smonitor_ctl->f_circ_ctls, NULL,
                                                         c_append_circles_meq_ctl,
                                                         c_delete_circles_meq_ctl,
                                                         (void *) init_f_sph_field_on_circle_ctls,
                                                         (void *) dealloc_f_sph_field_on_circle_ctls,
                                                         (void *) draw_sph_each_fld_on_circle_ctl_expand,
                                                         f_lp_vws->dcirc_Widgets, window);
	GtkWidget *vbox_smontr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	
	GtkWidget *expand_layer_spec = draw_sph_layer_spectr_ctls_vbox(f_smonitor_ctl->f_lp_ctl,
																   f_lp_vws, window);
	GtkWidget *expand_pick_mode = draw_sph_pick_mode_ctls_vbox(f_smonitor_ctl->f_pspec_ctl, 
															   f_lp_vws, window);
	GtkWidget *expand_gauss_c = draw_sph_gauss_coef_ctls_vbox(f_smonitor_ctl->f_g_pwr, 
															   f_lp_vws, window);
	GtkWidget *expand_dipolarity_c = draw_sph_dipolarity_ctls_vbox(f_smonitor_ctl->f_fdip_ctl, 
																   f_lp_vws, window);
	
	GtkWidget *expand_dbench_c = draw_sph_dynamobench_ctls_vbox(f_smonitor_ctl->f_dbench_ctl, 
																    window);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_volume_average_prefix);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_volume_pwr_spectr_prefix);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_sph_mntr_vol_lor_work_prefix);
	GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_volume_pwr_spectr_format);
	
    GtkWidget *hbox_5 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_degree_v_spectra_switch);
    GtkWidget *hbox_6 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_order_v_spectra_switch);
    GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_diff_v_lm_spectra_switch);
	GtkWidget *hbox_8 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_axis_v_power_switch);
	
    GtkWidget *hbox_9 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_heat_Nusselt_file_prefix);
	GtkWidget *hbox_10 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_heat_Nusselt_file_format);
	
    GtkWidget *hbox_11 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_comp_Nusselt_file_prefix);
	GtkWidget *hbox_12 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_comp_Nusselt_file_format);
	
    GtkWidget *hbox_13 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_typ_scale_file_prefix_ctl);
    GtkWidget *hbox_14 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_typ_scale_file_format_ctl);
	
	
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_6  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_7,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_8,  FALSE, FALSE, 0);
	
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_9,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_10,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_12, FALSE, FALSE, 0);
	
	gtk_container_add(GTK_CONTAINER(vbox_smontr), expand_vpwrs);
	gtk_box_pack_start(GTK_BOX(vbox_smontr), expand_layer_spec,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_smontr), expand_pick_mode,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_smontr), expand_gauss_c,  FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_smontr), expand_dipolarity_c,  FALSE, FALSE, 0);
	
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_14, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_smontr), expand_dbench_c,  FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(vbox_smontr), expand_dcircs);
	
	GtkWidget *expand_smntr = draw_control_block(f_smonitor_ctl->c_block_name, f_smonitor_ctl->f_iflag,
												  window, vbox_smontr);
    return expand_smntr;
};

