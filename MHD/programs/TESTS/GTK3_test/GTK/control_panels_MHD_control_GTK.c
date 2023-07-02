//
//  control_panels_MHD_control_GTK.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/1/23.
//

#include "control_panels_MHD_control_GTK.h"



static GtkWidget * draw_MHD_evo_scheme_control_vbox(struct f_MHD_evo_scheme_controls *f_mevo_ctl, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_mevo_ctl->f_scheme_ctl);
    
    GtkWidget *hbox_2 =  draw_real_item_entry_hbox(f_mevo_ctl->f_coef_implicit_ctl);
    GtkWidget *hbox_3 =  draw_real_item_entry_hbox(f_mevo_ctl->f_coef_imp_v_ctl);
    GtkWidget *hbox_4 =  draw_real_item_entry_hbox(f_mevo_ctl->f_coef_imp_t_ctl);
    GtkWidget *hbox_5 =  draw_real_item_entry_hbox(f_mevo_ctl->f_coef_imp_b_ctl);
    GtkWidget *hbox_6 =  draw_real_item_entry_hbox(f_mevo_ctl->f_coef_imp_c_ctl);
    
    GtkWidget *hbox_7 =  draw_chara_item_entry_hbox(f_mevo_ctl->f_Legendre_trans_type);
    GtkWidget *hbox_8 =  draw_chara_item_entry_hbox(f_mevo_ctl->f_FFT_library);
    GtkWidget *hbox_9 =  draw_chara_item_entry_hbox(f_mevo_ctl->f_import_mode);
    GtkWidget *hbox_10 = draw_int_item_entry_hbox(f_mevo_ctl->f_leg_vector_len);
    
    GtkWidget *hbox_11 = draw_chara_switch_entry_hbox(f_mevo_ctl->f_iflag_supg_ctl);
    GtkWidget *hbox_12 = draw_chara_switch_entry_hbox(f_mevo_ctl->f_iflag_supg_v_ctl);
    GtkWidget *hbox_13 = draw_chara_switch_entry_hbox(f_mevo_ctl->f_iflag_supg_t_ctl);
    GtkWidget *hbox_14 = draw_chara_switch_entry_hbox(f_mevo_ctl->f_iflag_supg_b_ctl);
    GtkWidget *hbox_15 = draw_chara_switch_entry_hbox(f_mevo_ctl->f_iflag_supg_c_ctl);
    
    GtkWidget *hbox_16 = draw_chara_item_entry_hbox(f_mevo_ctl->f_method_4_CN);
    GtkWidget *hbox_17 = draw_chara_item_entry_hbox(f_mevo_ctl->f_precond_4_CN);
    GtkWidget *hbox_18 = draw_int_item_entry_hbox(f_mevo_ctl->f_num_multi_pass_ctl);
    GtkWidget *hbox_19 = draw_int_item_entry_hbox(f_mevo_ctl->f_maxiter_ctl);
    GtkWidget *hbox_20 = draw_real_item_entry_hbox(f_mevo_ctl->f_eps_4_velo_ctl);
    GtkWidget *hbox_21 = draw_real_item_entry_hbox(f_mevo_ctl->f_eps_4_magne_ctl);
    GtkWidget *hbox_22 = draw_real_item_entry_hbox(f_mevo_ctl->f_eps_crank_ctl);
    GtkWidget *hbox_23 = draw_real_item_entry_hbox(f_mevo_ctl->f_eps_B_crank_ctl);
    
    GtkWidget *hbox_24 = draw_chara_switch_entry_hbox(f_mevo_ctl->f_diffuse_correct);
    
    GtkWidget *vbox_step = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_10, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_12, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_13, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_14, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_15, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_16, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_17, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_18, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_19, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_20, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_21, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_22, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_23, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_24, FALSE, FALSE, 0);
    
    GtkWidget *expand_rst = draw_control_block(f_mevo_ctl->c_block_name, f_mevo_ctl->f_iflag,
                                               480, 320, window, vbox_step);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_rst, FALSE, FALSE, 0);
    return vbox_out;
};

static GtkWidget * draw_MHD_restart_control_vbox(struct f_MHD_restart_controls *f_mrst_ctl, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_mrst_ctl->f_restart_flag_ctl);
    GtkWidget *vbox_step = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_1,  FALSE, FALSE, 0);
    
    GtkWidget *expand_rst = draw_control_block(f_mrst_ctl->c_block_name, f_mrst_ctl->f_iflag,
                                               480, 64, window, vbox_step);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_rst, FALSE, FALSE, 0);
    return vbox_out;
};

static GtkWidget * draw_time_step_control_vbox(struct f_time_step_control_ctls *f_tctl, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    
    GtkWidget *hbox_1 = draw_int_item_entry_hbox(f_tctl->f_i_step_init_ctl);
    GtkWidget *hbox_2 = draw_int_item_entry_hbox(f_tctl->f_i_step_number_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_tctl->f_elapsed_time_ctl);
    
    GtkWidget *hbox_4 = draw_int_item_entry_hbox(f_tctl->f_i_step_check_ctl);
    GtkWidget *hbox_5 = draw_int_item_entry_hbox(f_tctl->f_i_step_rst_ctl);
    GtkWidget *hbox_6 =  draw_int_item_entry_hbox(f_tctl->f_i_step_ucd_ctl);
    GtkWidget *hbox_7 =  draw_int_item_entry_hbox(f_tctl->f_i_step_monitor_ctl);
    
    GtkWidget *hbox_8 =  draw_int_item_entry_hbox(f_tctl->f_i_step_psf_ctl);
    GtkWidget *hbox_9 =  draw_int_item_entry_hbox(f_tctl->f_i_step_iso_ctl);
    GtkWidget *hbox_10 = draw_int_item_entry_hbox(f_tctl->f_i_step_map_ctl);
    GtkWidget *hbox_11 = draw_int_item_entry_hbox(f_tctl->f_i_step_pvr_ctl);
    GtkWidget *hbox_12 = draw_int_item_entry_hbox(f_tctl->f_i_step_lic_ctl);
    GtkWidget *hbox_13 = draw_int_item_entry_hbox(f_tctl->f_i_step_fline_ctl);
    
    GtkWidget *hbox_14 =  draw_int_item_entry_hbox(f_tctl->f_i_step_sgs_coefs_ctl);
    GtkWidget *hbox_15 =  draw_int_item_entry_hbox(f_tctl->f_i_step_boundary_ctl);
    
    GtkWidget *hbox_16 =  draw_real_item_entry_hbox(f_tctl->f_dt_ctl);
    GtkWidget *hbox_17 =  draw_real_item_entry_hbox(f_tctl->f_time_init_ctl);
    
    
    GtkWidget *hbox_18 =  draw_int_item_entry_hbox(f_tctl->f_i_diff_steps_ctl);
    GtkWidget *hbox_19 =  draw_chara_item_entry_hbox(f_tctl->f_flexible_step_ctl);
    
    GtkWidget *hbox_20 =  draw_real_item_entry_hbox(f_tctl->f_ratio_to_cfl_ctl);
    GtkWidget *hbox_21 =  draw_int_item_entry_hbox(f_tctl->f_start_rst_step_ctl);
    GtkWidget *hbox_22 =  draw_int_item_entry_hbox(f_tctl->f_end_rst_step_ctl);
    GtkWidget *hbox_23 =  draw_real_item_entry_hbox(f_tctl->f_min_delta_t_ctl);
    GtkWidget *hbox_24 =  draw_real_item_entry_hbox(f_tctl->f_max_delta_t_ctl);
    GtkWidget *hbox_25 =  draw_real_item_entry_hbox(f_tctl->f_max_eps_to_shrink_ctl);
    GtkWidget *hbox_26 =  draw_real_item_entry_hbox(f_tctl->f_min_eps_to_expand_ctl);
    GtkWidget *hbox_27 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_field_ctl);
    GtkWidget *hbox_28 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_monitor_ctl);
    
    GtkWidget *hbox_29 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_check_ctl);
    GtkWidget *hbox_30 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_rst_ctl);
    GtkWidget *hbox_31 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_psf_ctl);
    GtkWidget *hbox_32 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_iso_ctl);
    GtkWidget *hbox_33 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_map_ctl);
    GtkWidget *hbox_34 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_pvr_ctl);
    GtkWidget *hbox_35 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_lic_ctl);
    GtkWidget *hbox_36 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_fline_ctl);
    
    GtkWidget *hbox_37 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_sgs_coefs_ctl);
    GtkWidget *hbox_38 =  draw_real_item_entry_hbox(f_tctl->f_delta_t_boundary_ctl);
    
    GtkWidget *vbox_step = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_10, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_12, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_13, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_14, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_15, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_16, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_17, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_18, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_19, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_20, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_21, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_22, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_23, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_24, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_25, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_26, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_27, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_28, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_29, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_30, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_31, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_32, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_33, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_34, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_35, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_36, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_37, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_step), hbox_38, FALSE, FALSE, 0);
    
    GtkWidget *expand_step = draw_control_block(f_tctl->c_block_name, f_tctl->f_iflag,
                                               480, 320, window, vbox_step);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_step, FALSE, FALSE, 0);
    return vbox_out;
};

GtkWidget * draw_MHD_control_expand(GtkWidget *window, struct f_MHD_control_ctls *f_smctl_ctl)
{
    GtkWidget *expand_MHD_control;
    
    GtkWidget * vbox_tstep =      draw_time_step_control_vbox(f_smctl_ctl->f_tctl, window);
    GtkWidget * vbox_restart =    draw_MHD_restart_control_vbox(f_smctl_ctl->f_mrst_ctl, window);
    GtkWidget * vbox_evo_scheme = draw_MHD_evo_scheme_control_vbox(f_smctl_ctl->f_mevo_ctl, window);
    
    GtkWidget *vbox_control = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_control), vbox_tstep,      FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_control), vbox_restart,    FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_control), vbox_evo_scheme, FALSE, FALSE, 0);
    
    expand_MHD_control = draw_control_block(f_smctl_ctl->c_block_name, f_smctl_ctl->f_iflag,
                                            560, 500, window, vbox_control);
    return expand_MHD_control;
}
