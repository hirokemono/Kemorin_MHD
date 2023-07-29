//
//  c_ctl_data_4_sph_monitor.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
//

#include "c_ctl_data_4_sph_monitor.h"

extern void * c_sph_monitor_ctl_block_name(void *f_smonitor_ctl);
extern void * c_sph_monitor_ctl_iflag(void *f_smonitor_ctl);
extern void * c_sph_monitor_lp_ctl(void *f_smonitor_ctl);
extern void * c_sph_monitor_g_pwr(void *f_smonitor_ctl);
extern void * c_sph_monitor_pspec_ctl(void *f_smonitor_ctl);
extern void * c_sph_monitor_circ_ctls(void *f_smonitor_ctl);
extern void * c_sph_monitor_dbench_ctl(void *f_smonitor_ctl);
extern void * c_sph_monitor_fdip_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_vol_ave_prefix(void *f_smonitor_ctl);
extern void * c_sph_mntr_vol_pspec_prefix(void *f_smonitor_ctl);
extern void * c_sph_mntr_v_pwr_spectr_fmt(void *f_smonitor_ctl);
extern void * c_sph_mntr_degree_v_spectra_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_order_v_spectra_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_diff_v_lm_spectr_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_axis_v_power_switch(void *f_smonitor_ctl);
extern void * c_sph_mntr_h_Nusselt_file_pfx(void *f_smonitor_ctl);
extern void * c_sph_mntr_c_Nusselt_file_pfx(void *f_smonitor_ctl);
extern void * c_sph_mntr_h_Nusselt_file_fmt(void *f_smonitor_ctl);
extern void * c_sph_mntr_c_Nusselt_file_fmt(void *f_smonitor_ctl);
extern void * c_sph_mntr_lscale_file_pfix_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_lscale_file_fmt_ctl(void *f_smonitor_ctl);

extern void * c_sph_monitor_ctl_v_pwr_name(void *f_smonitor_ctl);
extern void * c_data_on_circles_block_name(void *f_circ_ctls);

extern void * c_pick_spectr_ctl_block_name(void *f_pspec_ctl);
extern void * c_pick_spectr_ctl_iflag(void *f_pspec_ctl);
extern void * c_sph_picked_mode_head_ctl(void *f_pspec_ctl);
extern void * c_sph_picked_mode_fmt_ctl(void *f_pspec_ctl);
extern void * c_sph_idx_pick_layer_ctl(void *f_pspec_ctl);
extern void * c_sph_pick_radius_ctl(void *f_pspec_ctl);
extern void * c_sph_idx_pick_sph_ctl(void *f_pspec_ctl);
extern void * c_sph_idx_pick_sph_l_ctl(void *f_pspec_ctl);
extern void * c_sph_idx_pick_sph_m_ctl(void *f_pspec_ctl);

extern void * c_sph_l_spectr_ctl_block_name(void *f_lp_ctl);
extern void * c_sph_l_spectr_ctl_iflag(void *f_lp_ctl);
extern void * c_sph_layer_pwr_spectr_prefix(void *f_lp_ctl);
extern void * c_sph_layer_pwr_spectr_format(void *f_lp_ctl);
extern void * c_sph_l_spec_degree_switch(void *f_lp_ctl);
extern void * c_sph_l_spec_order_switch(void *f_lp_ctl);
extern void * c_sph_l_spec_diff_lm_switch(void *f_lp_ctl);
extern void * c_sph_l_spec_axis_power_switch(void *f_lp_ctl);
extern void * c_sph_l_spectr_r_idx_ctl(void *f_lp_ctl);
extern void * c_sph_l_spectr_radius_ctl(void *f_lp_ctl);

extern void * c_sph_gauss_c_ctl_block_name(void *f_g_pwr);
extern void * c_sph_gauss_c_ctl_iflag(void *f_g_pwr);
extern void * c_sph_gauss_coefs_prefix(void *f_g_pwr);
extern void * c_sph_gauss_coefs_format(void *f_g_pwr);
extern void * c_sph_gauss_coefs_radius_ctl(void *f_g_pwr);
extern void * c_sph_idx_gauss_ctl(void *f_g_pwr);
extern void * c_sph_idx_gauss_l_ctl(void *f_g_pwr);
extern void * c_sph_idx_gauss_m_ctl(void *f_g_pwr);

extern void * c_dipolarity_ctl_block_name(void *f_fdip_ctl);
extern void * c_dipolarity_ctl_iflag(void *f_fdip_ctl);
extern void * c_dipolarity_file_prefix_ctl(void *f_fdip_ctl);
extern void * c_dipolarity_file_format_ctl(void *f_fdip_ctl);
extern void * c_dipolarity_truncation_ctl(void *f_fdip_ctl);

extern void * c_dynamobench_ctl_block_name(void *f_dbench_ctl);
extern void * c_dynamobench_ctl_iflag(void *f_dbench_ctl);
extern void * c_sph_dynamobench_file_ctl(void *f_dbench_ctl);
extern void * c_sph_dynamobench_format_ctl(void *f_dbench_ctl);
extern void * c_sph_detailed_dbench_file_ctl(void *f_dbench_ctl);
extern void * c_sph_dbench_field_file_ctl(void *f_dbench_ctl);
extern void * c_sph_dbench_spectr_file_ctl(void *f_dbench_ctl);
extern void * c_sph_dbench_nphi_mid_eq_ctl(void *f_dbench_ctl);


static struct f_MHD_sph_layer_spectr_ctls * init_f_MHD_sph_layer_spectr_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_sph_layer_spectr_ctls *f_lp_ctl 
			= (struct f_MHD_sph_layer_spectr_ctls *) malloc(sizeof(struct f_MHD_sph_layer_spectr_ctls));
	if(f_lp_ctl == NULL){
		printf("malloc error for f_lp_ctl\n");
		exit(0);
	};
	f_lp_ctl->f_self =  c_load_self(f_parent);
	
	f_lp_ctl->f_iflag = (int *) c_sph_l_spectr_ctl_iflag(f_lp_ctl->f_self);
	char *f_block_name =   (char *) c_sph_l_spectr_ctl_block_name(f_lp_ctl->f_self);
	f_lp_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_lp_ctl->f_layered_pwr_spectr_prefix = init_f_ctl_chara_item(c_sph_layer_pwr_spectr_prefix,
																  f_lp_ctl->f_self);
	f_lp_ctl->f_layered_pwr_spectr_format =  init_f_ctl_chara_item(c_sph_layer_pwr_spectr_format,
																   f_lp_ctl->f_self);
	f_lp_ctl->f_degree_spectra_switch =  init_f_ctl_chara_item(c_sph_l_spec_degree_switch,
															   f_lp_ctl->f_self);
	f_lp_ctl->f_order_spectra_switch =  init_f_ctl_chara_item(c_sph_l_spec_order_switch,
															  f_lp_ctl->f_self);
	f_lp_ctl->f_diff_lm_spectra_switch =  init_f_ctl_chara_item(c_sph_l_spec_diff_lm_switch, f_lp_ctl->f_self);
	f_lp_ctl->f_axis_power_switch =  init_f_ctl_chara_item(c_sph_l_spec_axis_power_switch,
														   f_lp_ctl->f_self);
	f_lp_ctl->f_idx_spec_layer_ctl =   init_f_ctl_int_array(c_sph_l_spectr_r_idx_ctl, 
															f_lp_ctl->f_self);
	f_lp_ctl->f_layer_radius_ctl =     init_f_ctl_real_array(c_sph_l_spectr_radius_ctl, 
															 f_lp_ctl->f_self);
	
	return f_lp_ctl;
}

static struct f_MHD_sph_gauss_coefs_ctls * init_f_MHD_sph_gauss_coefs_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_sph_gauss_coefs_ctls *f_g_pwr 
			= (struct f_MHD_sph_gauss_coefs_ctls *) malloc(sizeof(struct f_MHD_sph_gauss_coefs_ctls));
	if(f_g_pwr == NULL){
		printf("malloc error for f_g_pwr\n");
		exit(0);
	};
	
	f_g_pwr->f_self =  c_load_self(f_parent);
	
	f_g_pwr->f_iflag = (int *)      c_sph_gauss_c_ctl_iflag(f_g_pwr->f_self);
	char *f_block_name =   (char *) c_sph_gauss_c_ctl_block_name(f_g_pwr->f_self);
	f_g_pwr->c_block_name = strngcopy_from_f(f_block_name);
	
	f_g_pwr->f_gauss_coefs_prefix = init_f_ctl_chara_item(c_sph_gauss_coefs_prefix, 
														  f_g_pwr->f_self);
	f_g_pwr->f_gauss_coefs_format =  init_f_ctl_chara_item(c_sph_gauss_coefs_format, 
														   f_g_pwr->f_self);
	f_g_pwr->f_gauss_coefs_radius_ctl = init_f_ctl_real_item(c_sph_gauss_coefs_radius_ctl,
															 f_g_pwr->f_self);
	f_g_pwr->f_idx_gauss_ctl =     init_f_ctl_i2_array(c_sph_idx_gauss_ctl, 
													   f_g_pwr->f_self);
	f_g_pwr->f_idx_gauss_l_ctl =   init_f_ctl_int_array(c_sph_idx_gauss_l_ctl,
														f_g_pwr->f_self);
	f_g_pwr->f_idx_gauss_m_ctl =   init_f_ctl_int_array(c_sph_idx_gauss_m_ctl, 
														f_g_pwr->f_self);
	return f_g_pwr;
}


static struct f_MHD_sph_pick_mode_ctls * init_f_MHD_sph_pick_mode_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_sph_pick_mode_ctls *f_pspec_ctl 
			= (struct f_MHD_sph_pick_mode_ctls *) malloc(sizeof(struct f_MHD_sph_pick_mode_ctls));
	if(f_pspec_ctl == NULL){
		printf("malloc error for f_pspec_ctl\n");
		exit(0);
	};
	
	f_pspec_ctl->f_self =  c_load_self(f_parent);
	
	f_pspec_ctl->f_iflag = (int *) c_pick_spectr_ctl_iflag(f_pspec_ctl->f_self);
	char *f_block_name =   (char *) c_pick_spectr_ctl_block_name(f_pspec_ctl->f_self);
	f_pspec_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_pspec_ctl->f_picked_mode_head_ctl = init_f_ctl_chara_item(c_sph_picked_mode_head_ctl,
																f_pspec_ctl->f_self);
	f_pspec_ctl->f_picked_mode_fmt_ctl =  init_f_ctl_chara_item(c_sph_picked_mode_fmt_ctl,
																f_pspec_ctl->f_self);
	f_pspec_ctl->f_idx_pick_layer_ctl =   init_f_ctl_int_array(c_sph_idx_pick_layer_ctl,
															   f_pspec_ctl->f_self);
	f_pspec_ctl->f_pick_radius_ctl =      init_f_ctl_real_array(c_sph_pick_radius_ctl,
																f_pspec_ctl->f_self);
	f_pspec_ctl->f_idx_pick_sph_ctl =     init_f_ctl_i2_array(c_sph_idx_pick_sph_ctl, 
															  f_pspec_ctl->f_self);
	f_pspec_ctl->f_idx_pick_sph_l_ctl =   init_f_ctl_int_array(c_sph_idx_pick_sph_l_ctl,
															   f_pspec_ctl->f_self);
	f_pspec_ctl->f_idx_pick_sph_m_ctl =   init_f_ctl_int_array(c_sph_idx_pick_sph_m_ctl,
															   f_pspec_ctl->f_self);
	return f_pspec_ctl;
}

static struct f_MHD_sph_dynamobench_ctls * init_f_MHD_sph_dynamobench_ctls(void *(*c_load_self)(void *f_parent),
																		   void *f_parent)
{
	struct f_MHD_sph_dynamobench_ctls *f_dbench_ctl 
			= (struct f_MHD_sph_dynamobench_ctls *) malloc(sizeof(struct f_MHD_sph_dynamobench_ctls));
	if(f_dbench_ctl == NULL){
		printf("malloc error for f_dbench_ctl\n");
		exit(0);
	};
	f_dbench_ctl->f_self =  c_load_self(f_parent);
	
	f_dbench_ctl->f_iflag = (int *) c_dynamobench_ctl_iflag(f_dbench_ctl->f_self);
	char *f_block_name =   (char *) c_dynamobench_ctl_block_name(f_dbench_ctl->f_self);
	f_dbench_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_dbench_ctl->f_dynamobench_file_ctl = init_f_ctl_chara_item(c_sph_dynamobench_file_ctl, f_dbench_ctl->f_self);
	f_dbench_ctl->f_dynamobench_format_ctl =  init_f_ctl_chara_item(c_sph_dynamobench_format_ctl, f_dbench_ctl->f_self);
	f_dbench_ctl->f_detailed_dbench_file_ctl =  init_f_ctl_chara_item(c_sph_detailed_dbench_file_ctl, f_dbench_ctl->f_self);
	f_dbench_ctl->f_dbench_field_file_ctl =  init_f_ctl_chara_item(c_sph_dbench_field_file_ctl, f_dbench_ctl->f_self);
	f_dbench_ctl->f_dbench_spectr_file_ctl =  init_f_ctl_chara_item(c_sph_dbench_spectr_file_ctl, f_dbench_ctl->f_self);
	f_dbench_ctl->f_nphi_mid_eq_ctl =  init_f_ctl_int_item(c_sph_dbench_nphi_mid_eq_ctl, f_dbench_ctl->f_self);
	return f_dbench_ctl;
}

static struct f_MHD_sph_dipolarity_ctls * init_f_MHD_sph_dipolarity_ctls(void *(*c_load_self)(void *f_parent), 
																		 void *f_parent)
{
	struct f_MHD_sph_dipolarity_ctls *f_fdip_ctl 
			= (struct f_MHD_sph_dipolarity_ctls *) malloc(sizeof(struct f_MHD_sph_dipolarity_ctls));
	if(f_fdip_ctl == NULL){
		printf("malloc error for f_fdip_ctl\n");
		exit(0);
	};
	f_fdip_ctl->f_self =  c_load_self(f_parent);
	
	f_fdip_ctl->f_iflag = (int *) c_dipolarity_ctl_iflag(f_fdip_ctl->f_self);
	char *f_block_name =   (char *) c_dipolarity_ctl_block_name(f_fdip_ctl->f_self);
	f_fdip_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_fdip_ctl->f_fdip_file_prefix_ctl = init_f_ctl_chara_item(c_dipolarity_file_prefix_ctl, f_fdip_ctl->f_self);
	f_fdip_ctl->f_fdip_file_format_ctl =  init_f_ctl_chara_item(c_dipolarity_file_format_ctl, f_fdip_ctl->f_self);
	f_fdip_ctl->f_fdip_truncation_ctl =   init_f_ctl_int_array(c_dipolarity_truncation_ctl, f_fdip_ctl->f_self);
	return f_fdip_ctl;
}

static void init_f_MHD_sph_monitor_arrays(struct f_MHD_sph_monitor_ctls *f_smonitor_ctl)
{
	f_smonitor_ctl->f_num_vspec_ctl = c_sph_monitor_num_vspec_ctl(f_smonitor_ctl->f_self);
    char *f_block_name =   (char *) c_sph_monitor_ctl_v_pwr_name(f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_v_pwr = init_void_clist(strngcopy_from_f(f_block_name));
	f_smonitor_ctl->f_v_pwr->f_parent =  f_smonitor_ctl->f_self;
	
    void *void_in = NULL;
	int i;
	for(i=0;i<f_smonitor_ctl->f_num_vspec_ctl;i++){
        struct f_sph_vol_spectr_ctls *void_in = init_f_sph_vol_spectr_ctls(i, NULL,
                                                                           f_smonitor_ctl->f_self);
		append_void_clist((void *) void_in, f_smonitor_ctl->f_v_pwr);
	}
	f_smonitor_ctl->f_num_circ_ctls = c_data_on_circles_num(f_smonitor_ctl->f_self);
    f_block_name =   (char *) c_data_on_circles_block_name(f_smonitor_ctl->f_self);
 	f_smonitor_ctl->f_circ_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_smonitor_ctl->f_circ_ctls->f_parent =  f_smonitor_ctl->f_self;
	for(i=0;i<f_smonitor_ctl->f_num_circ_ctls;i++){
        struct f_sph_field_on_circle_ctls *void_in = init_f_sph_field_on_circle_ctls(i, NULL,
                                                                                     f_smonitor_ctl->f_self);
		append_void_clist((void *) void_in, f_smonitor_ctl->f_circ_ctls);
	}
	return;
}

struct f_MHD_sph_monitor_ctls * init_f_MHD_sph_monitor_ctls(void *(*c_load_self)(void *f_parent),
															void *f_parent)
{
	struct f_MHD_sph_monitor_ctls *f_smonitor_ctl 
			= (struct f_MHD_sph_monitor_ctls *) malloc(sizeof(struct f_MHD_sph_monitor_ctls));
	if(f_smonitor_ctl == NULL){
		printf("malloc error for f_smonitor_ctl\n");
		exit(0);
	};
	
	f_smonitor_ctl->f_self =  c_load_self(f_parent);
	
	f_smonitor_ctl->f_iflag = (int *) c_sph_monitor_ctl_iflag(f_smonitor_ctl->f_self);
	char *f_block_name =   (char *) c_sph_monitor_ctl_block_name(f_smonitor_ctl->f_self);
	f_smonitor_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	init_f_MHD_sph_monitor_arrays(f_smonitor_ctl);
	
	f_smonitor_ctl->f_lp_ctl =     init_f_MHD_sph_layer_spectr_ctls(c_sph_monitor_lp_ctl,
																	f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_g_pwr =      init_f_MHD_sph_gauss_coefs_ctls(c_sph_monitor_g_pwr, 
																   f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_pspec_ctl =  init_f_MHD_sph_pick_mode_ctls(c_sph_monitor_pspec_ctl,
																 f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_dbench_ctl = init_f_MHD_sph_dynamobench_ctls(c_sph_monitor_dbench_ctl, 
																   f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_fdip_ctl =   init_f_MHD_sph_dipolarity_ctls(c_sph_monitor_fdip_ctl, 
																  f_smonitor_ctl->f_self);
	
	f_smonitor_ctl->f_volume_average_prefix 
			=     init_f_ctl_chara_item(c_sph_mntr_vol_ave_prefix, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_volume_pwr_spectr_prefix 
			=  init_f_ctl_chara_item(c_sph_mntr_vol_pspec_prefix, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_volume_pwr_spectr_format
			=  init_f_ctl_chara_item(c_sph_mntr_v_pwr_spectr_fmt, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_degree_v_spectra_switch 
			=   init_f_ctl_chara_item(c_sph_mntr_degree_v_spectra_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_order_v_spectra_switch 
			=    init_f_ctl_chara_item(c_sph_mntr_order_v_spectra_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_diff_v_lm_spectra_switch 
			=  init_f_ctl_chara_item(c_sph_mntr_diff_v_lm_spectr_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_axis_v_power_switch 
			=       init_f_ctl_chara_item(c_sph_mntr_axis_v_power_switch, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_heat_Nusselt_file_prefix 
			=  init_f_ctl_chara_item(c_sph_mntr_h_Nusselt_file_pfx, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_comp_Nusselt_file_prefix 
			=  init_f_ctl_chara_item(c_sph_mntr_c_Nusselt_file_pfx, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_heat_Nusselt_file_format 
			=  init_f_ctl_chara_item(c_sph_mntr_h_Nusselt_file_fmt, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_comp_Nusselt_file_format 
			=  init_f_ctl_chara_item(c_sph_mntr_c_Nusselt_file_fmt, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_typ_scale_file_prefix_ctl 
			= init_f_ctl_chara_item(c_sph_mntr_lscale_file_pfix_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_typ_scale_file_format_ctl 
			= init_f_ctl_chara_item(c_sph_mntr_lscale_file_fmt_ctl, f_smonitor_ctl->f_self);
	return f_smonitor_ctl;
}
