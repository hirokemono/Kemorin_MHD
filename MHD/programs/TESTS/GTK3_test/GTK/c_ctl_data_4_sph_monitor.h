//
//  c_ctl_data_4_sph_monitor.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
//

#ifndef c_ctl_data_4_sph_monitor_h_
#define c_ctl_data_4_sph_monitor_h_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_int2_items_c.h"
#include "c_ctl_data_sph_monitor_arrays.h"


struct f_MHD_sph_pick_mode_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_picked_mode_head_ctl;
	struct chara_ctl_item *f_picked_mode_fmt_ctl;
	struct int_clist  *f_idx_pick_layer_ctl;
	struct real_clist *f_pick_radius_ctl;
	struct int2_clist *f_idx_pick_sph_ctl;
	struct int_clist  *f_idx_pick_sph_l_ctl;
	struct int_clist  *f_idx_pick_sph_m_ctl;
};

struct f_MHD_sph_gauss_coefs_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_gauss_coefs_prefix;
	struct chara_ctl_item *f_gauss_coefs_format;
	struct real_ctl_item *f_gauss_coefs_radius_ctl;
	struct int2_clist *f_idx_gauss_ctl;
	struct int_clist  *f_idx_gauss_l_ctl;
	struct int_clist  *f_idx_gauss_m_ctl;
};

struct f_MHD_sph_dipolarity_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_fdip_file_prefix_ctl;
	struct chara_ctl_item *f_fdip_file_format_ctl;
	struct int_clist  *f_fdip_truncation_ctl;
};

struct f_MHD_sph_dynamobench_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_dynamobench_file_ctl;
	struct chara_ctl_item *f_dynamobench_format_ctl;
	struct chara_ctl_item *f_detailed_dbench_file_ctl;
	struct chara_ctl_item *f_dbench_field_file_ctl;
	struct chara_ctl_item *f_dbench_spectr_file_ctl;
	struct int_ctl_item   *f_nphi_mid_eq_ctl;
};

struct f_MHD_sph_layer_spectr_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_layered_pwr_spectr_prefix;
    struct chara_ctl_item *f_layered_lorentz_spectr_prefix
	struct chara_ctl_item *f_layered_pwr_spectr_format;
	struct chara_ctl_item *f_degree_spectra_switch;
	struct chara_ctl_item *f_order_spectra_switch;
	struct chara_ctl_item *f_diff_lm_spectra_switch;
	struct chara_ctl_item *f_axis_power_switch;
	
	struct int_clist  *f_idx_spec_layer_ctl;
	struct real_clist *f_layer_radius_ctl;
};


struct f_MHD_sph_monitor_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	int f_num_vspec_ctl;
	struct void_clist *f_v_pwr;
	
	int f_num_circ_ctls;
	struct void_clist * f_circ_ctls;

	struct f_MHD_sph_layer_spectr_ctls *f_lp_ctl;
	struct f_MHD_sph_gauss_coefs_ctls  *f_g_pwr;
	struct f_MHD_sph_pick_mode_ctls    *f_pspec_ctl;
	struct f_MHD_sph_dynamobench_ctls  *f_dbench_ctl;
	struct f_MHD_sph_dipolarity_ctls   *f_fdip_ctl;
	
	struct chara_ctl_item * f_volume_average_prefix;
	struct chara_ctl_item * f_volume_pwr_spectr_prefix;
    struct chara_ctl_item * f_sph_mntr_vol_lor_work_prefix;
	struct chara_ctl_item * f_volume_pwr_spectr_format;
	struct chara_ctl_item * f_degree_v_spectra_switch;
	struct chara_ctl_item * f_order_v_spectra_switch;
	struct chara_ctl_item * f_diff_v_lm_spectra_switch;
	struct chara_ctl_item * f_axis_v_power_switch;
	struct chara_ctl_item * f_heat_Nusselt_file_prefix;
	struct chara_ctl_item * f_comp_Nusselt_file_prefix;
	struct chara_ctl_item * f_heat_Nusselt_file_format;
	struct chara_ctl_item * f_comp_Nusselt_file_format;
	struct chara_ctl_item * f_typ_scale_file_prefix_ctl;
	struct chara_ctl_item * f_typ_scale_file_format_ctl;
};

/* prototypes */

struct f_MHD_sph_monitor_ctls * init_f_MHD_sph_monitor_ctls(void *(*c_load_self)(void *f_parent),
															void *f_parent);


#endif /* c_ctl_data_4_sph_monitor_h_ */
