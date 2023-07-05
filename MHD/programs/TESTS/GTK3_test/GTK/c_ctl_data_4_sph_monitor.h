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


struct f_sph_vol_spectr_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_volume_spec_file_ctl;
	struct f_ctl_chara_item *f_volume_ave_file_ctl;
	struct f_ctl_chara_item *f_volume_spec_format_ctl;
	struct f_ctl_chara_item *f_degree_v_spectra_switch;
	struct f_ctl_chara_item *f_order_v_spectra_switch;
	struct f_ctl_chara_item *f_diff_v_lm_spectra_switch;
	struct f_ctl_chara_item *f_axis_v_power_switch;
	struct f_ctl_real_item *f_inner_radius_ctl;
	struct f_ctl_real_item *f_outer_radius_ctl;
};

struct f_sph_field_on_circle_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_circle_field_file_ctl;
	struct f_ctl_chara_item *f_circle_spectr_file_ctl;
	struct f_ctl_chara_item *f_circle_file_format_ctl;
	struct f_ctl_chara_item *f_pick_circle_coord_ctl;\
	
	struct f_ctl_int_item   *f_nphi_mid_eq_ctl;
	struct f_ctl_real_item  *f_pick_s_ctl;
	struct f_ctl_real_item  *f_pick_z_ctl;
};

struct f_MHD_sph_pick_mode_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_picked_mode_head_ctl;
	struct f_ctl_chara_item *f_picked_mode_fmt_ctl;
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
	
	struct f_ctl_chara_item *f_gauss_coefs_prefix;
	struct f_ctl_chara_item *f_gauss_coefs_format;
	struct f_ctl_real_item *f_gauss_coefs_radius_ctl;
	struct int2_clist *f_idx_gauss_ctl;
	struct int_clist  *f_idx_gauss_l_ctl;
	struct int_clist  *f_idx_gauss_m_ctl;
};

struct f_MHD_sph_dipolarity_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_fdip_file_prefix_ctl;
	struct f_ctl_chara_item *f_fdip_file_format_ctl;
	struct int_clist  *f_fdip_truncation_ctl;
};

struct f_MHD_sph_dynamobench_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_dynamobench_file_ctl;
	struct f_ctl_chara_item *f_dynamobench_format_ctl;
	struct f_ctl_chara_item *f_detailed_dbench_file_ctl;
	struct f_ctl_chara_item *f_dbench_field_file_ctl;
	struct f_ctl_chara_item *f_dbench_spectr_file_ctl;
	struct f_ctl_int_item   *f_nphi_mid_eq_ctl;
};

struct f_MHD_sph_layer_spectr_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_layered_pwr_spectr_prefix;
	struct f_ctl_chara_item *f_layered_pwr_spectr_format;
	struct f_ctl_chara_item *f_degree_spectra_switch;
	struct f_ctl_chara_item *f_order_spectra_switch;
	struct f_ctl_chara_item *f_diff_lm_spectra_switch;
	struct f_ctl_chara_item *f_axis_power_switch;
	
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
	
	struct f_ctl_chara_item * f_volume_average_prefix;
	struct f_ctl_chara_item * f_volume_pwr_spectr_prefix;
	struct f_ctl_chara_item * f_volume_pwr_spectr_format;
	struct f_ctl_chara_item * f_degree_v_spectra_switch;
	struct f_ctl_chara_item * f_order_v_spectra_switch;
	struct f_ctl_chara_item * f_diff_v_lm_spectra_switch;
	struct f_ctl_chara_item * f_axis_v_power_switch;
	struct f_ctl_chara_item * f_heat_Nusselt_file_prefix;
	struct f_ctl_chara_item * f_comp_Nusselt_file_prefix;
	struct f_ctl_chara_item * f_heat_Nusselt_file_format;
	struct f_ctl_chara_item * f_comp_Nusselt_file_format;
	struct f_ctl_chara_item * f_typ_scale_file_prefix_ctl;
	struct f_ctl_chara_item * f_typ_scale_file_format_ctl;
};

/* prototypes */

extern int    c_sph_monitor_num_vspec_ctl(void *f_smonitor_ctl);
extern void * c_append_sph_mntr_vspec_ctl(int idx, char *block_name,
                                          void *f_smonitor_ctl);
extern void * c_delete_sph_mntr_vspec_ctl(int idx, void *f_smonitor_ctl);
extern void * c_sph_monitor_vspec_ctl(int idx, void *f_smonitor_ctl);

extern int    c_data_on_circles_num(void *f_circ_ctls);
extern void * c_data_on_circles_meq_ctl(int idx, void *f_circ_ctls);
extern void * c_append_circles_meq_ctl(int idx, char *c_name, void *f_circ_ctls);
extern void * c_delete_circles_meq_ctl(int idx, void *f_circ_ctls);

struct f_sph_vol_spectr_ctls * init_f_sph_vol_spectr_ctls(int idx, void *f_parent);
void * dealloc_f_sph_vol_spectr_ctls(void *f_item);

struct f_sph_field_on_circle_ctls * init_f_sph_field_on_circle_ctls(int idx, void *f_parent);
void * dealloc_f_sph_field_on_circle_ctls(void *f_item);

struct f_MHD_sph_monitor_ctls * init_f_MHD_sph_monitor_ctls(void *(*c_load_self)(void *f_parent),
															void *f_parent);


#endif /* c_ctl_data_4_sph_monitor_h_ */
