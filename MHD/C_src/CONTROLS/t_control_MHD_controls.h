/*
//  t_control_MHD_controls.h
//  
//
//  Created by Hiroaki Matsui on 2022/06/14.
*/

#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"


#ifndef T_CONTROL_MHD_CONTROLS_H_
#define T_CONTROL_MHD_CONTROLS_H_


struct f_MHD_restart_controls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_restart_flag_ctl;
};


struct f_time_step_control_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct int_ctl_item *f_i_step_init_ctl;
	struct int_ctl_item *f_i_step_number_ctl;
	struct real_ctl_item *f_elapsed_time_ctl;
	
	struct int_ctl_item *f_i_step_check_ctl;
	struct int_ctl_item *f_i_step_rst_ctl;
	struct int_ctl_item *f_i_step_pvr_ctl;
	struct int_ctl_item *f_i_step_psf_ctl;
	struct int_ctl_item *f_i_step_map_ctl;
	struct int_ctl_item *f_i_step_iso_ctl;
	struct int_ctl_item *f_i_step_lic_ctl;
	struct int_ctl_item *f_i_step_fline_ctl;
	
	struct int_ctl_item *f_i_step_ucd_ctl;
	struct int_ctl_item *f_i_step_monitor_ctl;
	struct int_ctl_item *f_i_step_sgs_coefs_ctl;
	struct int_ctl_item *f_i_step_boundary_ctl;
	
	struct real_ctl_item *f_dt_ctl;
	struct real_ctl_item *f_time_init_ctl;
	
	struct int_ctl_item *f_i_diff_steps_ctl;
	struct chara_ctl_item *f_flexible_step_ctl;
	
	struct real_ctl_item *f_ratio_to_cfl_ctl;
	struct int_ctl_item *f_start_rst_step_ctl;
	struct int_ctl_item *f_end_rst_step_ctl;
	struct real_ctl_item *f_min_delta_t_ctl;
	struct real_ctl_item *f_max_delta_t_ctl;
	struct real_ctl_item *f_max_eps_to_shrink_ctl;
	struct real_ctl_item *f_min_eps_to_expand_ctl;
	
	struct real_ctl_item *f_delta_t_check_ctl;
	struct real_ctl_item *f_delta_t_rst_ctl;
	
	struct real_ctl_item *f_delta_t_psf_ctl;
	struct real_ctl_item *f_delta_t_iso_ctl;
	struct real_ctl_item *f_delta_t_map_ctl;
	struct real_ctl_item *f_delta_t_pvr_ctl;
	struct real_ctl_item *f_delta_t_fline_ctl;
	struct real_ctl_item *f_delta_t_lic_ctl;
	
	struct real_ctl_item *f_delta_t_field_ctl;
	struct real_ctl_item *f_delta_t_monitor_ctl;
	struct real_ctl_item *f_delta_t_sgs_coefs_ctl;
	struct real_ctl_item *f_delta_t_boundary_ctl;
};

struct f_MHD_evo_scheme_controls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_scheme_ctl;
	
	struct real_ctl_item *f_coef_implicit_ctl;
	struct real_ctl_item *f_coef_imp_v_ctl;
	struct real_ctl_item *f_coef_imp_t_ctl;
	struct real_ctl_item *f_coef_imp_b_ctl;
	struct real_ctl_item *f_coef_imp_c_ctl;
	
	struct chara_ctl_item *f_iflag_supg_ctl;
	struct chara_ctl_item *f_iflag_supg_v_ctl;
	struct chara_ctl_item *f_iflag_supg_t_ctl;
	struct chara_ctl_item *f_iflag_supg_b_ctl;
	struct chara_ctl_item *f_iflag_supg_c_ctl;
	
	struct int_ctl_item *f_num_multi_pass_ctl;
	struct int_ctl_item *f_maxiter_ctl;
	struct real_ctl_item *f_eps_4_velo_ctl;
	struct real_ctl_item *f_eps_4_magne_ctl;
	struct real_ctl_item *f_eps_crank_ctl;
	struct real_ctl_item *f_eps_B_crank_ctl;
	
	struct chara_ctl_item *f_diffuse_correct;
	
	struct chara_ctl_item *f_method_4_CN;
	struct chara_ctl_item *f_precond_4_CN;
	struct chara_ctl_item *f_Legendre_trans_type;
	struct chara_ctl_item *f_FFT_library;
	struct chara_ctl_item *f_import_mode;
	struct int_ctl_item *f_leg_vector_len;
};


struct f_MHD_control_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	void * f_tctl;
	void * f_mrst_ctl;
	void * f_mevo_ctl;
};

/*  prototypes */

struct f_MHD_control_ctls * init_f_MHD_control_ctls(void *(*c_load_self)(void *f_parent), void *f_parent);


#endif    /* T_CONTROL_MHD_CONTROLS_H_ */
