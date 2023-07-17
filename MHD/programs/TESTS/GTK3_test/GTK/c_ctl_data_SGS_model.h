/*
//  c_ctl_data_SGS_model.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_SGS_MODEL_H_
#define C_CTL_DATA_SGS_MODEL_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_ctl_array_single_items_c.h"



struct f_MHD_SGS_SPH_filter_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_sph_filter_type_ctl;
	struct chara_ctl_item *f_radial_filter_type_ctl;
	struct int_ctl_item *f_maximum_moments_ctl;
	struct real_ctl_item *f_sphere_filter_width_ctl;
	struct real_ctl_item *f_radial_filter_width_ctl;
	struct int_ctl_item *f_first_reference_ctl;
	struct int_ctl_item *f_second_reference_ctl;
};

struct f_MHD_SGS_3d_filter_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_clist *f_whole_filter_grp_ctl;
	struct chara_clist *f_fluid_filter_grp_ctl;
	struct chara_ctl_item *f_momentum_filter_ctl;
	struct chara_ctl_item *f_heat_filter_ctl;
	struct chara_ctl_item *f_induction_filter_ctl;
	struct chara_ctl_item *f_compostion_filter_ctl;
};

struct f_MHD_SGS_layer_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_layering_grp_type_ctl;
	struct chara_clist *f_layer_grp_name_ctl;
	struct int_clist   *f_igrp_stack_layer_ctl;
	struct int_ctl_item   *f_num_layering_grp_ctl;
	struct int_ctl_item   *f_num_fl_layer_grp_ctl;
	struct chara_ctl_item *f_start_layering_grp_name_ctl;
	struct chara_ctl_item *f_start_fl_layer_grp_name_ctl;
	struct int_ctl_item   *f_ngrp_SGS_on_sphere_ctl;
};

struct f_MHD_SGS_filter_file_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_filter_head_ctl;
	struct chara_ctl_item *f_filter_coef_head_ctl;
	struct chara_ctl_item *f_filter_elen_head_ctl;
	struct chara_ctl_item *f_filter_moms_head_ctl;
	struct chara_ctl_item *f_filter_wide_head_ctl;
	struct chara_ctl_item *f_model_coef_ini_head_ctl;
	struct chara_ctl_item *f_commute_coef_ini_head_ctl;
	struct chara_ctl_item *f_filter_elen_format;
	struct chara_ctl_item *f_filter_3d_format;
	struct chara_ctl_item *f_filter_wide_format;
	struct chara_ctl_item *f_model_coef_rst_format;
	struct chara_ctl_item *f_commute_coef_rst_format;
};

struct f_MHD_SGS_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_SGS_model_name_ctl;
	struct chara_ctl_item *f_SGS_filter_name_ctl;
	struct chara_ctl_item *f_DIFF_model_coef_ctl;
	struct chara_ctl_item *f_SGS_negative_clip_ctl;
	struct chara_ctl_item *f_SGS_marging_ctl;
	struct chara_ctl_item *f_SGS_perturbation_ctl;
	struct chara_ctl_item *f_SGS_model_coef_type_ctl;
	struct chara_ctl_item *f_heat_flux_csim_type_ctl;
	struct chara_ctl_item *f_comp_flux_csim_type_ctl;
	struct chara_ctl_item *f_mom_flux_csim_type_ctl;
	struct chara_ctl_item *f_maxwell_csim_type_ctl;
	struct chara_ctl_item *f_uxb_csim_type_ctl;
	struct chara_ctl_item *f_SGS_model_coef_coord_ctl;
	struct chara_ctl_item *f_SGS_buo_Csim_usage_ctl;
	
	struct int_ctl_item *f_istep_dynamic_ctl;
	struct int_ctl_item *f_min_step_dynamic_ctl;
	struct int_ctl_item *f_max_step_dynamic_ctl;
	
	struct real_ctl_item *f_stabilize_weight_ctl;
	struct real_ctl_item *f_delta_to_shrink_dynamic_ctl;
	struct real_ctl_item *f_delta_to_extend_dynamic_ctl;
	
	struct int_ctl_item *f_ngrp_radial_ave_ctl;
	struct int_ctl_item *f_ngrp_med_ave_ctl;
	
	struct real_ctl_item *f_clipping_limit_ctl;
	struct real_ctl_item *f_SGS_hf_factor_ctl;
	struct real_ctl_item *f_SGS_cf_factor_ctl;
	struct real_ctl_item *f_SGS_mf_factor_ctl;
	struct real_ctl_item *f_SGS_mxwl_factor_ctl;
	struct real_ctl_item *f_SGS_uxb_factor_ctl;
	
	struct chara_clist *f_SGS_terms_ctl;
	struct chara_clist *f_commutate_fld_ctl;
	
	struct f_MHD_SGS_filter_file_control *f_ffile_ctl;
	struct f_MHD_SGS_layer_control       *f_elayer_ctl;
	struct f_MHD_SGS_3d_filter_control    *f_s3df_ctl;
	
	char *f_sph_filter_name;
	int   f_num_sph_filter_ctl;
	struct void_clist *f_sph_filter_ctl;
};

/* prototypes */
extern void * c_append_SGS_sph_filter_ctl(int idx, char *block_name, void *f_sph_filter_ctl);
extern void * c_delete_SGS_sph_filter_ctl(int idx, void *f_sph_filter_ctl);

struct f_MHD_SGS_SPH_filter_control * init_f_MHD_SGS_SPH_filter_control(int idx_in, void *f_parent);
void *dealloc_f_MHD_SGS_SPH_filter_control(void *void_in);


struct f_MHD_SGS_model_control * init_f_MHD_SGS_model_control(void *(*c_load_self)(void *f_parent),
															  void *f_parent);

#endif /* C_CTL_DATA_SGS_MODEL_H_ */
