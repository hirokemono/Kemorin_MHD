/*
//  c_ctl_data_SGS_model.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_SGS_model.h"

extern void * c_SGS_model_ctl_block_name(void *f_sgs_ctl);
extern void * c_SGS_model_ctl_iflag(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_model_name_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_filter_name_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_DIFF_model_c_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_negative_clip_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_marging_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_perturbation_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_m_coef_type_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_hflux_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_cflux_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_mflux_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_maxwell_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_uxb_csim_type_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_coef_coord_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_buo_Csim_usage(void *f_sgs_ctl);
extern void * c_SGS_model_istep_dynamic_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_min_step_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_max_step_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_stabilize_weight(void *f_sgs_ctl);
extern void * c_SGS_model_del_shrink_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_del_extend_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_ngrp_radial_ave(void *f_sgs_ctl);
extern void * c_SGS_model_ngrp_med_ave_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_clipping_limit_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_hf_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_cf_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_mf_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_mxwl_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_uxb_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_terms_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_commutate_fld_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_ffile_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_elayer_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_s3df_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_sph_filter_name(void *f_sgs_ctl);
extern int    c_SGS_model_num_sph_filter_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_sph_filter_ctl(int i, void *f_sgs_ctl);

extern void * c_SGS_3d_filter_ctl_block_name(void *f_s3df_ctl);
extern void * c_SGS_3d_filter_ctl_iflag(void *f_s3df_ctl);
extern void * c_SGS_3d_whole_filter_grp_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_fluid_filter_grp_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_momentum_filter_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_heat_filter_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_induction_filter_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_comp_filter_ctl(void *f_s3df_ctl);

extern void * c_SGS_filter_file_block_name(void *f_ffile_ctl);
extern void * c_SGS_filter_file_iflag(void *f_ffile_ctl);
extern void * c_SGS_filter_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_coef_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_elen_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_moms_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_wide_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_model_coef_ini_head(void *f_ffile_ctl);
extern void * c_SGS_commute_coef_ini_head(void *f_ffile_ctl);
extern void * c_SGS_filter_elen_format(void *f_ffile_ctl);
extern void * c_SGS_filter_3d_format(void *f_ffile_ctl);
extern void * c_SGS_filter_wide_format(void *f_ffile_ctl);
extern void * c_SGS_model_coef_rst_format(void *f_ffile_ctl);
extern void * c_SGS_commute_coef_rst_format(void *f_ffile_ctl);

extern void * c_SGS_layering_ctl_block_name(void *f_elayer_ctl);
extern void * c_SGS_layering_ctl_iflag(void *f_elayer_ctl);
extern void * c_SGS_layering_grp_type_ctl(void *f_elayer_ctl);
extern void * c_SGS_layer_grp_name_ctl(void *f_elayer_ctl);
extern void * c_SGS_igrp_stack_layer_ctl(void *f_elayer_ctl);
extern void * c_SGS_num_layering_grp_ctl(void *f_elayer_ctl);
extern void * c_SGS_num_fluid_layer_grp_ctl(void *f_elayer_ctl);
extern void * c_SGS_start_layer_grp_name_ctl(void *f_elayer_ctl);
extern void * c_SGS_start_fluid_grp_name_ctl(void *f_elayer_ctl);
extern void * c_SGS_ngrp_on_sphere_ctl(void *f_elayer_ctl);

extern void * c_SGS_sph_filter_ctl_block_name(void *f_sph_filter_ctl);
extern void * c_SGS_sph_filter_ctl_iflag(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_sph_filter_type_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_r_filter_type_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_maximum_moments_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_sph_filter_width_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_r_filter_width_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_first_reference_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_second_reference_ctl(void *f_sph_filter_ctl);



struct f_MHD_SGS_SPH_filter_control * init_f_MHD_SGS_SPH_filter_control(int idx_in, void *f_parent)
{
	struct f_MHD_SGS_SPH_filter_control *f_sph_filter_ctl 
			= (struct f_MHD_SGS_SPH_filter_control *) malloc(sizeof(struct f_MHD_SGS_SPH_filter_control));
	if(f_sph_filter_ctl == NULL){
		printf("malloc error for f_sph_filter_ctl\n");
		exit(0);
	};
	
	f_sph_filter_ctl->f_self = c_SGS_model_sph_filter_ctl(idx_in, f_parent);
	
	f_sph_filter_ctl->f_iflag =  (int *)  c_SGS_sph_filter_ctl_iflag(f_sph_filter_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_sph_filter_ctl_block_name(f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_sph_filter_ctl->f_sph_filter_type_ctl =    init_f_ctl_chara_item(c_SPH_SGS_sph_filter_type_ctl,
																		 f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_radial_filter_type_ctl = init_f_ctl_chara_item(c_SPH_SGS_r_filter_type_ctl,
																		 f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_maximum_moments_ctl =    init_f_ctl_int_item(c_SPH_SGS_maximum_moments_ctl,
																	  f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_sphere_filter_width_ctl =  init_f_ctl_real_item(c_SPH_SGS_sph_filter_width_ctl,
																		f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_radial_filter_width_ctl =  init_f_ctl_real_item(c_SPH_SGS_r_filter_width_ctl,
																		f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_first_reference_ctl =   init_f_ctl_int_item(c_SPH_SGS_first_reference_ctl,
																	f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_second_reference_ctl =  init_f_ctl_int_item(c_SPH_SGS_second_reference_ctl,
																	f_sph_filter_ctl->f_self);
	return f_sph_filter_ctl;
};

void *dealloc_f_MHD_SGS_SPH_filter_control(struct f_MHD_SGS_SPH_filter_control *f_sph_filter_ctl)
{
    dealloc_int_ctl_item_c(f_sph_filter_ctl->f_maximum_moments_ctl);
    dealloc_int_ctl_item_c(f_sph_filter_ctl->f_first_reference_ctl);
    dealloc_int_ctl_item_c(f_sph_filter_ctl->f_second_reference_ctl);

    dealloc_real_ctl_item_c(f_sph_filter_ctl->f_sphere_filter_width_ctl);
    dealloc_real_ctl_item_c(f_sph_filter_ctl->f_radial_filter_width_ctl);

    dealloc_chara_ctl_item_c(f_sph_filter_ctl->f_radial_filter_width_ctl);
    dealloc_chara_ctl_item_c(f_sph_filter_ctl->f_radial_filter_type_ctl);

    free(f_sph_filter_ctl);
    return f_sph_filter_ctl;
}


struct f_MHD_SGS_3d_filter_control * init_f_MHD_SGS_3d_filter_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_3d_filter_control *f_s3df_ctl 
			= (struct f_MHD_SGS_3d_filter_control *) malloc(sizeof(struct f_MHD_SGS_3d_filter_control));
	if(f_s3df_ctl == NULL){
		printf("malloc error for f_s3df_ctl\n");
		exit(0);
	};
	
	f_s3df_ctl->f_self =  c_load_self(f_parent);
	
	f_s3df_ctl->f_iflag =  (int *)  c_SGS_3d_filter_ctl_iflag(f_s3df_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_3d_filter_ctl_block_name(f_s3df_ctl->f_self);
	f_s3df_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_s3df_ctl->f_whole_filter_grp_ctl =  init_f_ctl_chara_array(c_SGS_3d_whole_filter_grp_ctl,
																 f_s3df_ctl->f_self);
	f_s3df_ctl->f_fluid_filter_grp_ctl =  init_f_ctl_chara_array(c_SGS_3d_fluid_filter_grp_ctl,
																 f_s3df_ctl->f_self);
	f_s3df_ctl->f_momentum_filter_ctl =   init_f_ctl_chara_item(c_SGS_3d_momentum_filter_ctl,
																f_s3df_ctl->f_self);
	f_s3df_ctl->f_heat_filter_ctl =       init_f_ctl_chara_item(c_SGS_3d_heat_filter_ctl,
																f_s3df_ctl->f_self);
	f_s3df_ctl->f_induction_filter_ctl =  init_f_ctl_chara_item(c_SGS_3d_induction_filter_ctl,
																f_s3df_ctl->f_self);
	f_s3df_ctl->f_compostion_filter_ctl = init_f_ctl_chara_item(c_SGS_3d_comp_filter_ctl,
																f_s3df_ctl->f_self);
	return f_s3df_ctl;
}

struct f_MHD_SGS_layer_control * init_f_MHD_SGS_layer_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_layer_control *f_elayer_ctl 
			= (struct f_MHD_SGS_layer_control *) malloc(sizeof(struct f_MHD_SGS_layer_control));
	if(f_elayer_ctl == NULL){
		printf("malloc error for f_elayer_ctl\n");
		exit(0);
	};
	
	f_elayer_ctl->f_self =  c_load_self(f_parent);
	
	f_elayer_ctl->f_iflag =  (int *)  c_SGS_layering_ctl_iflag(f_elayer_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_layering_ctl_block_name(f_elayer_ctl->f_self);
	f_elayer_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_elayer_ctl->f_layering_grp_type_ctl =  init_f_ctl_chara_item(c_SGS_layering_grp_type_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_layer_grp_name_ctl =     init_f_ctl_chara_array(c_SGS_layer_grp_name_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_igrp_stack_layer_ctl =  init_f_ctl_int_array(c_SGS_igrp_stack_layer_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_num_layering_grp_ctl =  init_f_ctl_int_item(c_SGS_num_layering_grp_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_num_fl_layer_grp_ctl =  init_f_ctl_int_item(c_SGS_num_fluid_layer_grp_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_start_layering_grp_name_ctl =  init_f_ctl_chara_item(c_SGS_start_layer_grp_name_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_start_fl_layer_grp_name_ctl =  init_f_ctl_chara_item(c_SGS_start_fluid_grp_name_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_ngrp_SGS_on_sphere_ctl =  init_f_ctl_int_item(c_SGS_ngrp_on_sphere_ctl,
														   f_elayer_ctl->f_self);
	return f_elayer_ctl;
}

struct f_MHD_SGS_filter_file_control * init_f_MHD_SGS_filter_file_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_filter_file_control *f_ffile_ctl 
			= (struct f_MHD_SGS_filter_file_control *) malloc(sizeof(struct f_MHD_SGS_filter_file_control));
	if(f_ffile_ctl == NULL){
		printf("malloc error for f_ffile_ctl\n");
		exit(0);
	};
	
	f_ffile_ctl->f_self =  c_load_self(f_parent);
	
	f_ffile_ctl->f_iflag =  (int *)  c_SGS_filter_file_iflag(f_ffile_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_filter_file_block_name(f_ffile_ctl->f_self);
	f_ffile_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_ffile_ctl->f_filter_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_coef_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_coef_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_elen_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_elen_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_moms_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_moms_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_wide_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_wide_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_model_coef_ini_head_ctl =  init_f_ctl_chara_item(c_SGS_model_coef_ini_head,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_commute_coef_ini_head_ctl =  init_f_ctl_chara_item(c_SGS_commute_coef_ini_head,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_elen_format =  init_f_ctl_chara_item(c_SGS_filter_elen_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_3d_format =  init_f_ctl_chara_item(c_SGS_filter_3d_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_wide_format =  init_f_ctl_chara_item(c_SGS_filter_wide_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_model_coef_rst_format =  init_f_ctl_chara_item(c_SGS_model_coef_rst_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_commute_coef_rst_format =  init_f_ctl_chara_item(c_SGS_commute_coef_rst_format,
																 f_ffile_ctl->f_self);
	return f_ffile_ctl;
}

struct f_MHD_SGS_model_control * init_f_MHD_SGS_model_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_model_control *f_sgs_ctl 
			= (struct f_MHD_SGS_model_control *) malloc(sizeof(struct f_MHD_SGS_model_control));
	if(f_sgs_ctl == NULL){
		printf("malloc error for f_sgs_ctl\n");
		exit(0);
	};
	
	f_sgs_ctl->f_self =  c_load_self(f_parent);
	
	f_sgs_ctl->f_iflag =        (int *) c_SGS_model_ctl_iflag(f_sgs_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_model_ctl_block_name(f_sgs_ctl->f_self);
	f_sgs_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_sgs_ctl->f_SGS_model_name_ctl =    init_f_ctl_chara_item(c_SGS_model_SGS_model_name_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_filter_name_ctl =   init_f_ctl_chara_item(c_SGS_model_filter_name_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_DIFF_model_coef_ctl =   init_f_ctl_chara_item(c_SGS_model_DIFF_model_c_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_negative_clip_ctl = init_f_ctl_chara_item(c_SGS_model_negative_clip_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_marging_ctl =       init_f_ctl_chara_item(c_SGS_model_SGS_marging_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_perturbation_ctl =  init_f_ctl_chara_item(c_SGS_model_perturbation_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_model_coef_type_ctl =  init_f_ctl_chara_item(c_SGS_model_m_coef_type_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_heat_flux_csim_type_ctl =  init_f_ctl_chara_item(c_SGS_model_hflux_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_comp_flux_csim_type_ctl =  init_f_ctl_chara_item(c_SGS_model_cflux_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_mom_flux_csim_type_ctl =   init_f_ctl_chara_item(c_SGS_model_mflux_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_maxwell_csim_type_ctl =    init_f_ctl_chara_item(c_SGS_model_maxwell_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_uxb_csim_type_ctl =        init_f_ctl_chara_item(c_SGS_model_uxb_csim_type_ctl,
																  f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_model_coef_coord_ctl = init_f_ctl_chara_item(c_SGS_model_coef_coord_ctl, 
																  f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_buo_Csim_usage_ctl =   init_f_ctl_chara_item(c_SGS_model_SGS_buo_Csim_usage, 
																  f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_istep_dynamic_ctl =    init_f_ctl_int_item(c_SGS_model_istep_dynamic_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_min_step_dynamic_ctl = init_f_ctl_int_item(c_SGS_model_min_step_dynamic, f_sgs_ctl->f_self);
	f_sgs_ctl->f_max_step_dynamic_ctl = init_f_ctl_int_item(c_SGS_model_max_step_dynamic, f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_stabilize_weight_ctl =        init_f_ctl_real_item(c_SGS_model_stabilize_weight, 
																	f_sgs_ctl->f_self);
	f_sgs_ctl->f_delta_to_shrink_dynamic_ctl = init_f_ctl_real_item(c_SGS_model_del_shrink_dynamic, 
																	f_sgs_ctl->f_self);
	f_sgs_ctl->f_delta_to_extend_dynamic_ctl = init_f_ctl_real_item(c_SGS_model_del_extend_dynamic, 
																	f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_ngrp_radial_ave_ctl = init_f_ctl_int_item(c_SGS_model_ngrp_radial_ave, f_sgs_ctl->f_self);
	f_sgs_ctl->f_ngrp_med_ave_ctl =    init_f_ctl_int_item(c_SGS_model_ngrp_med_ave_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_clipping_limit_ctl =  init_f_ctl_real_item(c_SGS_model_clipping_limit_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_hf_factor_ctl =   init_f_ctl_real_item(c_SGS_model_SGS_hf_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_cf_factor_ctl =   init_f_ctl_real_item(c_SGS_model_SGS_cf_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_mf_factor_ctl =   init_f_ctl_real_item(c_SGS_model_SGS_mf_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_mxwl_factor_ctl = init_f_ctl_real_item(c_SGS_model_mxwl_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_uxb_factor_ctl =  init_f_ctl_real_item(c_SGS_model_SGS_uxb_factor_ctl, f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_SGS_terms_ctl =       init_f_ctl_chara_array(c_SGS_model_SGS_terms_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_commutate_fld_ctl =   init_f_ctl_chara_array(c_SGS_model_commutate_fld_ctl, f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_ffile_ctl = init_f_MHD_SGS_filter_file_control(c_SGS_model_ffile_ctl, 
																f_sgs_ctl->f_self);
	f_sgs_ctl->f_elayer_ctl = init_f_MHD_SGS_layer_control(c_SGS_model_elayer_ctl, 
														   f_sgs_ctl->f_self);
	f_sgs_ctl->f_s3df_ctl = init_f_MHD_SGS_3d_filter_control(c_SGS_model_s3df_ctl,
															 f_sgs_ctl->f_self);
	
	
	f_block_name =   (char *) c_SGS_model_sph_filter_name(f_sgs_ctl->f_self);
 	f_sgs_ctl->f_sph_filter_ctl = init_void_clist(strngcopy_from_f(f_block_name));
	f_sgs_ctl->f_sph_filter_ctl->f_parent =  f_sgs_ctl->f_self;
	f_sgs_ctl->f_num_sph_filter_ctl = c_SGS_model_num_sph_filter_ctl(f_sgs_ctl->f_self);
	int i;
	for(i=0;i<f_sgs_ctl->f_num_sph_filter_ctl;i++){
		struct f_MHD_SGS_SPH_filter_control *void_in 
				= init_f_MHD_SGS_SPH_filter_control(i, f_sgs_ctl->f_self);
		append_void_clist((void *) void_in, f_sgs_ctl->f_sph_filter_ctl);
	}
	return f_sgs_ctl;
};
