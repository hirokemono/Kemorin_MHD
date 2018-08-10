/*
//  t_ctl_data_SGS_model_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/07/30.
*/

#ifndef t_ctl_data_SGS_model_c_h_
#define t_ctl_data_SGS_model_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_ctl_data_sph_filter_list.h"

struct filter_file_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *filter_head_c;
	struct chara_ctl_item *filter_coef_head_c;
	struct chara_ctl_item *filter_elen_head_c;
	struct chara_ctl_item *filter_moms_head_c;
	
	struct chara_ctl_item *filter_wide_head_c;
	struct chara_ctl_item *model_coef_ini_head_c;
	struct chara_ctl_item *commute_coef_ini_head_c;
	
	struct chara_ctl_item *filter_elen_format_c;
	struct chara_ctl_item *filter_3d_format_c;
	struct chara_ctl_item *filter_wide_format_c;
	
	struct chara_ctl_item *model_coef_rst_format_c;
	struct chara_ctl_item *commute_coef_rst_format_c;
};

struct layering_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *layering_grp_type_c;
	struct chara_ctl_array *layer_grp_name_c;
	struct int_ctl_array *igrp_stack_layer_c;
	
	struct int_ctl_item *num_layering_grp_c;
	struct int_ctl_item *num_fl_layer_grp_c;
	struct chara_ctl_item *start_layering_grp_name_c;
	struct chara_ctl_item *start_fl_layer_grp_name_c;
	
	struct int_ctl_item *ngrp_SGS_on_sphere_c;
};

struct SGS_3d_filter_ctl_c{
	int maxlen;
	
	struct chara_ctl_array *whole_filter_grp_ctl;
	struct chara_ctl_array *fluid_filter_grp_ctl;
	
	struct chara_ctl_item *momentum_filter_ctl;
	struct chara_ctl_item *heat_filter_ctl;
	struct chara_ctl_item *induction_filter_ctl;
	struct chara_ctl_item *compostion_filter_ctl;
};

struct SGS_model_control_c{
	int maxlen;
	
	struct chara_ctl_item *SGS_model_name_c;
	struct chara_ctl_item *SGS_filter_name_c;
	struct chara_ctl_item *DIFF_model_coef_c;
	struct chara_ctl_item *SGS_negative_clip_c;
	struct chara_ctl_item *SGS_marging_c;
	struct chara_ctl_item *SGS_perturbation_c;
	
	struct chara_ctl_item *SGS_model_coef_type_c;
	
	struct chara_ctl_item *heat_flux_csim_type_c;
	struct chara_ctl_item *comp_flux_csim_type_c;
	struct chara_ctl_item *mom_flux_csim_type_c;
	struct chara_ctl_item *maxwell_csim_type_c;
	struct chara_ctl_item *uxb_csim_type_c;
	
	struct chara_ctl_item *SGS_model_coef_coord_c;
	struct chara_ctl_item *SGS_buo_Csim_usage_c;
	
	struct int_ctl_item *istep_dynamic_c;
	struct int_ctl_item *min_step_dynamic_c;
	struct int_ctl_item *max_step_dynamic_c;
	struct real_ctl_item *stabilize_weight_c;
	struct real_ctl_item *delta_to_shrink_dynamic_c;
	struct real_ctl_item *delta_to_extend_dynamic_c;
	
	struct int_ctl_item *ngrp_radial_ave_c;
	struct int_ctl_item *ngrp_med_ave_c;
	
	struct real_ctl_item *clipping_limit_c;
	
	struct real_ctl_item *SGS_hf_factor_c;
	struct real_ctl_item *SGS_cf_factor_c;
	struct real_ctl_item *SGS_mf_factor_c;
	struct real_ctl_item *SGS_mxwl_factor_c;
	struct real_ctl_item *SGS_uxb_factor_c;
	
	struct chara_ctl_array *SGS_terms_c;
	struct chara_ctl_array *commutate_fld_c;
	
	int iflag_file_ctl;
	struct filter_file_ctl_c *ffile_c;
	
	int iflag_layer_ctl;
	struct layering_ctl_c *elayer_c;
	
	int iflag_3dfilter_ctl;
	struct SGS_3d_filter_ctl_c *s3df_c;
	
	struct sph_filter_ctl_list sph_filter_list;
};


/* prototype */
void get_label_filter_file_ctl(int index, char *label);
void get_label_layering_ctl(int index, char *label);
void get_label_SGS_3d_filter_ctl(int index, char *label);
void get_label_SGS_model_ctl(int index, char *label);

void alloc_filter_file_ctl_c(struct filter_file_ctl_c *ffile_c);
void dealloc_filter_file_ctl_c(struct filter_file_ctl_c *ffile_c);
int read_filter_file_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct filter_file_ctl_c *ffile_c);
int write_filter_file_ctl_c(FILE *fp, int level, const char *label, 
                            struct filter_file_ctl_c *ffile_c);

void alloc_layering_ctl_c(struct layering_ctl_c *elayer_c);
void dealloc_layering_ctl_c(struct layering_ctl_c *elayer_c);
int read_layering_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct layering_ctl_c *elayer_c);
int write_layering_ctl_c(FILE *fp, int level, const char *label, 
                         struct layering_ctl_c *elayer_c);

void alloc_SGS_3d_filter_ctl_c(struct SGS_3d_filter_ctl_c *s3df_c);
void dealloc_SGS_3d_filter_ctl_c(struct SGS_3d_filter_ctl_c *s3df_c);
int read_SGS_3d_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct SGS_3d_filter_ctl_c *s3df_c);
int write_SGS_3d_filter_ctl_c(FILE *fp, int level, const char *label, 
                              struct SGS_3d_filter_ctl_c *s3df_c);

void alloc_SGS_model_ctl_c(struct SGS_model_control_c *SGS_ctl_c);
void dealloc_SGS_model_ctl_c(struct SGS_model_control_c *SGS_ctl_c);
int read_SGS_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct SGS_model_control_c *SGS_ctl_c);
int write_SGS_model_ctl_c(FILE *fp, int level, const char *label,
                          struct SGS_model_control_c *SGS_ctl_c);

#endif /* t_ctl_data_SGS_model_c_h */
