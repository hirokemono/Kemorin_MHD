/*
//  t_ctl_data_SGS_model_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/07/30.
*/

#include "t_ctl_data_SGS_model_c.h"

#define NLBL_FILTGER_FILE_CTL  12
#define NLBL_LAYERING_CTL       8
#define NLBL_SPH_FILTER_CTL     7
#define NLBL_SGS_3D_FILTER_CTL  6
#define NLBL_SGS_MODEL_CTL     34

const char label_filter_file_ctl[NLBL_FILTGER_FILE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"filter_file_prefix"},
	/*[ 1]*/	{"filter_elength_prefix"},
	/*[ 2]*/	{"filter_moment_prefix"},
	/*[ 3]*/	{"filter_coefs_prefix"},
	/*[ 4]*/	{"wider_filter_prefix"},
	/*[ 5]*/	{"model_coef_rst_prefix"},
	/*[ 6]*/	{"commutel_coef_rst_prefix"},
	
	/*[ 7]*/	{"filter_elen_format"},
	/*[ 8]*/	{"filter_3d_format"},
	/*[ 9]*/	{"filter_wide_format"},
	/*[10]*/	{"model_coef_rst_format"},
	/*[11]*/	{"commute_coef_rst_format"}
};

const char label_layering_ctl[NLBL_LAYERING_CTL][KCHARA_C] = {
	/*[ 0]*/	{"layering_data_ctl"},
	/*[ 1]*/	{"layer_grp_name_ctl"},
	/*[ 2]*/	{"grp_stack_each_layer_ctl"},
	
	/*[ 3]*/	{"num_layering_grp_ctl"},
	/*[ 4]*/	{"start_layering_grp_name_ctl"},
	/*[ 5]*/	{"num_fl_layer_grp_ctl"},
	/*[ 6]*/	{"start_fl_layer_grp_name_ctl"},
	
	/*[ 7]*/	{"ngrp_SGS_on_sphere_ctl"}
};

const char label_sph_filter_ctl[NLBL_SPH_FILTER_CTL][KCHARA_C] = {
	/*[ 0]*/	{"sph_filter_type"},
	/*[ 1]*/	{"radial_filter_type"},
	/*[ 2]*/	{"number_of_moments"},
	/*[ 3]*/	{"radial_filter_width"},
	/*[ 4]*/	{"sphere_filter_width"},
	/*[ 5]*/	{"first_reference_filter_ID"},
	/*[ 6]*/	{"second_reference_filter_ID"}
};

const char label_SGS_3d_filter_ctl[NLBL_SGS_3D_FILTER_CTL][KCHARA_C] = {
	/*[ 0]*/	{"whole_filtering_grp_ctl"},
	/*[ 1]*/	{"fluid_filtering_grp_ctl"},
	
	/*[ 2]*/	{"momentum_filter_ctl"},
	/*[ 3]*/	{"heat_filter_ctl"},
	/*[ 4]*/	{"induction_filter_ctl"},
	/*[ 5]*/	{"composition_filter_ctl"}
};

const char label_SGS_model_ctl[NLBL_SGS_MODEL_CTL][KCHARA_C] = {
	/*[ 0]*/	{"SGS_model_ctl"},
	/*[ 1]*/	{"filtering_scheme_ctl"},
	/*[ 2]*/	{"negative_clip_ctl"},
	/*[ 3]*/	{"clipping_limit_ctl"},
	
	/*[ 4]*/	{"SGS_hf_factor_ctl"},
	/*[ 5]*/	{"SGS_cf_factor_ctl"},
	/*[ 6]*/	{"SGS_mf_factor_ctl"},
	/*[ 7]*/	{"SGS_mxwl_factor_ctl"},
	/*[ 8]*/	{"SGS_uxb_factor_ctl"},
	
	/*[ 9]*/	{"direction_marging_ctl"},
	/*[10]*/	{"diff_coef_mode_ctl"},
	/*[11]*/	{"3d_filtering_ctl"},
	/*[12]*/	{"istep_dynamic_ctl"},
	/*[13]*/	{"stabilize_weight_ctl"},
	/*[14]*/	{"min_step_dynamic_ctl"},
	/*[15]*/	{"max_step_dynamic_ctl"},
	/*[16]*/	{"delta_to_shrink_ctl"},
	/*[17]*/	{"delta_to_extend_ctl"},
	/*[18]*/	{"SGS_perturbation_ctl"},
	
	/*[19]*/	{"num_radial_averaging_area"},
	/*[20]*/	{"num_med_averaging_area"},
	
	/*[21]*/	{"model_coef_type_ctl"},
	/*[22]*/	{"heat_flux_csim_type_ctl"},
	/*[23]*/	{"comp_flux_csim_type_ctl"},
	/*[24]*/	{"mom_flux_csim_type_ctl"},
	/*[25]*/	{"maxwell_csim_type_ctl"},
	/*[26]*/	{"uxb_csim_type_ctl"},
	
	/*[27]*/	{"model_coef_coordinate_ctl"},
	/*[28]*/	{"SGS_buoyancy_Csim_usage"},
	
	/*[29]*/	{"SGS_terms_ctl"},
	/*[30]*/	{"commutation_ctl"},
	/*[31]*/	{"filter_files_def"},
	/*[32]*/	{"dynamic_model_layer_ctl"},
	/*[33]*/	{"sph_filter_ctl"}
};

void alloc_filter_file_ctl_c(struct filter_file_ctl_c *ffile_c){
	int i;
	
	ffile_c->maxlen = 0;
	for (i=0;i<NLBL_FILTGER_FILE_CTL;i++){
		if(strlen(label_filter_file_ctl[i]) > ffile_c->maxlen){
			ffile_c->maxlen = strlen(label_filter_file_ctl[i]);
		};
	};
	
	
	ffile_c->filter_head_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->filter_coef_head_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->filter_elen_head_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->filter_moms_head_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	ffile_c->filter_wide_head_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->model_coef_ini_head_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->commute_coef_ini_head_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	ffile_c->filter_elen_format_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->filter_3d_format_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->filter_wide_format_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	ffile_c->model_coef_rst_format_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	ffile_c->commute_coef_rst_format_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_ctl_chara_item(ffile_c->filter_head_c);
	alloc_ctl_chara_item(ffile_c->filter_coef_head_c);
	alloc_ctl_chara_item(ffile_c->filter_elen_head_c);
	alloc_ctl_chara_item(ffile_c->filter_moms_head_c);
	
	alloc_ctl_chara_item(ffile_c->filter_wide_head_c);
	alloc_ctl_chara_item(ffile_c->model_coef_ini_head_c);
	alloc_ctl_chara_item(ffile_c->commute_coef_ini_head_c);
	
	alloc_ctl_chara_item(ffile_c->filter_elen_format_c);
	alloc_ctl_chara_item(ffile_c->filter_3d_format_c);
	alloc_ctl_chara_item(ffile_c->filter_wide_format_c);
	
	alloc_ctl_chara_item(ffile_c->model_coef_rst_format_c);
	alloc_ctl_chara_item(ffile_c->commute_coef_rst_format_c);
	
	return;
};

void dealloc_filter_file_ctl_c(struct filter_file_ctl_c *ffile_c){
	
	dealloc_ctl_chara_item(ffile_c->filter_head_c);
	dealloc_ctl_chara_item(ffile_c->filter_coef_head_c);
	dealloc_ctl_chara_item(ffile_c->filter_elen_head_c);
	dealloc_ctl_chara_item(ffile_c->filter_moms_head_c);
	
	dealloc_ctl_chara_item(ffile_c->filter_wide_head_c);
	dealloc_ctl_chara_item(ffile_c->model_coef_ini_head_c);
	dealloc_ctl_chara_item(ffile_c->commute_coef_ini_head_c);
	
	dealloc_ctl_chara_item(ffile_c->filter_elen_format_c);
	dealloc_ctl_chara_item(ffile_c->filter_3d_format_c);
	dealloc_ctl_chara_item(ffile_c->filter_wide_format_c);
	
	dealloc_ctl_chara_item(ffile_c->model_coef_rst_format_c);
	dealloc_ctl_chara_item(ffile_c->commute_coef_rst_format_c);
	
	free(ffile_c->filter_head_c);
	free(ffile_c->filter_coef_head_c);
	free(ffile_c->filter_elen_head_c);
	free(ffile_c->filter_moms_head_c);
	
	free(ffile_c->filter_wide_head_c);
	free(ffile_c->model_coef_ini_head_c);
	free(ffile_c->commute_coef_ini_head_c);
	
	free(ffile_c->filter_elen_format_c);
	free(ffile_c->filter_3d_format_c);
	free(ffile_c->filter_wide_format_c);
	
	free(ffile_c->model_coef_rst_format_c);
	free(ffile_c->commute_coef_rst_format_c);
	
	return;
};

int read_filter_file_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct filter_file_ctl_c *ffile_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 0], ffile_c->filter_head_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 1], ffile_c->filter_elen_head_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 2], ffile_c->filter_moms_head_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 3], ffile_c->filter_coef_head_c);
		
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 4], ffile_c->filter_wide_head_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 5], ffile_c->model_coef_ini_head_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 6], ffile_c->commute_coef_ini_head_c);
		
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 7], ffile_c->filter_elen_format_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 8], ffile_c->filter_3d_format_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[ 9], ffile_c->filter_wide_format_c);
		
		read_character_ctl_item_c(buf, label_filter_file_ctl[10], ffile_c->model_coef_rst_format_c);
		read_character_ctl_item_c(buf, label_filter_file_ctl[11], ffile_c->commute_coef_rst_format_c);
	};
	return 1;
};

int write_filter_file_ctl_c(FILE *fp, int level, const char *label, 
                            struct filter_file_ctl_c *ffile_c){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 0], ffile_c->filter_head_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 1], ffile_c->filter_elen_head_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 2], ffile_c->filter_moms_head_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 3], ffile_c->filter_coef_head_c);
	
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 4], ffile_c->filter_wide_head_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 5], ffile_c->model_coef_ini_head_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 6], ffile_c->commute_coef_ini_head_c);
	
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 7], ffile_c->filter_elen_format_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 8], ffile_c->filter_3d_format_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[ 9], ffile_c->filter_wide_format_c);
	
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[10], ffile_c->model_coef_rst_format_c);
	write_character_ctl_item_c(fp, level, ffile_c->maxlen, label_filter_file_ctl[11], ffile_c->commute_coef_rst_format_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_layering_ctl_c(struct layering_ctl_c *elayer_c){
	int i;
	
	elayer_c->maxlen = 0;
	for (i=0;i<NLBL_LAYERING_CTL;i++){
		if(strlen(label_layering_ctl[i]) > elayer_c->maxlen){
			elayer_c->maxlen = strlen(label_layering_ctl[i]);
		};
	};
	
	elayer_c->layering_grp_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	elayer_c->start_layering_grp_name_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	elayer_c->start_fl_layer_grp_name_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_ctl_chara_item(elayer_c->layering_grp_type_c);
	alloc_ctl_chara_item(elayer_c->start_layering_grp_name_c);
	alloc_ctl_chara_item(elayer_c->start_fl_layer_grp_name_c);
	
	elayer_c->num_layering_grp_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	elayer_c->num_fl_layer_grp_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	elayer_c->ngrp_SGS_on_sphere_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	
	elayer_c->layer_grp_name_c = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	elayer_c->igrp_stack_layer_c = (struct int_ctl_array *) malloc(sizeof(struct int_ctl_array));
	init_ctl_chara_array(elayer_c->layer_grp_name_c);
	init_ctl_int_array(elayer_c->igrp_stack_layer_c);
	
	init_ctl_int_item(elayer_c->num_layering_grp_c);
	init_ctl_int_item(elayer_c->num_fl_layer_grp_c);
	init_ctl_int_item(elayer_c->ngrp_SGS_on_sphere_c);
	
	return;
};

void dealloc_layering_ctl_c(struct layering_ctl_c *elayer_c){
	
	dealloc_ctl_chara_item(elayer_c->layering_grp_type_c);
	dealloc_ctl_chara_item(elayer_c->start_layering_grp_name_c);
	dealloc_ctl_chara_item(elayer_c->start_fl_layer_grp_name_c);
	
	dealloc_ctl_chara_array(elayer_c->layer_grp_name_c);
	free(elayer_c->layer_grp_name_c);
	
	dealloc_ctl_int_array(elayer_c->igrp_stack_layer_c);
	free(elayer_c->igrp_stack_layer_c);
	
	free(elayer_c->layering_grp_type_c);
	free(elayer_c->start_layering_grp_name_c);
	free(elayer_c->start_fl_layer_grp_name_c);
	
	free(elayer_c->num_layering_grp_c);
	free(elayer_c->num_fl_layer_grp_c);
	free(elayer_c->ngrp_SGS_on_sphere_c);
	
	return;
};

int read_layering_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct layering_ctl_c *elayer_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_character_ctl_item_c(buf, label_layering_ctl[ 0], elayer_c->layering_grp_type_c);
		
		read_character_ctl_array_c(fp, buf, label_layering_ctl[1], elayer_c->layer_grp_name_c);
		read_integer_ctl_array_c(fp, buf, label_layering_ctl[2], elayer_c->igrp_stack_layer_c);
		
		read_character_ctl_item_c(buf, label_layering_ctl[ 4], elayer_c->start_layering_grp_name_c);
		read_character_ctl_item_c(buf, label_layering_ctl[ 6], elayer_c->start_fl_layer_grp_name_c);
		
		read_integer_ctl_item_c(buf, label_layering_ctl[ 3], elayer_c->num_layering_grp_c);
		read_integer_ctl_item_c(buf, label_layering_ctl[ 5], elayer_c->num_fl_layer_grp_c);
		read_integer_ctl_item_c(buf, label_layering_ctl[ 7], elayer_c->ngrp_SGS_on_sphere_c);
	};
	return 1;
};

int write_layering_ctl_c(FILE *fp, int level, const char *label, struct layering_ctl_c *elayer_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, elayer_c->maxlen, label_layering_ctl[ 0], elayer_c->layering_grp_type_c);
	
	if(elayer_c->layer_grp_name_c->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, strlen(label_layering_ctl[1]),
				label_layering_ctl[1], elayer_c->layer_grp_name_c);
	
	if(elayer_c->igrp_stack_layer_c->num > 0) fprintf(fp, "!\n");
	write_integer_ctl_array_c(fp, level, strlen(label_layering_ctl[2]),
				label_layering_ctl[2], elayer_c->igrp_stack_layer_c);
	
	write_integer_ctl_item_c(fp, level, elayer_c->maxlen, label_layering_ctl[ 3], elayer_c->num_layering_grp_c);
	write_character_ctl_item_c(fp, level, elayer_c->maxlen, label_layering_ctl[ 4], elayer_c->start_layering_grp_name_c);
	write_integer_ctl_item_c(fp, level, elayer_c->maxlen, label_layering_ctl[ 5], elayer_c->num_fl_layer_grp_c);
	write_character_ctl_item_c(fp, level, elayer_c->maxlen, label_layering_ctl[ 6], elayer_c->start_fl_layer_grp_name_c);
	
	write_integer_ctl_item_c(fp, level, elayer_c->maxlen, label_layering_ctl[ 7], elayer_c->ngrp_SGS_on_sphere_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_each_sph_filter_ctl_c(struct sph_filter_ctl_c *sph_filter_c){
	int i;
	
	sph_filter_c->maxlen = 0;
	for (i=0;i<NLBL_SPH_FILTER_CTL;i++){
		if(strlen(label_sph_filter_ctl[i]) > sph_filter_c->maxlen){
			sph_filter_c->maxlen = strlen(label_sph_filter_ctl[i]);
		};
	};
	
	sph_filter_c->sph_filter_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	sph_filter_c->radial_filter_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_ctl_chara_item(sph_filter_c->sph_filter_type_c);
	alloc_ctl_chara_item(sph_filter_c->radial_filter_type_c);
	
	sph_filter_c->maximum_moments_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	sph_filter_c->first_reference_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	sph_filter_c->second_reference_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	
	init_ctl_int_item(sph_filter_c->maximum_moments_c);
	init_ctl_int_item(sph_filter_c->first_reference_c);
	init_ctl_int_item(sph_filter_c->second_reference_c);
	
	sph_filter_c->sphere_filter_width_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	sph_filter_c->radial_filter_width_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	
	init_ctl_real_item(sph_filter_c->sphere_filter_width_c);
	init_ctl_real_item(sph_filter_c->radial_filter_width_c);
	
	return;
};

void dealloc_each_sph_filter_ctl_c(struct sph_filter_ctl_c *sph_filter_c){
	
	dealloc_ctl_chara_item(sph_filter_c->sph_filter_type_c);
	dealloc_ctl_chara_item(sph_filter_c->radial_filter_type_c);
	
	free(sph_filter_c->sph_filter_type_c);
	free(sph_filter_c->radial_filter_type_c);
	
	free(sph_filter_c->maximum_moments_c);
	free(sph_filter_c->first_reference_c);
	free(sph_filter_c->second_reference_c);
	
	free(sph_filter_c->sphere_filter_width_c);
	free(sph_filter_c->radial_filter_width_c);
	
	return;
};

int read_each_sph_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sph_filter_ctl_c *sph_filter_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_character_ctl_item_c(buf, label_sph_filter_ctl[ 0], sph_filter_c->sph_filter_type_c);
		read_character_ctl_item_c(buf, label_sph_filter_ctl[ 1], sph_filter_c->radial_filter_type_c);
		
		read_integer_ctl_item_c(buf, label_sph_filter_ctl[ 2], sph_filter_c->maximum_moments_c);
		read_integer_ctl_item_c(buf, label_sph_filter_ctl[ 5], sph_filter_c->first_reference_c);
		read_integer_ctl_item_c(buf, label_sph_filter_ctl[ 6], sph_filter_c->second_reference_c);
		
		read_real_ctl_item_c(buf, label_sph_filter_ctl[ 3], sph_filter_c->sphere_filter_width_c);
		read_real_ctl_item_c(buf, label_sph_filter_ctl[ 4], sph_filter_c->radial_filter_width_c);
	};
	return 1;
};

int write_each_sph_filter_ctl_c(FILE *fp, int level, const char *label, 
                                struct sph_filter_ctl_c *sph_filter_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 0], sph_filter_c->sph_filter_type_c);
	write_character_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 1], sph_filter_c->radial_filter_type_c);
	
	write_integer_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 2], sph_filter_c->maximum_moments_c);
	write_real_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 3], sph_filter_c->sphere_filter_width_c);
	write_real_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 4], sph_filter_c->radial_filter_width_c);
	
	write_integer_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 5], sph_filter_c->first_reference_c);
	write_integer_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 6], sph_filter_c->second_reference_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_SGS_3d_filter_ctl_c(struct SGS_3d_filter_ctl_c *s3df_c){
	int i;
	
	s3df_c->maxlen = 0;
	for (i=0;i<NLBL_SGS_3D_FILTER_CTL;i++){
		if(strlen(label_SGS_3d_filter_ctl[i]) > s3df_c->maxlen){
			s3df_c->maxlen = strlen(label_SGS_3d_filter_ctl[i]);
		};
	};
	
	s3df_c->whole_filter_grp_ctl = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	s3df_c->fluid_filter_grp_ctl = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	init_ctl_chara_array(s3df_c->whole_filter_grp_ctl);
	init_ctl_chara_array(s3df_c->fluid_filter_grp_ctl);
	
	s3df_c->momentum_filter_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	s3df_c->heat_filter_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	s3df_c->induction_filter_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	s3df_c->compostion_filter_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(s3df_c->momentum_filter_ctl);
	alloc_ctl_chara_item(s3df_c->heat_filter_ctl);
	alloc_ctl_chara_item(s3df_c->induction_filter_ctl);
	alloc_ctl_chara_item(s3df_c->compostion_filter_ctl);
	
	return;
};

void dealloc_SGS_3d_filter_ctl_c(struct SGS_3d_filter_ctl_c *s3df_c){
	
	dealloc_ctl_chara_array(s3df_c->whole_filter_grp_ctl);
	dealloc_ctl_chara_array(s3df_c->fluid_filter_grp_ctl);
	free(s3df_c->whole_filter_grp_ctl);
	free(s3df_c->fluid_filter_grp_ctl);
	
	dealloc_ctl_chara_item(s3df_c->momentum_filter_ctl);
	dealloc_ctl_chara_item(s3df_c->heat_filter_ctl);
	dealloc_ctl_chara_item(s3df_c->induction_filter_ctl);
	dealloc_ctl_chara_item(s3df_c->compostion_filter_ctl);
	
	free(s3df_c->momentum_filter_ctl);
	free(s3df_c->heat_filter_ctl);
	free(s3df_c->induction_filter_ctl);
	free(s3df_c->compostion_filter_ctl);
	
	return;
};

int read_SGS_3d_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct SGS_3d_filter_ctl_c *s3df_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_character_ctl_array_c(fp, buf, label_SGS_3d_filter_ctl[ 0], s3df_c->whole_filter_grp_ctl);
		read_character_ctl_array_c(fp, buf, label_SGS_3d_filter_ctl[ 1], s3df_c->fluid_filter_grp_ctl);
		
		read_character_ctl_item_c(buf, label_SGS_3d_filter_ctl[ 2], s3df_c->momentum_filter_ctl);
		read_character_ctl_item_c(buf, label_SGS_3d_filter_ctl[ 3], s3df_c->heat_filter_ctl);
		read_character_ctl_item_c(buf, label_SGS_3d_filter_ctl[ 4], s3df_c->induction_filter_ctl);
		read_character_ctl_item_c(buf, label_SGS_3d_filter_ctl[ 5], s3df_c->compostion_filter_ctl);
	};
	return 1;
};

int write_SGS_3d_filter_ctl_c(FILE *fp, int level, const char *label,
                              struct SGS_3d_filter_ctl_c *s3df_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	if(s3df_c->whole_filter_grp_ctl->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, strlen(label_SGS_3d_filter_ctl[0]),
				label_SGS_3d_filter_ctl[0], s3df_c->whole_filter_grp_ctl);
	
	if(s3df_c->fluid_filter_grp_ctl->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, strlen(label_SGS_3d_filter_ctl[1]),
				label_SGS_3d_filter_ctl[1], s3df_c->fluid_filter_grp_ctl);
	
	write_character_ctl_item_c(fp, level, s3df_c->maxlen, label_SGS_3d_filter_ctl[ 2], s3df_c->momentum_filter_ctl);
	write_character_ctl_item_c(fp, level, s3df_c->maxlen, label_SGS_3d_filter_ctl[ 3], s3df_c->heat_filter_ctl);
	write_character_ctl_item_c(fp, level, s3df_c->maxlen, label_SGS_3d_filter_ctl[ 4], s3df_c->induction_filter_ctl);
	write_character_ctl_item_c(fp, level, s3df_c->maxlen, label_SGS_3d_filter_ctl[ 5], s3df_c->compostion_filter_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_sph_filter_ctl_c(struct SGS_model_control_c *SGS_ctl_c){
	int i, num;
	num = SGS_ctl_c->num_sph_filter_c;
	SGS_ctl_c->sph_filter_c = (struct sph_filter_ctl_c **) malloc(num*sizeof(struct sph_filter_ctl_c *));
	
	for(i=0;i<num;i++){
		SGS_ctl_c->sph_filter_c[i] = (struct sph_filter_ctl_c *) malloc(sizeof(struct sph_filter_ctl_c));
		alloc_each_sph_filter_ctl_c(SGS_ctl_c->sph_filter_c[i]);
	};
	return;
};
void dealloc_sph_filter_ctl_c(struct SGS_model_control_c *SGS_ctl_c){
	int i;
	for(i=0;i<SGS_ctl_c->num_sph_filter_c;i++){
		dealloc_each_sph_filter_ctl_c(SGS_ctl_c->sph_filter_c[i]);
		free(SGS_ctl_c->sph_filter_c[i]);
	};
	free(SGS_ctl_c->sph_filter_c);
	return;
};
int read_sph_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct SGS_model_control_c *SGS_ctl_c){
	int iflag = 0;
	int icou = 0;
	
	alloc_sph_filter_ctl_c(SGS_ctl_c);
	while(find_control_end_array_flag_c(buf, label, SGS_ctl_c->num_sph_filter_c, icou) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label) > 0){
			iflag = read_each_sph_filter_ctl_c(fp, buf, label, SGS_ctl_c->sph_filter_c[icou]);
			icou = icou + iflag;
		};
	};
	return 1;
}
int write_sph_filter_ctl_c(FILE *fp, int level, const char *label, 
			struct SGS_model_control_c *SGS_ctl_c){
	int i;
	
	if(SGS_ctl_c->num_sph_filter_c == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, SGS_ctl_c->num_sph_filter_c);
	for(i=0;i<SGS_ctl_c->num_sph_filter_c;i++){
		write_each_sph_filter_ctl_c(fp, level, label, SGS_ctl_c->sph_filter_c[i]);
		fprintf(fp, "!\n");
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_SGS_model_ctl_c(struct SGS_model_control_c *SGS_ctl_c){
	int i;
	
	SGS_ctl_c->maxlen = 0;
	for (i=0;i<NLBL_SGS_MODEL_CTL;i++){
		if(strlen(label_SGS_model_ctl[i]) > SGS_ctl_c->maxlen){
			SGS_ctl_c->maxlen = strlen(label_SGS_model_ctl[i]);
		};
	};
	
	SGS_ctl_c->ffile_c = (struct filter_file_ctl_c *) malloc(sizeof(struct filter_file_ctl_c));
	alloc_filter_file_ctl_c(SGS_ctl_c->ffile_c);
	
	SGS_ctl_c->elayer_c = (struct layering_ctl_c *) malloc(sizeof(struct layering_ctl_c));
	alloc_layering_ctl_c(SGS_ctl_c->elayer_c);
	
	SGS_ctl_c->s3df_c = (struct SGS_3d_filter_ctl_c *) malloc(sizeof(struct SGS_3d_filter_ctl_c));
	alloc_SGS_3d_filter_ctl_c(SGS_ctl_c->s3df_c);
	
	SGS_ctl_c->SGS_terms_c = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	SGS_ctl_c->commutate_fld_c = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	init_ctl_chara_array(SGS_ctl_c->SGS_terms_c);
	init_ctl_chara_array(SGS_ctl_c->commutate_fld_c);
	
	SGS_ctl_c->SGS_model_name_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->SGS_filter_name_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->DIFF_model_coef_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->SGS_negative_clip_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->SGS_marging_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->SGS_perturbation_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->SGS_model_coef_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->heat_flux_csim_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->comp_flux_csim_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->mom_flux_csim_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->maxwell_csim_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->uxb_csim_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->SGS_model_coef_coord_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	SGS_ctl_c->SGS_buo_Csim_usage_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_ctl_chara_item(SGS_ctl_c->SGS_model_name_c);
	alloc_ctl_chara_item(SGS_ctl_c->SGS_filter_name_c);
	alloc_ctl_chara_item(SGS_ctl_c->DIFF_model_coef_c);
	alloc_ctl_chara_item(SGS_ctl_c->SGS_negative_clip_c);
	alloc_ctl_chara_item(SGS_ctl_c->SGS_marging_c);
	alloc_ctl_chara_item(SGS_ctl_c->SGS_perturbation_c);
	alloc_ctl_chara_item(SGS_ctl_c->SGS_model_coef_type_c);
	alloc_ctl_chara_item(SGS_ctl_c->heat_flux_csim_type_c);
	alloc_ctl_chara_item(SGS_ctl_c->comp_flux_csim_type_c);
	alloc_ctl_chara_item(SGS_ctl_c->mom_flux_csim_type_c);
	alloc_ctl_chara_item(SGS_ctl_c->maxwell_csim_type_c);
	alloc_ctl_chara_item(SGS_ctl_c->uxb_csim_type_c);
	alloc_ctl_chara_item(SGS_ctl_c->SGS_model_coef_coord_c);
	alloc_ctl_chara_item(SGS_ctl_c->SGS_buo_Csim_usage_c);
	
	SGS_ctl_c->istep_dynamic_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	SGS_ctl_c->min_step_dynamic_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	SGS_ctl_c->max_step_dynamic_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	SGS_ctl_c->ngrp_radial_ave_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	SGS_ctl_c->ngrp_med_ave_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	
	init_ctl_int_item(SGS_ctl_c->istep_dynamic_c);
	init_ctl_int_item(SGS_ctl_c->min_step_dynamic_c);
	init_ctl_int_item(SGS_ctl_c->max_step_dynamic_c);
	init_ctl_int_item(SGS_ctl_c->ngrp_radial_ave_c);
	init_ctl_int_item(SGS_ctl_c->ngrp_med_ave_c);
	
	SGS_ctl_c->stabilize_weight_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->delta_to_shrink_dynamic_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->delta_to_extend_dynamic_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->clipping_limit_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->SGS_hf_factor_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->SGS_cf_factor_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->SGS_mf_factor_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->SGS_mxwl_factor_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	SGS_ctl_c->SGS_uxb_factor_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	
	init_ctl_real_item(SGS_ctl_c->stabilize_weight_c);
	init_ctl_real_item(SGS_ctl_c->delta_to_shrink_dynamic_c);
	init_ctl_real_item(SGS_ctl_c->delta_to_extend_dynamic_c);
	init_ctl_real_item(SGS_ctl_c->clipping_limit_c);
	init_ctl_real_item(SGS_ctl_c->SGS_hf_factor_c);
	init_ctl_real_item(SGS_ctl_c->SGS_cf_factor_c);
	init_ctl_real_item(SGS_ctl_c->SGS_mf_factor_c);
	init_ctl_real_item(SGS_ctl_c->SGS_mxwl_factor_c);
	init_ctl_real_item(SGS_ctl_c->SGS_uxb_factor_c);
	
	return;
}

void dealloc_SGS_model_ctl_c(struct SGS_model_control_c *SGS_ctl_c){
	
	SGS_ctl_c->num_sph_filter_c = 0;
	dealloc_sph_filter_ctl_c(SGS_ctl_c);
	
	SGS_ctl_c->iflag_file_ctl = 0;
	SGS_ctl_c->iflag_layer_ctl = 0;
	SGS_ctl_c->iflag_3dfilter_ctl = 0;
	dealloc_filter_file_ctl_c(SGS_ctl_c->ffile_c);
	dealloc_layering_ctl_c(SGS_ctl_c->elayer_c);
	dealloc_SGS_3d_filter_ctl_c(SGS_ctl_c->s3df_c);
	free(SGS_ctl_c->ffile_c);
	free(SGS_ctl_c->elayer_c);
	free(SGS_ctl_c->s3df_c);
	
	dealloc_ctl_chara_array(SGS_ctl_c->SGS_terms_c);
	dealloc_ctl_chara_array(SGS_ctl_c->commutate_fld_c);
	free(SGS_ctl_c->SGS_terms_c);
	free(SGS_ctl_c->commutate_fld_c);
	
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_model_name_c);
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_filter_name_c);
	dealloc_ctl_chara_item(SGS_ctl_c->DIFF_model_coef_c);
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_negative_clip_c);
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_marging_c);
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_perturbation_c);
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_model_coef_type_c);
	dealloc_ctl_chara_item(SGS_ctl_c->heat_flux_csim_type_c);
	dealloc_ctl_chara_item(SGS_ctl_c->comp_flux_csim_type_c);
	dealloc_ctl_chara_item(SGS_ctl_c->mom_flux_csim_type_c);
	dealloc_ctl_chara_item(SGS_ctl_c->maxwell_csim_type_c);
	dealloc_ctl_chara_item(SGS_ctl_c->uxb_csim_type_c);
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_model_coef_coord_c);
	dealloc_ctl_chara_item(SGS_ctl_c->SGS_buo_Csim_usage_c);
	
	free(SGS_ctl_c->SGS_model_name_c);
	free(SGS_ctl_c->SGS_filter_name_c);
	free(SGS_ctl_c->DIFF_model_coef_c);
	free(SGS_ctl_c->SGS_negative_clip_c);
	free(SGS_ctl_c->SGS_marging_c);
	free(SGS_ctl_c->SGS_perturbation_c);
	free(SGS_ctl_c->SGS_model_coef_type_c);
	free(SGS_ctl_c->heat_flux_csim_type_c);
	free(SGS_ctl_c->comp_flux_csim_type_c);
	free(SGS_ctl_c->mom_flux_csim_type_c);
	free(SGS_ctl_c->maxwell_csim_type_c);
	free(SGS_ctl_c->uxb_csim_type_c);
	free(SGS_ctl_c->SGS_model_coef_coord_c);
	free(SGS_ctl_c->SGS_buo_Csim_usage_c);
	
	free(SGS_ctl_c->stabilize_weight_c);
	free(SGS_ctl_c->delta_to_shrink_dynamic_c);
	free(SGS_ctl_c->delta_to_extend_dynamic_c);
	free(SGS_ctl_c->clipping_limit_c);
	free(SGS_ctl_c->SGS_hf_factor_c);
	free(SGS_ctl_c->SGS_cf_factor_c);
	free(SGS_ctl_c->SGS_mf_factor_c);
	free(SGS_ctl_c->SGS_mxwl_factor_c);
	free(SGS_ctl_c->SGS_uxb_factor_c);
	
	return;
}

int read_SGS_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct SGS_model_control_c *SGS_ctl_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		iflag = find_control_array_flag_c(buf, label_SGS_model_ctl[33], &SGS_ctl_c->num_sph_filter_c);
		if(iflag > 0) iflag = read_sph_filter_ctl_c(fp, buf, label_SGS_model_ctl[33],
					SGS_ctl_c);
		
		if(right_begin_flag_c(buf, label_SGS_model_ctl[31]) > 0){
			SGS_ctl_c->iflag_file_ctl = read_filter_file_ctl_c(fp, buf, 
						label_SGS_model_ctl[31], SGS_ctl_c->ffile_c);
		};
		if(right_begin_flag_c(buf, label_SGS_model_ctl[32]) > 0){
			SGS_ctl_c->iflag_layer_ctl = read_layering_ctl_c(fp, buf, 
					label_SGS_model_ctl[32], SGS_ctl_c->elayer_c);
		};
		if(right_begin_flag_c(buf, label_SGS_model_ctl[11]) > 0){
			SGS_ctl_c->iflag_3dfilter_ctl = read_SGS_3d_filter_ctl_c(fp, buf, 
					label_SGS_model_ctl[11], SGS_ctl_c->s3df_c);
		};
		
		read_character_ctl_array_c(fp, buf, label_SGS_model_ctl[29], SGS_ctl_c->SGS_terms_c);
		read_character_ctl_array_c(fp, buf, label_SGS_model_ctl[30], SGS_ctl_c->commutate_fld_c);
		
		read_character_ctl_item_c(buf, label_SGS_model_ctl[ 0], SGS_ctl_c->SGS_model_name_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[ 1], SGS_ctl_c->SGS_filter_name_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[10], SGS_ctl_c->DIFF_model_coef_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[ 2], SGS_ctl_c->SGS_negative_clip_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[ 9], SGS_ctl_c->SGS_marging_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[18], SGS_ctl_c->SGS_perturbation_c);
		
		read_character_ctl_item_c(buf, label_SGS_model_ctl[21], SGS_ctl_c->SGS_model_coef_type_c);
		
		read_character_ctl_item_c(buf, label_SGS_model_ctl[22], SGS_ctl_c->heat_flux_csim_type_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[23], SGS_ctl_c->comp_flux_csim_type_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[24], SGS_ctl_c->mom_flux_csim_type_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[25], SGS_ctl_c->maxwell_csim_type_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[26], SGS_ctl_c->uxb_csim_type_c);
		
		read_character_ctl_item_c(buf, label_SGS_model_ctl[27], SGS_ctl_c->SGS_model_coef_coord_c);
		read_character_ctl_item_c(buf, label_SGS_model_ctl[28], SGS_ctl_c->SGS_buo_Csim_usage_c);
		
		read_integer_ctl_item_c(buf, label_SGS_model_ctl[12], SGS_ctl_c->istep_dynamic_c);
		read_integer_ctl_item_c(buf, label_SGS_model_ctl[14], SGS_ctl_c->min_step_dynamic_c);
		read_integer_ctl_item_c(buf, label_SGS_model_ctl[15], SGS_ctl_c->max_step_dynamic_c);
		
		read_integer_ctl_item_c(buf, label_SGS_model_ctl[19], SGS_ctl_c->ngrp_radial_ave_c);
		read_integer_ctl_item_c(buf, label_SGS_model_ctl[20], SGS_ctl_c->ngrp_med_ave_c);
		
		read_real_ctl_item_c(buf, label_SGS_model_ctl[13], SGS_ctl_c->stabilize_weight_c);
		read_real_ctl_item_c(buf, label_SGS_model_ctl[16], SGS_ctl_c->delta_to_shrink_dynamic_c);
		read_real_ctl_item_c(buf, label_SGS_model_ctl[17], SGS_ctl_c->delta_to_extend_dynamic_c);
		
		read_real_ctl_item_c(buf, label_SGS_model_ctl[ 3], SGS_ctl_c->clipping_limit_c);
		
		read_real_ctl_item_c(buf, label_SGS_model_ctl[ 4], SGS_ctl_c->SGS_hf_factor_c);
		read_real_ctl_item_c(buf, label_SGS_model_ctl[ 5], SGS_ctl_c->SGS_cf_factor_c);
		read_real_ctl_item_c(buf, label_SGS_model_ctl[ 6], SGS_ctl_c->SGS_mf_factor_c);
		read_real_ctl_item_c(buf, label_SGS_model_ctl[ 7], SGS_ctl_c->SGS_mxwl_factor_c);
		read_real_ctl_item_c(buf, label_SGS_model_ctl[ 8], SGS_ctl_c->SGS_uxb_factor_c);
	};
	return 1;
};
 
int write_SGS_model_ctl_c(FILE *fp, int level, const char *label, 
                          struct SGS_model_control_c *SGS_ctl_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 0], SGS_ctl_c->SGS_model_name_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 1], SGS_ctl_c->SGS_filter_name_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 2], SGS_ctl_c->SGS_negative_clip_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 3], SGS_ctl_c->clipping_limit_c);
	
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 9], SGS_ctl_c->SGS_marging_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[10], SGS_ctl_c->DIFF_model_coef_c);
	
	if(SGS_ctl_c->iflag_3dfilter_ctl > 0){
		fprintf(fp, "!\n");
		level = write_SGS_3d_filter_ctl_c(fp, level, label_SGS_model_ctl[11], SGS_ctl_c->s3df_c);
	};
	
	write_integer_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[12], SGS_ctl_c->istep_dynamic_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[13], SGS_ctl_c->stabilize_weight_c);
	write_integer_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[14], SGS_ctl_c->min_step_dynamic_c);
	write_integer_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[15], SGS_ctl_c->max_step_dynamic_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[16], SGS_ctl_c->delta_to_shrink_dynamic_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[17], SGS_ctl_c->delta_to_extend_dynamic_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[18], SGS_ctl_c->SGS_perturbation_c);
	
	write_integer_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[19], SGS_ctl_c->ngrp_radial_ave_c);
	write_integer_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[20], SGS_ctl_c->ngrp_med_ave_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[21], SGS_ctl_c->SGS_model_coef_type_c);
	
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[22], SGS_ctl_c->heat_flux_csim_type_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[23], SGS_ctl_c->comp_flux_csim_type_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[24], SGS_ctl_c->mom_flux_csim_type_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[25], SGS_ctl_c->maxwell_csim_type_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[26], SGS_ctl_c->uxb_csim_type_c);
	
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[27], SGS_ctl_c->SGS_model_coef_coord_c);
	write_character_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[28], SGS_ctl_c->SGS_buo_Csim_usage_c);
	
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 4], SGS_ctl_c->SGS_hf_factor_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 5], SGS_ctl_c->SGS_cf_factor_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 6], SGS_ctl_c->SGS_mf_factor_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 7], SGS_ctl_c->SGS_mxwl_factor_c);
	write_real_ctl_item_c(fp, level, SGS_ctl_c->maxlen, label_SGS_model_ctl[ 8], SGS_ctl_c->SGS_uxb_factor_c);
	
	if(SGS_ctl_c->SGS_terms_c->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, strlen(label_SGS_model_ctl[29]),
				label_SGS_model_ctl[29], SGS_ctl_c->SGS_terms_c);
	
	if(SGS_ctl_c->commutate_fld_c->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, strlen(label_SGS_model_ctl[30]),
				label_SGS_model_ctl[30], SGS_ctl_c->commutate_fld_c);
	
	if(SGS_ctl_c->iflag_file_ctl > 0){
		fprintf(fp, "!\n");
		level = write_filter_file_ctl_c(fp, level, label_SGS_model_ctl[31], SGS_ctl_c->ffile_c);
	};
	if(SGS_ctl_c->iflag_layer_ctl > 0){
		fprintf(fp, "!\n");
		level = write_layering_ctl_c(fp, level, label_SGS_model_ctl[32], SGS_ctl_c->elayer_c);
	};
	if(SGS_ctl_c->num_sph_filter_c > 0){
		fprintf(fp, "!\n");
		level = write_sph_filter_ctl_c(fp, level, label_SGS_model_ctl[33], SGS_ctl_c);
	};
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
