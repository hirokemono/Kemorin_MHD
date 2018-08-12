/*
//  t_control_data_LIC_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#include "t_control_data_LIC_c.h"

#define NLBL_LIC_CTL  18

const char label_lic_ctl_c[NLBL_LIC_CTL][KCHARA_C] = {
	/*[ 0]*/	{"LIC_field"},
	
	/*[ 1]*/	{"color_field"},
	/*[ 2]*/	{"color_component"},
	/*[ 3]*/	{"opacity_field"},
	/*[ 4]*/	{"opacity_component"},
	
	/*[ 5]*/	{"masking_control"},
	
	/*[ 6]*/	{"noise_type"},
	/*[ 7]*/	{"noise_file_prefix"},
	/*[ 8]*/	{"noise_resolution"},
	
	/*[ 9]*/	{"kernel_function_type"},
	/*[10]*/	{"kernel_image_prefix"},
	
	/*[11]*/	{"LIC_trace_length_mode"},
	/*[12]*/	{"LIC_trace_length"},
	/*[13]*/	{"LIC_trace_count"},
	
	/*[14]*/	{"normalization_type"},
	/*[15]*/	{"normalization_value"},
	/*[16]*/	{"reflection_reference"},
	/*[17]*/	{"referection_parameter"}
};


void get_label_lic_ctl_c(int index, char *label){
    if(index < NLBL_LIC_CTL) strngcopy(label, label_lic_ctl_c[index]);
    return;
};

void alloc_lic_ctl_c(struct lic_ctl_c *lic_c){
	int i;
	
	lic_c->maxlen = 0;
	for (i=0;i<NLBL_LIC_CTL;i++){
		if(strlen(label_lic_ctl_c[i]) > lic_c->maxlen){
			lic_c->maxlen = (int) strlen(label_lic_ctl_c[i]);
		};
	};
	
	lic_c->LIC_field_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(lic_c->LIC_field_ctl);
	
	lic_c->color_field_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->color_component_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->opacity_field_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->opacity_component_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(lic_c->color_field_ctl);
	alloc_chara_ctl_item_c(lic_c->color_component_ctl);
	alloc_chara_ctl_item_c(lic_c->opacity_field_ctl);
	alloc_chara_ctl_item_c(lic_c->opacity_component_ctl);
	
	lic_c->noise_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->noise_file_prefix_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->noise_resolution_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	alloc_chara_ctl_item_c(lic_c->noise_type_ctl);
	alloc_chara_ctl_item_c(lic_c->noise_file_prefix_ctl);
	init_ctl_int_item(lic_c->noise_resolution_ctl);
	
	lic_c->kernel_function_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->kernal_file_prefix_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(lic_c->kernel_function_type_ctl);
	alloc_chara_ctl_item_c(lic_c->kernal_file_prefix_ctl);
	
	lic_c->LIC_trace_length_def_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->LIC_trace_length_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	lic_c->LIC_trace_count_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	alloc_chara_ctl_item_c(lic_c->LIC_trace_length_def_ctl);
	init_ctl_real_item(lic_c->LIC_trace_length_ctl);
	init_ctl_int_item(lic_c->LIC_trace_count_ctl);
	
	lic_c->normalization_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->normalization_value_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	alloc_chara_ctl_item_c(lic_c->normalization_type_ctl);
	init_ctl_real_item(lic_c->normalization_value_ctl);
	
	lic_c->reflection_ref_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_c->reflection_parameter_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	alloc_chara_ctl_item_c(lic_c->reflection_ref_type_ctl);
	init_ctl_real_item(lic_c->reflection_parameter_ctl);
	
	init_lic_masking_ctl_list(&lic_c->lic_mask_list);
	
	return;
};


void dealloc_lic_ctl_c(struct lic_ctl_c *lic_c){
	dealloc_chara_ctl_item_c(lic_c->LIC_field_ctl);
	free(lic_c->LIC_field_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->color_field_ctl);
	dealloc_chara_ctl_item_c(lic_c->color_component_ctl);
	dealloc_chara_ctl_item_c(lic_c->opacity_field_ctl);
	dealloc_chara_ctl_item_c(lic_c->opacity_component_ctl);
	free(lic_c->color_field_ctl);
	free(lic_c->color_component_ctl);
	free(lic_c->opacity_field_ctl);
	free(lic_c->opacity_component_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->noise_type_ctl);
	dealloc_chara_ctl_item_c(lic_c->noise_file_prefix_ctl);
	free(lic_c->noise_type_ctl);
	free(lic_c->noise_file_prefix_ctl);
	free(lic_c->noise_resolution_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->kernel_function_type_ctl);
	dealloc_chara_ctl_item_c(lic_c->kernal_file_prefix_ctl);
	free(lic_c->kernel_function_type_ctl);
	free(lic_c->kernal_file_prefix_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->LIC_trace_length_def_ctl);
	free(lic_c->LIC_trace_length_def_ctl);
	free(lic_c->LIC_trace_length_ctl);
	free(lic_c->LIC_trace_count_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->normalization_type_ctl);
	free(lic_c->normalization_type_ctl);
	free(lic_c->normalization_value_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->reflection_ref_type_ctl);
	free(lic_c->reflection_ref_type_ctl);
	free(lic_c->reflection_parameter_ctl);
	
	clear_lic_masking_ctl_list(&lic_c->lic_mask_list);
	return;
};


int read_lic_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_ctl_c *lic_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 0], lic_c->LIC_field_ctl);
		
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 1], lic_c->color_field_ctl);
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 2], lic_c->color_component_ctl);
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 3], lic_c->opacity_field_ctl);
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 4], lic_c->opacity_component_ctl);
		
		iflag = read_lic_masking_ctl_list(fp, buf, label_lic_ctl_c[ 5], &lic_c->lic_mask_list);
		
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 6], lic_c->noise_type_ctl);
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 7], lic_c->noise_file_prefix_ctl);
		read_integer_ctl_item_c(buf, label_lic_ctl_c[ 8], lic_c->noise_resolution_ctl);
		
		read_chara_ctl_item_c(buf, label_lic_ctl_c[ 9], lic_c->kernel_function_type_ctl);
		read_chara_ctl_item_c(buf, label_lic_ctl_c[10], lic_c->kernal_file_prefix_ctl);
		
		read_chara_ctl_item_c(buf, label_lic_ctl_c[11], lic_c->LIC_trace_length_def_ctl);
		read_real_ctl_item_c(buf, label_lic_ctl_c[12], lic_c->LIC_trace_length_ctl);
		read_integer_ctl_item_c(buf, label_lic_ctl_c[13], lic_c->LIC_trace_count_ctl);
		
		read_chara_ctl_item_c(buf, label_lic_ctl_c[14], lic_c->normalization_type_ctl);
		read_real_ctl_item_c(buf, label_lic_ctl_c[15], lic_c->normalization_value_ctl);
		
		read_chara_ctl_item_c(buf, label_lic_ctl_c[16], lic_c->reflection_ref_type_ctl);
		read_real_ctl_item_c(buf, label_lic_ctl_c[17], lic_c->reflection_parameter_ctl);
	};
	return 1;
};

int write_lic_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_ctl_c *lic_c){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 0], lic_c->LIC_field_ctl);
	
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 1], lic_c->color_field_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 2], lic_c->color_component_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 3], lic_c->opacity_field_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 4], lic_c->opacity_component_ctl);
	
	level = write_lic_masking_ctl_list(fp, level, label_lic_ctl_c[ 5], &lic_c->lic_mask_list);
	
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 6], lic_c->noise_type_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 7], lic_c->noise_file_prefix_ctl);
	write_integer_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 8], lic_c->noise_resolution_ctl);
	
	
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[ 9], lic_c->kernel_function_type_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[10], lic_c->kernal_file_prefix_ctl);
	
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[11], lic_c->LIC_trace_length_def_ctl);
	write_real_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[12], lic_c->LIC_trace_length_ctl);
	write_integer_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[13], lic_c->LIC_trace_count_ctl);
	
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[14], lic_c->normalization_type_ctl);
	write_real_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[15], lic_c->normalization_value_ctl);
	
	write_chara_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[16], lic_c->reflection_ref_type_ctl);
	write_real_ctl_item_c(fp, level, lic_c->maxlen, label_lic_ctl_c[17], lic_c->reflection_parameter_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

