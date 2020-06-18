/*
//  t_control_data_LIC_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#include "t_control_data_LIC_c.h"

struct lic_ctl_c * init_lic_ctl_c(){
    struct lic_ctl_c *lic_c;
    if((lic_c = (struct lic_ctl_c *) malloc(sizeof(struct lic_ctl_c))) == NULL) {
        printf("malloc error for lic_ctl_c \n");
        exit(0);
    }
    
	lic_c->label_lic_ctl = init_label_LIC_ctl_f();
	lic_c->LIC_field_ctl = init_chara_ctl_item_c();
	
	lic_c->color_field_ctl = init_chara_ctl_item_c();
	lic_c->color_component_ctl = init_chara_ctl_item_c();
	lic_c->opacity_field_ctl = init_chara_ctl_item_c();
	lic_c->opacity_component_ctl = init_chara_ctl_item_c();
	
	lic_c->vr_sample_mode_ctl_c = init_chara_ctl_item_c();
	lic_c->step_size_ctl_c = init_real_ctl_item_c();
	
	lic_c->normalization_type_ctl_c = init_chara_ctl_item_c();
	lic_c->normalization_value_ctl_c = init_real_ctl_item_c();
	
	init_lic_masking_ctl_list(&lic_c->lic_mask_list);
	
	lic_c->lic_nze_c = init_lic_noise_ctl_c();
	lic_c->lic_knl_c = init_lic_kernel_ctl_c();
	return lic_c;
};


void dealloc_lic_ctl_c(struct lic_ctl_c *lic_c){
	dealloc_control_labels_f(lic_c->label_lic_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->LIC_field_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->color_field_ctl);
	dealloc_chara_ctl_item_c(lic_c->color_component_ctl);
	dealloc_chara_ctl_item_c(lic_c->opacity_field_ctl);
	dealloc_chara_ctl_item_c(lic_c->opacity_component_ctl);
	
	dealloc_chara_ctl_item_c(lic_c->vr_sample_mode_ctl_c);
	free(lic_c->step_size_ctl_c);
	
	dealloc_chara_ctl_item_c(lic_c->normalization_type_ctl_c);
	free(lic_c->normalization_value_ctl_c);
	
	clear_lic_masking_ctl_list(&lic_c->lic_mask_list);
	dealloc_lic_noise_ctl_c(lic_c->lic_nze_c);
	dealloc_lic_kernel_ctl_c(lic_c->lic_knl_c);
    
    free(lic_c);
	return;
};


int read_lic_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_ctl_c *lic_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, lic_c->label_lic_ctl->label[ 0],
							  lic_c->LIC_field_ctl);
		
		read_chara_ctl_item_c(buf, lic_c->label_lic_ctl->label[ 1],
							  lic_c->color_field_ctl);
		read_chara_ctl_item_c(buf, lic_c->label_lic_ctl->label[ 2],
							  lic_c->color_component_ctl);
		read_chara_ctl_item_c(buf, lic_c->label_lic_ctl->label[ 3],
							  lic_c->opacity_field_ctl);
		read_chara_ctl_item_c(buf, lic_c->label_lic_ctl->label[ 4],
							  lic_c->opacity_component_ctl);
		
		iflag = read_lic_masking_ctl_list(fp, buf, lic_c->label_lic_ctl->label[ 5],
										  &lic_c->lic_mask_list);
		iflag = read_lic_noise_ctl_c(fp, buf, lic_c->label_lic_ctl->label[ 6],
									 lic_c->lic_nze_c);
		iflag = read_lic_kernel_ctl_c(fp, buf, lic_c->label_lic_ctl->label[ 7], 
									  lic_c->lic_knl_c);
		
		read_chara_ctl_item_c(buf, lic_c->label_lic_ctl->label[ 8],
							  lic_c->vr_sample_mode_ctl_c);
		read_real_ctl_item_c(buf, lic_c->label_lic_ctl->label[ 9],
							 lic_c->step_size_ctl_c);
		
		read_chara_ctl_item_c(buf, lic_c->label_lic_ctl->label[10],
							  lic_c->normalization_type_ctl_c);
		read_real_ctl_item_c(buf, lic_c->label_lic_ctl->label[11],
							 lic_c->normalization_value_ctl_c);
	};
	return 1;
};

int write_lic_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_ctl_c *lic_c){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						   lic_c->label_lic_ctl->label[ 0], 
						   lic_c->LIC_field_ctl);
	
	write_chara_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						   lic_c->label_lic_ctl->label[ 1], 
						   lic_c->color_field_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						   lic_c->label_lic_ctl->label[ 2], 
						   lic_c->color_component_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						   lic_c->label_lic_ctl->label[ 3], 
						   lic_c->opacity_field_ctl);
	write_chara_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						   lic_c->label_lic_ctl->label[ 4], 
						   lic_c->opacity_component_ctl);
	
	level = write_lic_masking_ctl_list(fp, level, lic_c->label_lic_ctl->label[ 5],
									   &lic_c->lic_mask_list);
	level = write_lic_noise_ctl_c(fp, level, lic_c->label_lic_ctl->label[ 6],
								  lic_c->lic_nze_c);
	level = write_lic_kernel_ctl_c(fp, level, lic_c->label_lic_ctl->label[ 7], 
								   lic_c->lic_knl_c);
	
	write_chara_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						   lic_c->label_lic_ctl->label[ 8], 
						   lic_c->vr_sample_mode_ctl_c);
	write_real_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						  lic_c->label_lic_ctl->label[ 9],
						  lic_c->step_size_ctl_c);
	
	write_chara_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen, 
						   lic_c->label_lic_ctl->label[10],
						   lic_c->normalization_type_ctl_c);
	write_real_ctl_item_c(fp, level, lic_c->label_lic_ctl->maxlen,
						  lic_c->label_lic_ctl->label[11],
						  lic_c->normalization_value_ctl_c);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

