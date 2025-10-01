/*
//  t_control_data_LIC_kernel_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#include "t_control_data_LIC_kernel_c.h"

struct lic_kernel_ctl_c * init_lic_kernel_ctl_c(){
    struct lic_kernel_ctl_c *lic_knl_c;
    if((lic_knl_c = (struct lic_kernel_ctl_c *) malloc(sizeof(struct lic_kernel_ctl_c))) == NULL) {
        printf("malloc error for lic_kernel_ctl_c \n");
        exit(0);
    }
    
	lic_knl_c->label_lic_kernel = init_label_LIC_kernel_f();
	lic_knl_c->kernel_function_type_ctl = init_chara_ctl_item_c();
	
	lic_knl_c->kernel_resolution_ctl_c = init_int_ctl_item_c();
	
    lic_knl_c->kernel_sigma_ctl_c =  init_real_ctl_item_c();
    lic_knl_c->kernel_peak_ctl_c =   init_real_ctl_item_c();
    lic_knl_c->kernel_half_lengh_c = init_real_ctl_item_c();
	
	lic_knl_c->LIC_trace_length_def_ctl = init_chara_ctl_item_c();
	lic_knl_c->LIC_trace_count_ctl = init_int_ctl_item_c();
	
	return lic_knl_c;
};


void dealloc_lic_kernel_ctl_c(struct lic_kernel_ctl_c *lic_knl_c){
	dealloc_control_labels_f(lic_knl_c->label_lic_kernel);
	dealloc_chara_ctl_item_c(lic_knl_c->kernel_function_type_ctl);
	free(lic_knl_c->kernel_resolution_ctl_c);
	free(lic_knl_c->kernel_sigma_ctl_c);
	free(lic_knl_c->kernel_peak_ctl_c);
	free(lic_knl_c->kernel_half_lengh_c);
	
	dealloc_chara_ctl_item_c(lic_knl_c->LIC_trace_length_def_ctl);
    free(lic_knl_c);
	return;
};


int read_lic_kernel_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_kernel_ctl_c *lic_knl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
        read_chara_ctl_item_c(buf, lic_knl_c->label_lic_kernel->label[0], 
                              lic_knl_c->kernel_function_type_ctl);
		read_integer_ctl_item_c(buf, lic_knl_c->label_lic_kernel->label[1],
								lic_knl_c->kernel_resolution_ctl_c);
		
		read_real_ctl_item_c(buf, lic_knl_c->label_lic_kernel->label[2],
							 lic_knl_c->kernel_sigma_ctl_c);
		read_real_ctl_item_c(buf, lic_knl_c->label_lic_kernel->label[3],
							 lic_knl_c->kernel_peak_ctl_c);
		
		read_chara_ctl_item_c(buf, lic_knl_c->label_lic_kernel->label[4], 
							  lic_knl_c->LIC_trace_length_def_ctl);
		read_real_ctl_item_c(buf, lic_knl_c->label_lic_kernel->label[5],
							 lic_knl_c->kernel_half_lengh_c);
		read_integer_ctl_item_c(buf, lic_knl_c->label_lic_kernel->label[6],
								lic_knl_c->LIC_trace_count_ctl);
	};
	return 1;
};

int write_lic_kernel_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_kernel_ctl_c *lic_knl_c){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, lic_knl_c->label_lic_kernel->maxlen, 
						   lic_knl_c->label_lic_kernel->label[0], 
                           lic_knl_c->kernel_function_type_ctl);
	write_integer_ctl_item_c(fp, level, lic_knl_c->label_lic_kernel->maxlen,
							 lic_knl_c->label_lic_kernel->label[1], 
                             lic_knl_c->kernel_resolution_ctl_c);
	write_real_ctl_item_c(fp, level, lic_knl_c->label_lic_kernel->maxlen, 
						  lic_knl_c->label_lic_kernel->label[2],
						  lic_knl_c->kernel_sigma_ctl_c);
	write_real_ctl_item_c(fp, level, lic_knl_c->label_lic_kernel->maxlen, 
						  lic_knl_c->label_lic_kernel->label[3],
						  lic_knl_c->kernel_peak_ctl_c);
	
	write_chara_ctl_item_c(fp, level, lic_knl_c->label_lic_kernel->maxlen, 
						   lic_knl_c->label_lic_kernel->label[4], 
						   lic_knl_c->LIC_trace_length_def_ctl);
	write_real_ctl_item_c(fp, level, lic_knl_c->label_lic_kernel->maxlen, 
						  lic_knl_c->label_lic_kernel->label[5],
						  lic_knl_c->kernel_half_lengh_c);
	write_integer_ctl_item_c(fp, level, lic_knl_c->label_lic_kernel->maxlen, 
							 lic_knl_c->label_lic_kernel->label[6],
							 lic_knl_c->LIC_trace_count_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

