/*
//  t_control_data_LIC_kernel_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#include "t_control_data_LIC_kernel_c.h"

int num_ctl_label_LIC_kernel_f();
void set_ctl_label_LIC_kernel_f(char *label_1d);

struct label_list_f * init_ctl_label_LIC_kernel_f(){
	int len_fix = lengthchara_f();
	struct label_list_f *label_list = alloc_ctl_label();
	label_list->num_labels = num_ctl_label_LIC_kernel_f();
	
    char *packed_name = alloc_string((long) (len_fix*label_list->num_labels));
	
	set_ctl_label_LIC_kernel_f(packed_name);
	set_labels_from_packed(len_fix, packed_name, label_list);
	free(packed_name);
	return label_list;
};

struct lic_kernel_ctl_c * alloc_lic_kernel_ctl_c(){
    struct lic_kernel_ctl_c *lic_knl_c;
    if((lic_knl_c = (struct lic_kernel_ctl_c *) malloc(sizeof(struct lic_kernel_ctl_c))) == NULL) {
        printf("malloc error for lic_kernel_ctl_c \n");
        exit(0);
    }
    
	lic_knl_c->label_lic_kernel = init_ctl_label_LIC_kernel_f();
	lic_knl_c->kernel_function_type_ctl = init_chara_ctl_item_c();
	
	lic_knl_c->kernel_resolution_ctl_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_int_ctl_item_c(lic_knl_c->kernel_resolution_ctl_c);
	
	lic_knl_c->kernel_sigma_ctl_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(lic_knl_c->kernel_sigma_ctl_c);
	lic_knl_c->kernel_peak_ctl_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(lic_knl_c->kernel_peak_ctl_c);
	lic_knl_c->kernel_half_lengh_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(lic_knl_c->kernel_half_lengh_c);
	
	lic_knl_c->LIC_trace_length_def_ctl = init_chara_ctl_item_c();
	lic_knl_c->LIC_trace_count_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_int_ctl_item_c(lic_knl_c->LIC_trace_count_ctl);
	
	return lic_knl_c;
};


void dealloc_lic_kernel_ctl_c(struct lic_kernel_ctl_c *lic_knl_c){
	dealloc_ctl_label(lic_knl_c->label_lic_kernel);
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

