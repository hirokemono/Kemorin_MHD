/*
//  t_control_data_LIC_noise_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#include "t_control_data_LIC_noise_c.h"

struct lic_noise_ctl_c * init_lic_noise_ctl_c(){
    struct lic_noise_ctl_c *lic_nze_c;
    if((lic_nze_c = (struct lic_noise_ctl_c *) malloc(sizeof(struct lic_noise_ctl_c))) == NULL) {
        printf("malloc error for lic_noise_ctl_c \n");
        exit(0);
    }
	lic_nze_c->label_lic_noise = init_label_LIC_noise_f();
	
	lic_nze_c->noise_type_ctl_c = init_chara_ctl_item_c();
	lic_nze_c->noise_file_name_ctl_c = init_chara_ctl_item_c();
	lic_nze_c->noise_file_format_ctl_c = init_chara_ctl_item_c();
	
	lic_nze_c->noise_resolution_ctl_c = init_int_ctl_item_c();
	lic_nze_c->noise_stepping_ctl_c =   init_int_ctl_item_c();
	
	lic_nze_c->noise_cube_size_ctl_c = init_real_ctl_item_c();
	
	return lic_nze_c;
};


void dealloc_lic_noise_ctl_c(struct lic_noise_ctl_c *lic_nze_c){
	dealloc_control_labels_f(lic_nze_c->label_lic_noise);
	dealloc_chara_ctl_item_c(lic_nze_c->noise_type_ctl_c);
	dealloc_chara_ctl_item_c(lic_nze_c->noise_file_name_ctl_c);
	dealloc_chara_ctl_item_c(lic_nze_c->noise_file_format_ctl_c);
	
	free(lic_nze_c->noise_resolution_ctl_c);
	free(lic_nze_c->noise_stepping_ctl_c);
	
	free(lic_nze_c->noise_cube_size_ctl_c);
    free(lic_nze_c);
	return;
};


int read_lic_noise_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_noise_ctl_c *lic_nze_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, lic_nze_c->label_lic_noise->label[ 0], 
							  lic_nze_c->noise_type_ctl_c);
		read_chara_ctl_item_c(buf, lic_nze_c->label_lic_noise->label[ 1],
							  lic_nze_c->noise_file_name_ctl_c);
		read_chara_ctl_item_c(buf, lic_nze_c->label_lic_noise->label[ 2], 
							  lic_nze_c->noise_file_format_ctl_c);
		
		read_integer_ctl_item_c(buf, lic_nze_c->label_lic_noise->label[ 3],
								lic_nze_c->noise_resolution_ctl_c);
		read_integer_ctl_item_c(buf, lic_nze_c->label_lic_noise->label[ 4],
								lic_nze_c->noise_stepping_ctl_c);
		read_real_ctl_item_c(buf, lic_nze_c->label_lic_noise->label[ 5],
							 lic_nze_c->noise_cube_size_ctl_c);
	};
	return 1;
};

int write_lic_noise_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_noise_ctl_c *lic_nze_c){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, lic_nze_c->label_lic_noise->maxlen,
						   lic_nze_c->label_lic_noise->label[ 0], 
						   lic_nze_c->noise_type_ctl_c);
	write_chara_ctl_item_c(fp, level, lic_nze_c->label_lic_noise->maxlen,
						   lic_nze_c->label_lic_noise->label[ 1], 
						   lic_nze_c->noise_file_name_ctl_c);
	write_chara_ctl_item_c(fp, level, lic_nze_c->label_lic_noise->maxlen,
						   lic_nze_c->label_lic_noise->label[ 2], 
						   lic_nze_c->noise_file_format_ctl_c);
	
	write_integer_ctl_item_c(fp, level, lic_nze_c->label_lic_noise->maxlen,
							 lic_nze_c->label_lic_noise->label[ 3], 
							 lic_nze_c->noise_resolution_ctl_c);
	write_integer_ctl_item_c(fp, level, lic_nze_c->label_lic_noise->maxlen,
							 lic_nze_c->label_lic_noise->label[ 4],
							 lic_nze_c->noise_stepping_ctl_c);
	write_real_ctl_item_c(fp, level, lic_nze_c->label_lic_noise->maxlen,
						  lic_nze_c->label_lic_noise->label[ 5],
						  lic_nze_c->noise_cube_size_ctl_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

