/*
//  t_control_data_LIC_noise_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#include "t_control_data_LIC_noise_c.h"

#define NLBL_LIC_CTL  18

int num_ctl_label_LIC_noise_f();
void set_ctl_label_LIC_noise_f(char *label_1d);


struct label_list_f * init_ctl_label_LIC_noise_f(){
	int len_fix = lengthchara_f();
	struct label_list_f *label_list = alloc_ctl_label();
	label_list->num_labels = num_ctl_label_LIC_noise_f();
	
    char *packed_name = alloc_string((long) (len_fix*label_list->num_labels));
	set_ctl_label_LIC_noise_f(packed_name);
	set_labels_from_packed(len_fix, packed_name, label_list);
	free(packed_name);
	return label_list;
};


void alloc_lic_noise_ctl_c(struct lic_noise_ctl_c *lic_nze_c){
	int i;
	lic_nze_c->label_lic_noise = init_ctl_label_LIC_noise_f();
	
	lic_nze_c->noise_type_ctl_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_nze_c->noise_file_name_ctl_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_nze_c->noise_file_format_ctl_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_chara_ctl_item_c(lic_nze_c->noise_type_ctl_c);
	alloc_chara_ctl_item_c(lic_nze_c->noise_file_name_ctl_c);
	alloc_chara_ctl_item_c(lic_nze_c->noise_file_format_ctl_c);
	
	lic_nze_c->noise_resolution_ctl_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	lic_nze_c->noise_stepping_ctl_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_int_ctl_item_c(lic_nze_c->noise_resolution_ctl_c);
	init_int_ctl_item_c(lic_nze_c->noise_stepping_ctl_c);
	
	lic_nze_c->noise_cube_size_ctl_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(lic_nze_c->noise_cube_size_ctl_c);
	
	return;
};


void dealloc_lic_noise_ctl_c(struct lic_noise_ctl_c *lic_nze_c){
	dealloc_ctl_label(lic_nze_c->label_lic_noise);
	dealloc_chara_ctl_item_c(lic_nze_c->noise_type_ctl_c);
	dealloc_chara_ctl_item_c(lic_nze_c->noise_file_name_ctl_c);
	dealloc_chara_ctl_item_c(lic_nze_c->noise_file_format_ctl_c);
	free(lic_nze_c->noise_type_ctl_c);
	free(lic_nze_c->noise_file_name_ctl_c);
	free(lic_nze_c->noise_file_format_ctl_c);
	
	free(lic_nze_c->noise_resolution_ctl_c);
	free(lic_nze_c->noise_stepping_ctl_c);
	
	free(lic_nze_c->noise_cube_size_ctl_c);
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

