/*
//  t_ctl_data_4_fields_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_4_fields_c.h"

#define NLBL_FIELD_CTL 3

const char label_field_ctl[NLBL_FIELD_CTL][KCHARA_C] = {
	/*[ 0]*/	{"nod_value_ctl"},

	/*[ 1]*/	{"quad_field_name_ctl"},
	/*[ 2]*/	{"linear_field_name_ctl"}
};

void get_label_field_ctl(int index, char *label){
    if(index < NLBL_FIELD_CTL) strngcopy(label, label_field_ctl[index]);
    return;
};


void alloc_field_ctl_c(struct field_ctl_c *fld_ctl){
	int i;
	
	fld_ctl->maxlen = 0;
	for (i=0;i<NLBL_FIELD_CTL;i++){
		if(strlen(label_field_ctl[i]) > fld_ctl->maxlen){
			fld_ctl->maxlen = (int) strlen(label_field_ctl[i]);
		};
	};
	
	init_chara3_ctl_list(&fld_ctl->field_list);
	
	init_chara_ctl_list(&fld_ctl->quad_phys_list);
	init_chara_ctl_list(&fld_ctl->linear_phys_list);
	
	return;
};

void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl){
	
	clear_chara3_ctl_list(&fld_ctl->field_list);
	
	clear_chara_ctl_list(&fld_ctl->quad_phys_list);
	clear_chara_ctl_list(&fld_ctl->linear_phys_list);
	
	return;
};

int read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct field_ctl_c *fld_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara3_ctl_list(fp, buf, label_field_ctl[ 0], &fld_ctl->field_list);
		
		read_chara_ctl_list(fp, buf, label_field_ctl[ 1], &fld_ctl->quad_phys_list);
		read_chara_ctl_list(fp, buf, label_field_ctl[ 2], &fld_ctl->linear_phys_list);
	};
	return 1;
};

int write_field_ctl_c(FILE *fp, int level, const char *label, struct field_ctl_c *fld_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara3_ctl_list(fp, level, label_field_ctl[0], &fld_ctl->field_list);
	
	write_chara_ctl_list(fp, level, label_field_ctl[1], &fld_ctl->quad_phys_list);
	write_chara_ctl_list(fp, level, label_field_ctl[2], &fld_ctl->linear_phys_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

