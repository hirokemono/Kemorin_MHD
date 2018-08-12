/*
//  t_ctl_data_temp_model_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/01.
*/

#include "t_ctl_data_temp_model_c.h"

#define NLBL_REFERENCE_POINT 3
#define NLBL_TAKEPIRO_MODEL  3

#define NLBL_REF_TEMPERATURE_CTL  8

const char label_reference_point_ctl[NLBL_REFERENCE_POINT][KCHARA_C] = {
	/*[ 0]*/	{"depth"},
	/*[ 1]*/	{"temperature"},
	/*[ 2]*/	{"composition"}
};

const char label_takepiro_model_ctl[NLBL_TAKEPIRO_MODEL][KCHARA_C] = {
	/*[ 0]*/	{"stratified_sigma_ctl"},
	/*[ 1]*/	{"stratified_width_ctl"},
	/*[ 2]*/	{"stratified_outer_r_ctl"}
};

const char label_ref_temperature_ctl[NLBL_REF_TEMPERATURE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"ref_temp_ctl"},
	/*[ 1]*/	{"low_temp_ctl"},
	/*[ 2]*/	{"high_temp_ctl"},
	
	/*[ 3]*/	{"ref_comp_ctl"},
	/*[ 4]*/	{"low_comp_ctl"},
	/*[ 5]*/	{"high_comp_ctl"},
	
	/*[ 6]*/	{"stratified_ctl"},
	/*[ 7]*/	{"takepiro_model_ctl"}
};


void get_label_reference_point_ctl(int index, char *label){
    if(index < NLBL_REFERENCE_POINT) strngcopy(label, label_reference_point_ctl[index]);
    return;
};
void get_label_takepiro_model_ctl(int index, char *label){
    if(index < NLBL_TAKEPIRO_MODEL) strngcopy(label, label_takepiro_model_ctl[index]);
    return;
};
void get_label_ref_temperature_ctl(int index, char *label){
    if(index < NLBL_REF_TEMPERATURE_CTL) strngcopy(label, label_ref_temperature_ctl[index]);
    return;
};


void alloc_reference_point_ctl_c(struct reference_point_ctl_c *ref_c){
	int i;
	
	ref_c->maxlen = 0;
	for (i=0;i<NLBL_REFERENCE_POINT;i++){
		if(strlen(label_reference_point_ctl[i]) > ref_c->maxlen){
			ref_c->maxlen = (int) strlen(label_reference_point_ctl[i]);
		};
	};
	
	ref_c->depth_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	ref_c->value_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	
	init_real_ctl_item_c(ref_c->depth_c);
	init_real_ctl_item_c(ref_c->value_c);
	
	return;
};

void dealloc_reference_point_ctl_c(struct reference_point_ctl_c *ref_c){
	
	free(ref_c->depth_c);
	free(ref_c->value_c);
	
	return;
};

int read_reftemp_point_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_point_ctl_c *ref_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real_ctl_item_c(buf, label_reference_point_ctl[ 0], ref_c->depth_c);
		read_real_ctl_item_c(buf, label_reference_point_ctl[ 1], ref_c->value_c);
	};
	return 1;
};

int read_refcomp_point_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_point_ctl_c *ref_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real_ctl_item_c(buf, label_reference_point_ctl[ 0], ref_c->depth_c);
		read_real_ctl_item_c(buf, label_reference_point_ctl[ 2], ref_c->value_c);
	};
	return 1;
};

int write_reftemp_point_ctl_c(FILE *fp, int level, const char *label,
                              struct reference_point_ctl_c *ref_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, ref_c->maxlen, label_reference_point_ctl[ 0], ref_c->depth_c);
	write_real_ctl_item_c(fp, level, ref_c->maxlen, label_reference_point_ctl[ 1], ref_c->value_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

int write_refcomp_point_ctl_c(FILE *fp, int level, const char *label, 
                              struct reference_point_ctl_c *ref_c){
		level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, ref_c->maxlen, label_reference_point_ctl[ 0], ref_c->depth_c);
	write_real_ctl_item_c(fp, level, ref_c->maxlen, label_reference_point_ctl[ 2], ref_c->value_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_takepiro_model_ctl_c(struct takepiro_model_ctl_c *takepiro_c){
	int i;
	
	takepiro_c->maxlen = 0;
	for (i=0;i<NLBL_TAKEPIRO_MODEL;i++){
		if(strlen(label_takepiro_model_ctl[i]) > takepiro_c->maxlen){
			takepiro_c->maxlen = (int) strlen(label_takepiro_model_ctl[i]);
		};
	};
	
	takepiro_c->stratified_sigma_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	takepiro_c->stratified_width_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	takepiro_c->stratified_outer_r_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	
	init_real_ctl_item_c(takepiro_c->stratified_sigma_c);
	init_real_ctl_item_c(takepiro_c->stratified_width_c);
	init_real_ctl_item_c(takepiro_c->stratified_outer_r_c);
	
	return;
};

void dealloc_takepiro_model_ctl_c(struct takepiro_model_ctl_c *takepiro_c){
	
	free(takepiro_c->stratified_sigma_c);
	free(takepiro_c->stratified_width_c);
	free(takepiro_c->stratified_outer_r_c);
	
	return;
};

int read_rtakepiro_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct takepiro_model_ctl_c *takepiro_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real_ctl_item_c(buf, label_takepiro_model_ctl[ 0], takepiro_c->stratified_sigma_c);
		read_real_ctl_item_c(buf, label_takepiro_model_ctl[ 1], takepiro_c->stratified_width_c);
		read_real_ctl_item_c(buf, label_takepiro_model_ctl[ 2], takepiro_c->stratified_outer_r_c);
	};
	return 1;
};

int write_takepiro_model_ctl_c(FILE *fp, int level,	const char *label, struct takepiro_model_ctl_c *takepiro_c){	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, takepiro_c->maxlen, label_takepiro_model_ctl[ 0], takepiro_c->stratified_sigma_c);
	write_real_ctl_item_c(fp, level, takepiro_c->maxlen, label_takepiro_model_ctl[ 1], takepiro_c->stratified_width_c);
	write_real_ctl_item_c(fp, level, takepiro_c->maxlen, label_takepiro_model_ctl[ 2], takepiro_c->stratified_outer_r_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_ref_temperature_ctl_c(struct reference_temperature_c *reft_ctl){
	int i;
	
	reft_ctl->maxlen = 0;
	for (i=0;i<NLBL_REF_TEMPERATURE_CTL;i++){
		if(strlen(label_ref_temperature_ctl[i]) > reft_ctl->maxlen){
			reft_ctl->maxlen = (int) strlen(label_ref_temperature_ctl[i]);
		};
	};
	
	reft_ctl->low_c = (struct reference_point_ctl_c *) malloc(sizeof(struct reference_point_ctl_c));
	alloc_reference_point_ctl_c(reft_ctl->low_c);
	
	reft_ctl->high_c = (struct reference_point_ctl_c *) malloc(sizeof(struct reference_point_ctl_c));
	alloc_reference_point_ctl_c(reft_ctl->high_c);
	
	reft_ctl->takepiro_c = (struct takepiro_model_ctl_c *) malloc(sizeof(struct takepiro_model_ctl_c));
	alloc_takepiro_model_ctl_c(reft_ctl->takepiro_c);
	
	reft_ctl->reference_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	reft_ctl->stratified_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_chara_ctl_item_c(reft_ctl->reference_c);
	alloc_chara_ctl_item_c(reft_ctl->stratified_c);
	
	return;
};

void dealloc_ref_temperature_ctl_c(struct reference_temperature_c *reft_ctl){
	
	dealloc_reference_point_ctl_c(reft_ctl->low_c);
	free(reft_ctl->low_c);
	reft_ctl->iflag_low_temp_ctl = 0;
	
	dealloc_reference_point_ctl_c(reft_ctl->high_c);
	free(reft_ctl->high_c);
	reft_ctl->iflag_high_temp_ctl = 0;
	
	dealloc_takepiro_model_ctl_c(reft_ctl->takepiro_c);
	free(reft_ctl->takepiro_c);
	reft_ctl->iflag_takepiro_c = 0;
	
	dealloc_chara_ctl_item_c(reft_ctl->reference_c);
	dealloc_chara_ctl_item_c(reft_ctl->stratified_c);
	
	free(reft_ctl->reference_c);
	free(reft_ctl->stratified_c);
	
	return;
};

int read_ref_temperature_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_temperature_c *reft_ctl){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_ref_temperature_ctl[ 1]) > 0) 
		reft_ctl->iflag_low_temp_ctl = read_reftemp_point_ctl_c(fp, buf, 
					label_ref_temperature_ctl[ 1], reft_ctl->low_c);
		if(right_begin_flag_c(buf, label_ref_temperature_ctl[ 2]) > 0) 
		reft_ctl->iflag_high_temp_ctl = read_reftemp_point_ctl_c(fp, buf, 
					label_ref_temperature_ctl[ 2], reft_ctl->high_c);
		if(right_begin_flag_c(buf, label_ref_temperature_ctl[ 7]) > 0) 
		reft_ctl->iflag_takepiro_c = read_rtakepiro_model_ctl_c(fp, buf, 
					label_ref_temperature_ctl[ 7], reft_ctl->takepiro_c);
		
		read_chara_ctl_item_c(buf, label_ref_temperature_ctl[ 0], reft_ctl->reference_c);
		read_chara_ctl_item_c(buf, label_ref_temperature_ctl[ 6], reft_ctl->stratified_c);
		
	};
	return 1;
};

int read_ref_composition_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_temperature_c *refc_ctl){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_ref_temperature_ctl[ 4]) > 0) 
		refc_ctl->iflag_low_temp_ctl = read_refcomp_point_ctl_c(fp, buf, 
					label_ref_temperature_ctl[ 4], refc_ctl->low_c);
		if(right_begin_flag_c(buf, label_ref_temperature_ctl[ 5]) > 0) 
		refc_ctl->iflag_high_temp_ctl = read_refcomp_point_ctl_c(fp, buf, 
					label_ref_temperature_ctl[ 5], refc_ctl->high_c);
		if(right_begin_flag_c(buf, label_ref_temperature_ctl[ 7]) > 0) 
		refc_ctl->iflag_takepiro_c = read_rtakepiro_model_ctl_c(fp, buf, 
					label_ref_temperature_ctl[ 7], refc_ctl->takepiro_c);
		
		read_chara_ctl_item_c(buf, label_ref_temperature_ctl[ 3], refc_ctl->reference_c);
		read_chara_ctl_item_c(buf, label_ref_temperature_ctl[ 6], refc_ctl->stratified_c);
	};
	return 1;
};

int write_ref_temperature_ctl_c(FILE *fp, int level, const char *label,
                                struct reference_temperature_c *reft_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, reft_ctl->maxlen, label_ref_temperature_ctl[ 0], reft_ctl->reference_c);
	
    if(reft_ctl->iflag_low_temp_ctl > 0){
        fprintf(fp, "!\n");
        level = write_reftemp_point_ctl_c(fp, level, label_ref_temperature_ctl[ 1], reft_ctl->low_c);
    };
    if(reft_ctl->iflag_high_temp_ctl > 0){
        fprintf(fp, "!\n");
        level = write_reftemp_point_ctl_c(fp, level, label_ref_temperature_ctl[ 2], reft_ctl->high_c);
    };
	if(reft_ctl->iflag_takepiro_c > 0) fprintf(fp, "!\n");
	
	write_chara_ctl_item_c(fp, level, reft_ctl->maxlen, label_ref_temperature_ctl[ 6], reft_ctl->stratified_c);
	
    if(reft_ctl->iflag_takepiro_c > 0){
        level = write_takepiro_model_ctl_c(fp, level, label_ref_temperature_ctl[ 7], reft_ctl->takepiro_c);
    };
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

int write_ref_composition_ctl_c(FILE *fp, int level, const char *label, 
                                struct reference_temperature_c *refc_ctl){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, refc_ctl->maxlen, label_ref_temperature_ctl[ 3], refc_ctl->reference_c);
	
    if(refc_ctl->iflag_low_temp_ctl > 0){ 
        fprintf(fp, "!\n");
        level = write_refcomp_point_ctl_c(fp, level, label_ref_temperature_ctl[ 4], refc_ctl->low_c);
    };
    if(refc_ctl->iflag_high_temp_ctl > 0){
        fprintf(fp, "!\n");
        level = write_refcomp_point_ctl_c(fp, level, label_ref_temperature_ctl[ 5], refc_ctl->high_c);
    };
    
	write_chara_ctl_item_c(fp, level, refc_ctl->maxlen, label_ref_temperature_ctl[ 6], refc_ctl->stratified_c);
	
    if(refc_ctl->iflag_takepiro_c > 0){
        level = write_takepiro_model_ctl_c(fp, level, label_ref_temperature_ctl[ 7], refc_ctl->takepiro_c);
    };
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


