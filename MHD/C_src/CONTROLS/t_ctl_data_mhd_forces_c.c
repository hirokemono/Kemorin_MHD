/*
//  t_ctl_data_mhd_forces_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_mhd_forces_c.h"

#define NLBL_FORCES_CTL      1
#define NLBL_GRAVITY_CTL     2
#define NLBL_CORIOLIS_CTL    1
#define NLBL_MAGNETO_CV_CTL  2

const char label_forces_ctl[NLBL_FORCES_CTL][KCHARA_C] = {
	/*[ 0]*/	{"force_ctl"}
};

const char label_gravity_ctl[NLBL_GRAVITY_CTL][KCHARA_C] = {
	/*[ 0]*/	{"gravity_type_ctl"},
	/*[ 1]*/	{"gravity_vec"}
};

const char label_coriolis_ctl[NLBL_CORIOLIS_CTL][KCHARA_C] = {
	/*[ 0]*/	{"rotation_vec"},
};

const char label_magneto_cv_ctl[NLBL_MAGNETO_CV_CTL][KCHARA_C] = {
	/*[ 0]*/	{"magneto_cv_ctl"},
	/*[ 1]*/	{"ext_magne_vec"},
};


void get_label_forces_ctl(int index, char *label){
    if(index < NLBL_FORCES_CTL) strngcopy(label, label_forces_ctl[index]);
    return;
};
void get_label_gravity_ctl(int index, char *label){
    if(index < NLBL_GRAVITY_CTL) strngcopy(label, label_gravity_ctl[index]);
    return;
};
void get_label_coriolis_ctl(int index, char *label){
    if(index < NLBL_CORIOLIS_CTL) strngcopy(label, label_coriolis_ctl[index]);
    return;
};
void get_label_magneto_cv_ctl(int index, char *label){
    if(index < NLBL_MAGNETO_CV_CTL) strngcopy(label, label_magneto_cv_ctl[index]);
    return;
};


void alloc_forces_ctl_c(struct forces_ctl_c *frc_ctl){
	int i;
	
	frc_ctl->maxlen = 0;
	for (i=0;i<NLBL_FORCES_CTL;i++){
		if(strlen(label_forces_ctl[i]) > frc_ctl->maxlen){
			frc_ctl->maxlen = (int) strlen(label_forces_ctl[i]);
		};
	};
	
	init_chara_ctl_list(&frc_ctl->force_names_list);
	
	return;
};

void dealloc_forces_ctl_c(struct forces_ctl_c *frc_ctl){
	clear_chara_ctl_list(&frc_ctl->force_names_list);
	return;
};

int read_forces_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct forces_ctl_c *frc_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_list(fp, buf, label_forces_ctl[ 0], &frc_ctl->force_names_list);
	};
	return 1;
};

int write_forces_ctl_c(FILE *fp, int level,	const char *label, 
                       struct forces_ctl_c *frc_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_list(fp, level, label_forces_ctl[0], &frc_ctl->force_names_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_gravity_ctl_c(struct gravity_ctl_c *g_ctl){
	int i;
	
	g_ctl->maxlen = 0;
	for (i=0;i<NLBL_GRAVITY_CTL;i++){
		if(strlen(label_gravity_ctl[i]) > g_ctl->maxlen){
			g_ctl->maxlen = (int) strlen(label_gravity_ctl[i]);
		};
	};
	
	init_chara_real_ctl_list(&g_ctl->gravity_vec_list);
	g_ctl->gravity_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(g_ctl->gravity_c);
	
	return;
};

void dealloc_gravity_ctl_c(struct gravity_ctl_c *g_ctl){
	
	clear_chara_real_ctl_list(&g_ctl->gravity_vec_list);
	
	dealloc_chara_ctl_item_c(g_ctl->gravity_c);
	free(g_ctl->gravity_c);
	
	return;
};

int read_gravity_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct gravity_ctl_c *g_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_gravity_ctl[ 0], g_ctl->gravity_c);
		read_chara_real_ctl_list(fp, buf, label_gravity_ctl[ 1], &g_ctl->gravity_vec_list);
	};
	return 1;
};

int write_gravity_ctl_c(FILE *fp, int level, const char *label,
                        struct gravity_ctl_c *g_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, g_ctl->maxlen, label_gravity_ctl[0], g_ctl->gravity_c);
	
	write_chara_real_ctl_list(fp, level, label_gravity_ctl[1], &g_ctl->gravity_vec_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_coriolis_ctl_c(struct coriolis_ctl_c *cor_ctl){
	int i;
	
	cor_ctl->maxlen = 0;
	for (i=0;i<NLBL_CORIOLIS_CTL;i++){
		if(strlen(label_coriolis_ctl[i]) > cor_ctl->maxlen){
			cor_ctl->maxlen = (int) strlen(label_coriolis_ctl[i]);
		};
	};
	
	init_chara_real_ctl_list(&cor_ctl->system_rotation_list);
	
	return;
};

void dealloc_coriolis_ctl_c(struct coriolis_ctl_c *cor_ctl){
	
	clear_chara_real_ctl_list(&cor_ctl->system_rotation_list);
	
	return;
};

int read_coriolis_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct coriolis_ctl_c *cor_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_ctl_list(fp, buf, label_coriolis_ctl[ 0], &cor_ctl->system_rotation_list);
	};
	return 1;
};

int write_coriolis_ctl_c(FILE *fp, int level, const char *label, 
                         struct coriolis_ctl_c *cor_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_real_ctl_list(fp, level, label_coriolis_ctl[0], &cor_ctl->system_rotation_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_magneto_cv_ctl_c(struct magneto_cv_ctl_c *mcv_ctl){
	int i;
	
	mcv_ctl->maxlen = 0;
	for (i=0;i<NLBL_MAGNETO_CV_CTL;i++){
		if(strlen(label_magneto_cv_ctl[i]) > mcv_ctl->maxlen){
			mcv_ctl->maxlen = (int) strlen(label_magneto_cv_ctl[i]);
		};
	};
	
	init_chara_real_ctl_list(&mcv_ctl->ext_magne_list);
	mcv_ctl->magneto_cv_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(mcv_ctl->magneto_cv_c);
	
	return;
};

void dealloc_magneto_cv_ctl_c(struct magneto_cv_ctl_c *mcv_ctl){
	
	clear_chara_real_ctl_list(&mcv_ctl->ext_magne_list);
	
	dealloc_chara_ctl_item_c(mcv_ctl->magneto_cv_c);
	free(mcv_ctl->magneto_cv_c);
	
	return;
};

int read_magneto_cv_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct magneto_cv_ctl_c *mcv_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_magneto_cv_ctl[ 0], mcv_ctl->magneto_cv_c);
		read_chara_real_ctl_list(fp, buf, label_magneto_cv_ctl[ 1], &mcv_ctl->ext_magne_list);
	};
	return 1;
};

int write_magneto_cv_ctl_c(FILE *fp, int level, const char *label, 
                           struct magneto_cv_ctl_c *mcv_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, mcv_ctl->maxlen, label_magneto_cv_ctl[0], mcv_ctl->magneto_cv_c);
	
	write_chara_real_ctl_list(fp, level, label_magneto_cv_ctl[1], &mcv_ctl->ext_magne_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
