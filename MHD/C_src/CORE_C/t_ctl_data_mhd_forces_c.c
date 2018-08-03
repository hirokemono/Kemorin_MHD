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


void alloc_forces_ctl_c(struct forces_ctl_c *frc_ctl){
	int i;
	
	frc_ctl->maxlen = 0;
	for (i=0;i<NLBL_FORCES_CTL;i++){
		if(strlen(label_forces_ctl[i]) > frc_ctl->maxlen){
			frc_ctl->maxlen = strlen(label_forces_ctl[i]);
		};
	};
	
	frc_ctl->force_names_c = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	
	return;
};

void dealloc_forces_ctl_c(struct forces_ctl_c *frc_ctl){
	
	dealloc_ctl_chara_array(frc_ctl->force_names_c);
	free(frc_ctl->force_names_c);
	
	return;
};

int read_forces_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct forces_ctl_c *frc_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_array_c(fp, buf, label_forces_ctl[ 0], frc_ctl->force_names_c);
	};
	return 1;
};

int write_forces_ctl_c(FILE *fp, int level,	const char *label, 
                       struct forces_ctl_c *frc_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_array_c(fp, level, strlen(label_forces_ctl[0]),
				label_forces_ctl[0], frc_ctl->force_names_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_gravity_ctl_c(struct gravity_ctl_c *g_ctl){
	int i;
	
	g_ctl->maxlen = 0;
	for (i=0;i<NLBL_GRAVITY_CTL;i++){
		if(strlen(label_gravity_ctl[i]) > g_ctl->maxlen){
			g_ctl->maxlen = strlen(label_gravity_ctl[i]);
		};
	};
	
	g_ctl->gravity_vec_c = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	g_ctl->gravity_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_ctl_chara_item(g_ctl->gravity_c);
	
	return;
};

void dealloc_gravity_ctl_c(struct gravity_ctl_c *g_ctl){
	
	dealloc_ctl_cr_array(g_ctl->gravity_vec_c);
	free(g_ctl->gravity_vec_c);
	
	dealloc_ctl_chara_item(g_ctl->gravity_c);
	free(g_ctl->gravity_c);
	
	return;
};

int read_gravity_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct gravity_ctl_c *g_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_gravity_ctl[ 0], g_ctl->gravity_c);
		read_cr_ctl_array_c(fp, buf, label_gravity_ctl[ 1], g_ctl->gravity_vec_c);
	};
	return 1;
};

int write_gravity_ctl_c(FILE *fp, int level, const char *label,
                        struct gravity_ctl_c *g_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, g_ctl->maxlen, label_gravity_ctl[0], g_ctl->gravity_c);
	
	if(g_ctl->gravity_vec_c->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, strlen(label_gravity_ctl[1]),
				label_gravity_ctl[1], g_ctl->gravity_vec_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_coriolis_ctl_c(struct coriolis_ctl_c *cor_ctl){
	int i;
	
	cor_ctl->maxlen = 0;
	for (i=0;i<NLBL_CORIOLIS_CTL;i++){
		if(strlen(label_coriolis_ctl[i]) > cor_ctl->maxlen){
			cor_ctl->maxlen = strlen(label_coriolis_ctl[i]);
		};
	};
	
	cor_ctl->system_rotation_c = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	
	return;
};

void dealloc_coriolis_ctl_c(struct coriolis_ctl_c *cor_ctl){
	
	dealloc_ctl_cr_array(cor_ctl->system_rotation_c);
	free(cor_ctl->system_rotation_c);
	
	return;
};

int read_coriolis_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct coriolis_ctl_c *cor_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_cr_ctl_array_c(fp, buf, label_coriolis_ctl[ 0], cor_ctl->system_rotation_c);
	};
	return 1;
};

int write_coriolis_ctl_c(FILE *fp, int level, const char *label, 
                         struct coriolis_ctl_c *cor_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_cr_ctl_array_c(fp, level, strlen(label_coriolis_ctl[0]),
				label_coriolis_ctl[0], cor_ctl->system_rotation_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_magneto_cv_ctl_c(struct magneto_cv_ctl_c *mcv_ctl){
	int i;
	
	mcv_ctl->maxlen = 0;
	for (i=0;i<NLBL_MAGNETO_CV_CTL;i++){
		if(strlen(label_magneto_cv_ctl[i]) > mcv_ctl->maxlen){
			mcv_ctl->maxlen = strlen(label_magneto_cv_ctl[i]);
		};
	};
	
	mcv_ctl->ext_magne_c = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	mcv_ctl->magneto_cv_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	
	alloc_ctl_chara_item(mcv_ctl->magneto_cv_c);
	
	return;
};

void dealloc_magneto_cv_ctl_c(struct magneto_cv_ctl_c *mcv_ctl){
	
	dealloc_ctl_cr_array(mcv_ctl->ext_magne_c);
	free(mcv_ctl->ext_magne_c);
	
	dealloc_ctl_chara_item(mcv_ctl->magneto_cv_c);
	free(mcv_ctl->magneto_cv_c);
	
	return;
};

int read_magneto_cv_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct magneto_cv_ctl_c *mcv_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_magneto_cv_ctl[ 0], mcv_ctl->magneto_cv_c);
		read_cr_ctl_array_c(fp, buf, label_magneto_cv_ctl[ 1], mcv_ctl->ext_magne_c);
	};
	return 1;
};

int write_magneto_cv_ctl_c(FILE *fp, int level, const char *label, 
                           struct magneto_cv_ctl_c *mcv_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, mcv_ctl->maxlen, label_magneto_cv_ctl[0], mcv_ctl->magneto_cv_c);
	
	if(mcv_ctl->ext_magne_c->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, strlen(label_magneto_cv_ctl[1]),
				label_magneto_cv_ctl[1], mcv_ctl->ext_magne_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
