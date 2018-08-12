/*
//  t_ctl_data_mhd_evolution_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_mhd_evolution_c.h"

#define NLBL_MHD_EVOLUTION_CTL  1
#define NLBL_MHD_EVO_AREA_CTL   2

const char label_mhd_evolution_ctl[NLBL_MHD_EVOLUTION_CTL][KCHARA_C] = {
	/*[ 0]*/	{"time_evo_ctl"}
};

const char label_mhd_evo_area_ctl[NLBL_MHD_EVO_AREA_CTL][KCHARA_C] = {
	/*[ 0]*/	{"fluid_ele_grp"},
	/*[ 1]*/	{"conduct_ele_grp"}
};


void get_label_mhd_evolution_ctl(int index, char *label){
    if(index < NLBL_MHD_EVOLUTION_CTL) strngcopy(label, label_mhd_evolution_ctl[index]);
    return;
};
void get_label_mhd_evo_area_ctl(int index, char *label){
    if(index < NLBL_MHD_EVO_AREA_CTL) strngcopy(label, label_mhd_evo_area_ctl[index]);
    return;
};


void alloc_mhd_evolution_ctl_c(struct mhd_evolution_ctl_c *evo_ctl){
	int i;
	
	evo_ctl->maxlen = 0;
	for (i=0;i<NLBL_MHD_EVOLUTION_CTL;i++){
		if(strlen(label_mhd_evolution_ctl[i]) > evo_ctl->maxlen){
			evo_ctl->maxlen = (int) strlen(label_mhd_evolution_ctl[i]);
		};
	};
	
	init_chara_ctl_list(&evo_ctl->t_evo_field_list);	
	return;
};

void dealloc_mhd_evolution_ctl_c(struct mhd_evolution_ctl_c *evo_ctl){
	clear_chara_ctl_list(&evo_ctl->t_evo_field_list);	
	return;
};

int read_mhd_evolution_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evolution_ctl_c *evo_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_list(fp, buf, label_mhd_evolution_ctl[ 0], &evo_ctl->t_evo_field_list);
	};
	return 1;
};

int write_mhd_evolution_ctl_c(FILE *fp, int level,
			const char *label, struct mhd_evolution_ctl_c *evo_ctl){

    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_list(fp, level, label_mhd_evolution_ctl[0], &evo_ctl->t_evo_field_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_mhd_evo_area_ctl_c(struct mhd_evo_area_ctl_c *earea_ctl){
	int i;
	
	earea_ctl->maxlen = 0;
	for (i=0;i<NLBL_MHD_EVO_AREA_CTL;i++){
		if(strlen(label_mhd_evo_area_ctl[i]) > earea_ctl->maxlen){
			earea_ctl->maxlen = (int) strlen(label_mhd_evo_area_ctl[i]);
		};
	};
	
	init_chara_ctl_list(&earea_ctl->evo_fluid_group_list);
	init_chara_ctl_list(&earea_ctl->evo_conduct_group_list);
	
	return;
};

void dealloc_mhd_evo_area_ctl_c(struct mhd_evo_area_ctl_c *earea_ctl){
	clear_chara_ctl_list(&earea_ctl->evo_fluid_group_list);
	clear_chara_ctl_list(&earea_ctl->evo_conduct_group_list);
	return;
};

int read_mhd_evo_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evo_area_ctl_c *earea_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_list(fp, buf, label_mhd_evo_area_ctl[ 0], &earea_ctl->evo_fluid_group_list);
		read_chara_ctl_list(fp, buf, label_mhd_evo_area_ctl[ 1], &earea_ctl->evo_conduct_group_list);
	};
	return 1;
};

int write_mhd_evo_area_ctl_c(FILE *fp, int level, const char *label, 
                             struct mhd_evo_area_ctl_c *earea_ctl){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_list(fp, level, label_mhd_evo_area_ctl[0], &earea_ctl->evo_fluid_group_list);
	write_chara_ctl_list(fp, level, label_mhd_evo_area_ctl[1], &earea_ctl->evo_conduct_group_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
