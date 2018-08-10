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
	
	evo_ctl->t_evo_field_c = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	init_ctl_chara_array(evo_ctl->t_evo_field_c);
	
	return;
};

void dealloc_mhd_evolution_ctl_c(struct mhd_evolution_ctl_c *evo_ctl){
	
	dealloc_ctl_chara_array(evo_ctl->t_evo_field_c);
	free(evo_ctl->t_evo_field_c);
	
	return;
};

int read_mhd_evolution_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evolution_ctl_c *evo_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_character_ctl_array_c(fp, buf, label_mhd_evolution_ctl[ 0], evo_ctl->t_evo_field_c);
	};
	return 1;
};

int write_mhd_evolution_ctl_c(FILE *fp, int level,
			const char *label, struct mhd_evolution_ctl_c *evo_ctl){

    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_array_c(fp, level, (int) strlen(label_mhd_evolution_ctl[0]),
				label_mhd_evolution_ctl[0], evo_ctl->t_evo_field_c);
	
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
	
	earea_ctl->evo_fluid_group_c = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	earea_ctl->evo_conduct_group_c = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	init_ctl_chara_array(earea_ctl->evo_fluid_group_c);
	init_ctl_chara_array(earea_ctl->evo_conduct_group_c);
	
	return;
};

void dealloc_mhd_evo_area_ctl_c(struct mhd_evo_area_ctl_c *earea_ctl){
	
	dealloc_ctl_chara_array(earea_ctl->evo_fluid_group_c);
	dealloc_ctl_chara_array(earea_ctl->evo_conduct_group_c);
	free(earea_ctl->evo_fluid_group_c);
	free(earea_ctl->evo_conduct_group_c);
	
	return;
};

int read_mhd_evo_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evo_area_ctl_c *earea_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_character_ctl_array_c(fp, buf, label_mhd_evo_area_ctl[ 0], earea_ctl->evo_fluid_group_c);
		read_character_ctl_array_c(fp, buf, label_mhd_evo_area_ctl[ 1], earea_ctl->evo_conduct_group_c);
	};
	return 1;
};

int write_mhd_evo_area_ctl_c(FILE *fp, int level, const char *label, 
                             struct mhd_evo_area_ctl_c *earea_ctl){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_array_c(fp, level, (int) strlen(label_mhd_evo_area_ctl[0]),
				label_mhd_evo_area_ctl[0], earea_ctl->evo_fluid_group_c);
	
	if(earea_ctl->evo_conduct_group_c->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, (int) strlen(label_mhd_evo_area_ctl[1]),
				label_mhd_evo_area_ctl[1], earea_ctl->evo_conduct_group_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
