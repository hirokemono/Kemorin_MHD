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


struct mhd_evolution_ctl_c * init_mhd_evolution_ctl_c(){
	int i;
    struct mhd_evolution_ctl_c *evo_ctl;
    if((evo_ctl = (struct mhd_evolution_ctl_c *) malloc(sizeof(struct mhd_evolution_ctl_c))) == NULL) {
        printf("malloc error for mhd_evolution_ctl_c \n");
        exit(0);
    }
	
    evo_ctl->iflag_use = 0;
	evo_ctl->maxlen = 0;
	for (i=0;i<NLBL_MHD_EVOLUTION_CTL;i++){
		if(strlen(label_mhd_evolution_ctl[i]) > evo_ctl->maxlen){
			evo_ctl->maxlen = (int) strlen(label_mhd_evolution_ctl[i]);
		};
	};
	
    evo_ctl->t_evo_field_list = init_chara_clist();	
	return evo_ctl;
};

void dealloc_mhd_evolution_ctl_c(struct mhd_evolution_ctl_c *evo_ctl){
	dealloc_chara_clist(evo_ctl->t_evo_field_list);
    free(evo_ctl);
	return;
};

void read_mhd_evolution_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evolution_ctl_c *evo_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_clist(fp, buf, label_mhd_evolution_ctl[ 0], evo_ctl->t_evo_field_list);
	};
    evo_ctl->iflag_use = 1;
	return;
};

int write_mhd_evolution_ctl_c(FILE *fp, int level,
			const char *label, struct mhd_evolution_ctl_c *evo_ctl){
    if(evo_ctl->iflag_use == 0) return level;

    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_clist(fp, level, label_mhd_evolution_ctl[0], evo_ctl->t_evo_field_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct mhd_evo_area_ctl_c * init_mhd_evo_area_ctl_c(){
	int i;
    struct mhd_evo_area_ctl_c *earea_ctl;
    if((earea_ctl = (struct mhd_evo_area_ctl_c *) malloc(sizeof(struct mhd_evo_area_ctl_c))) == NULL) {
        printf("malloc error for mhd_evo_area_ctl_c \n");
        exit(0);
    }
    
    earea_ctl->iflag_use = 0;
	earea_ctl->maxlen = 0;
	for (i=0;i<NLBL_MHD_EVO_AREA_CTL;i++){
		if(strlen(label_mhd_evo_area_ctl[i]) > earea_ctl->maxlen){
			earea_ctl->maxlen = (int) strlen(label_mhd_evo_area_ctl[i]);
		};
	};
	
    earea_ctl->evo_fluid_group_list =   init_chara_clist();
    earea_ctl->evo_conduct_group_list = init_chara_clist();
	
	return earea_ctl;
};

void dealloc_mhd_evo_area_ctl_c(struct mhd_evo_area_ctl_c *earea_ctl){
	dealloc_chara_clist(earea_ctl->evo_fluid_group_list);
	dealloc_chara_clist(earea_ctl->evo_conduct_group_list);
    free(earea_ctl);
    return;
};

void read_mhd_evo_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evo_area_ctl_c *earea_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_clist(fp, buf, label_mhd_evo_area_ctl[ 0], earea_ctl->evo_fluid_group_list);
		read_chara_clist(fp, buf, label_mhd_evo_area_ctl[ 1], earea_ctl->evo_conduct_group_list);
	};
    earea_ctl->iflag_use = 1;
	return;
};

int write_mhd_evo_area_ctl_c(FILE *fp, int level, const char *label, 
                             struct mhd_evo_area_ctl_c *earea_ctl){
    if(earea_ctl->iflag_use == 0) return level;
	
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_clist(fp, level, label_mhd_evo_area_ctl[0], earea_ctl->evo_fluid_group_list);
	write_chara_clist(fp, level, label_mhd_evo_area_ctl[1], earea_ctl->evo_conduct_group_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
