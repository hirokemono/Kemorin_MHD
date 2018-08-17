/*
//  t_ctl_data_mhd_evolution_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_mhd_evolution_c_h_
#define t_ctl_data_mhd_evolution_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_IO.h"


struct mhd_evolution_ctl_c{
	int maxlen;
	
	struct chara_clist *t_evo_field_list;
};

struct mhd_evo_area_ctl_c{
	int maxlen;
	
	struct chara_clist *evo_fluid_group_list;
	struct chara_clist *evo_conduct_group_list;
};


/* prototype */
void get_label_mhd_evolution_ctl(int index, char *label);
void get_label_mhd_evo_area_ctl(int index, char *label);

void alloc_mhd_evolution_ctl_c(struct mhd_evolution_ctl_c *evo_ctl);
void dealloc_mhd_evolution_ctl_c(struct mhd_evolution_ctl_c *evo_ctl);
int read_mhd_evolution_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evolution_ctl_c *evo_ctl);
int write_mhd_evolution_ctl_c(FILE *fp, int level,
			const char *label, struct mhd_evolution_ctl_c *evo_ctl);

void alloc_mhd_evo_area_ctl_c(struct mhd_evo_area_ctl_c *earea_ctl);
void dealloc_mhd_evo_area_ctl_c(struct mhd_evo_area_ctl_c *earea_ctl);
int read_mhd_evo_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct mhd_evo_area_ctl_c *earea_ctl);
int write_mhd_evo_area_ctl_c(FILE *fp, int level, const char *label, 
                             struct mhd_evo_area_ctl_c *earea_ctl);

#endif /* t_ctl_data_mhd_evolution_c_h_ */
