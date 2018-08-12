/*
//  t_ctl_data_mhd_forces_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_mhd_forces_c_h_
#define t_ctl_data_mhd_forces_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_control_chara_real_IO.h"

struct forces_ctl_c{
	int maxlen;
	
	struct chara_ctl_array *force_names_c;
};

struct gravity_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *gravity_c;
	struct chara_real_ctl_list gravity_vec_list;
};

struct coriolis_ctl_c{
	int maxlen;
	
	struct chara_real_ctl_list system_rotation_list;
};

struct magneto_cv_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *magneto_cv_c;
	struct chara_real_ctl_list ext_magne_list;
};

/* prototype */
void get_label_forces_ctl(int index, char *label);
void get_label_gravity_ctl(int index, char *label);
void get_label_coriolis_ctl(int index, char *label);
void get_label_magneto_cv_ctl(int index, char *label);


void alloc_forces_ctl_c(struct forces_ctl_c *frc_ctl);
void dealloc_forces_ctl_c(struct forces_ctl_c *frc_ctl);
int read_forces_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct forces_ctl_c *frc_ctl);
int write_forces_ctl_c(FILE *fp, int level,	const char *label,
                       struct forces_ctl_c *frc_ctl);

void alloc_gravity_ctl_c(struct gravity_ctl_c *g_ctl);
void dealloc_gravity_ctl_c(struct gravity_ctl_c *g_ctl);
int read_gravity_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct gravity_ctl_c *g_ctl);
int write_gravity_ctl_c(FILE *fp, int level, const char *label, 
                        struct gravity_ctl_c *g_ctl);

void alloc_coriolis_ctl_c(struct coriolis_ctl_c *cor_ctl);
void dealloc_coriolis_ctl_c(struct coriolis_ctl_c *cor_ctl);
int read_coriolis_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct coriolis_ctl_c *cor_ctl);
int write_coriolis_ctl_c(FILE *fp, int level, const char *label,
                         struct coriolis_ctl_c *cor_ctl);

void alloc_magneto_cv_ctl_c(struct magneto_cv_ctl_c *mcv_ctl);
void dealloc_magneto_cv_ctl_c(struct magneto_cv_ctl_c *mcv_ctl);
int read_magneto_cv_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct magneto_cv_ctl_c *mcv_ctl);
int write_magneto_cv_ctl_c(FILE *fp, int level, const char *label,
                           struct magneto_cv_ctl_c *mcv_ctl);


#endif /* t_ctl_data_mhd_forces_c_h_ */
