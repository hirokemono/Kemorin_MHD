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
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_real_IO.h"

#define NLBL_FORCES_CTL      1
#define NLBL_GRAVITY_CTL     2
#define NLBL_CORIOLIS_CTL    1
#define NLBL_MAGNETO_CV_CTL  2


struct forces_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara_clist *force_names_list;
};

struct gravity_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara_ctl_item *gravity_c;
	struct chara_real_clist *gravity_vec_list;
};

struct coriolis_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara_real_clist *system_rotation_list;
};

struct magneto_cv_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara_ctl_item *magneto_cv_c;
	struct chara_real_clist *ext_magne_list;
};

/* prototype */
void get_label_forces_ctl(int index, char *label);
void get_label_gravity_ctl(int index, char *label);
void get_label_coriolis_ctl(int index, char *label);
void get_label_magneto_cv_ctl(int index, char *label);


struct forces_ctl_c * init_forces_ctl_c();
void dealloc_forces_ctl_c(struct forces_ctl_c *frc_ctl);
void read_forces_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct forces_ctl_c *frc_ctl);
int write_forces_ctl_c(FILE *fp, int level,	const char *label,
                       struct forces_ctl_c *frc_ctl);

struct gravity_ctl_c * init_gravity_ctl_c();
void dealloc_gravity_ctl_c(struct gravity_ctl_c *g_ctl);
void read_gravity_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct gravity_ctl_c *g_ctl);
int write_gravity_ctl_c(FILE *fp, int level, const char *label, 
                        struct gravity_ctl_c *g_ctl);

struct coriolis_ctl_c * init_coriolis_ctl_c();
void dealloc_coriolis_ctl_c(struct coriolis_ctl_c *cor_ctl);
void read_coriolis_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct coriolis_ctl_c *cor_ctl);
int write_coriolis_ctl_c(FILE *fp, int level, const char *label,
                         struct coriolis_ctl_c *cor_ctl);

struct magneto_cv_ctl_c * init_magneto_cv_ctl_c();
void dealloc_magneto_cv_ctl_c(struct magneto_cv_ctl_c *mcv_ctl);
void read_magneto_cv_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct magneto_cv_ctl_c *mcv_ctl);
int write_magneto_cv_ctl_c(FILE *fp, int level, const char *label,
                           struct magneto_cv_ctl_c *mcv_ctl);


#endif /* t_ctl_data_mhd_forces_c_h_ */
