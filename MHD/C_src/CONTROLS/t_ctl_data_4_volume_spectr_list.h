/*
//  t_ctl_data_4_volume_spectr_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#ifndef t_ctl_data_4_volume_spectr_list_h_
#define t_ctl_data_4_volume_spectr_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

struct volume_spectr_control_c{
    int maxlen;
    
    struct chara_ctl_item *volume_spec_file_c;
    struct chara_ctl_item *volume_ave_file_c;
	
    struct real_ctl_item *inner_radius_c;
    struct real_ctl_item *outer_radius_c;
};

struct volume_spectr_ctl_list{
	struct volume_spectr_control_c *v_pwr_c;
	
	struct volume_spectr_ctl_list *_prev;
	struct volume_spectr_ctl_list *_next;
};

/* prototypes */
void get_label_volume_spectr_ctl(int index, char *label);

struct volume_spectr_control_c * init_volume_spectr_control_c();
void dealloc_volume_spectr_control_c(struct volume_spectr_control_c *v_pwr_c);
int read_volume_spectr_control_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct volume_spectr_control_c *v_pwr_c);
int write_volume_spectr_control_c(FILE *fp, int level, const char *label, 
			struct volume_spectr_control_c *v_pwr_c);

void init_sph_vol_spectr_list(struct volume_spectr_ctl_list *head);
void clear_sph_vol_spectr_ctl_list(struct volume_spectr_ctl_list *head);
struct volume_spectr_ctl_list *add_sph_vol_spectr_ctl_list_after(struct volume_spectr_ctl_list *current);
void delete_vol_spectr_ctl_list(struct volume_spectr_ctl_list *current);
int count_vol_spectr_ctl_list(struct volume_spectr_ctl_list *head);
struct volume_spectr_ctl_list *set_sph_vol_spec_ctl_list_pointer(int index, struct volume_spectr_ctl_list *head);

int read_sph_vol_spectr_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct volume_spectr_ctl_list *head);
int write_sph_vol_spectr_ctl_list(FILE *fp, int level, const char *label, 
			struct volume_spectr_ctl_list *head);


#endif /* t_ctl_data_4_volume_spectr_list_h_ */
