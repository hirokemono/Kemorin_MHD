/*
//  t_ctl_data_sph_filter_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#ifndef t_ctl_data_sph_filter_list_h
#define t_ctl_data_sph_filter_list_h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"


struct sph_filter_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *sph_filter_type_c;
	struct chara_ctl_item *radial_filter_type_c;
	struct int_ctl_item *maximum_moments_c;
	struct real_ctl_item *sphere_filter_width_c;
	struct real_ctl_item *radial_filter_width_c;
	
	struct int_ctl_item *first_reference_c;
	struct int_ctl_item *second_reference_c;
};

struct sph_filter_ctl_list{
	struct sph_filter_ctl_c *sph_filter_c;
	
	struct sph_filter_ctl_list *_prev;
	struct sph_filter_ctl_list *_next;
};

/* prototypes */
void get_label_sph_filter_ctl(int index, char *label);

struct sph_filter_ctl_c * init_sph_filter_ctl_c();
void dealloc_sph_filter_ctl_c(struct sph_filter_ctl_c *sph_filter_c);
int read_sph_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sph_filter_ctl_c *sph_filter_c);
int write_sph_filter_ctl_c(FILE *fp, int level, const char *label, 
			struct sph_filter_ctl_c *sph_filter_c);

void init_sph_filter_ctl_list(struct sph_filter_ctl_list *head);
void clear_sph_filter_ctl_list(struct sph_filter_ctl_list *head);
struct sph_filter_ctl_list *add_sph_filter_ctl_list_after(struct sph_filter_ctl_list *current);
void delete_sph_filter_ctl_list(struct sph_filter_ctl_list *current);
int count_sph_filter_ctl_list(struct sph_filter_ctl_list *head);
struct sph_filter_ctl_list *set_sph_filter_ctl_list_pointer(int index, struct sph_filter_ctl_list *head);

int read_sph_filter_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct sph_filter_ctl_list *head);
int write_sph_filter_ctl_list(FILE *fp, int level, const char *label, 
			struct sph_filter_ctl_list *head);


#endif /* t_ctl_data_sph_filter_list_h */
