/*
//  t_control_data_lic_masking_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#ifndef t_control_data_lic_masking_list_h_
#define t_control_data_lic_masking_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_IO.h"
#include "t_control_real2_IO.h"

struct lic_masking_ctl_c{
	struct label_list_f *label_lic_masking;
	
	struct chara_ctl_item *masking_type_ctl;
	struct chara_ctl_item *field_name_ctl;
	struct chara_ctl_item *component_ctl;
	
	struct real2_clist *mask_range_list;
};

struct lic_masking_ctl_list{
	struct lic_masking_ctl_c *lic_mask_c;
	
	struct lic_masking_ctl_list *_prev;
	struct lic_masking_ctl_list *_next;
};

/* prototypes */

struct lic_masking_ctl_c * init_lic_masking_ctl_c();
void dealloc_lic_masking_ctl_c(struct lic_masking_ctl_c *mask_ctl);
int read_lic_masking_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_masking_ctl_c *mask_ctl);
int write_lic_masking_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_masking_ctl_c *mask_ctl);

void init_lic_masking_ctl_list(struct lic_masking_ctl_list *head);
void clear_lic_masking_ctl_list(struct lic_masking_ctl_list *head);
struct lic_masking_ctl_list *add_lic_masking_ctl_list_after(struct lic_masking_ctl_list *current);
void delete_lic_masking_ctl_list(struct lic_masking_ctl_list *current);
int count_lic_masking_ctl_list(struct lic_masking_ctl_list *head);
struct lic_masking_ctl_list *set_lic_masking_ctl_list_pointer(int index, struct lic_masking_ctl_list *head);

int read_lic_masking_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct lic_masking_ctl_list *head);
int write_lic_masking_ctl_list(FILE *fp, int level, const char *label, 
			struct lic_masking_ctl_list *head);

#endif /* t_control_data_lic_masking_list_h_ */
