/*
//  t_control_data_pvr_isosurf_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#ifndef t_control_data_pvr_isosurf_list_h_
#define t_control_data_pvr_isosurf_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "m_PVR_control_labels_from_f.h"

struct pvr_isosurf_ctl_c{
	struct control_labels_f *label_pvr_isosurf;
	
	struct real_ctl_item *iso_value_ctl;
	struct real_ctl_item *opacity_ctl;
	struct chara_ctl_item *isosurf_type_ctl;
};

struct pvr_iso_ctl_list{
	struct pvr_isosurf_ctl_c *pvr_iso_c;
	
	struct pvr_iso_ctl_list *_prev;
	struct pvr_iso_ctl_list *_next;
};

/* prototypes */
struct pvr_isosurf_ctl_c * init_pvr_isosurf_ctl_c();
void dealloc_pvr_isosurf_ctl_c(struct pvr_isosurf_ctl_c *pvr_iso_c);
int read_pvr_isosurf_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct pvr_isosurf_ctl_c *isosurfs_c);
int write_pvr_isosurf_ctl_c(FILE *fp, int level, const char *label, 
			struct pvr_isosurf_ctl_c *isosurfs_c);


void init_pvr_iso_ctl_list(struct pvr_iso_ctl_list *head);
void clear_pvr_iso_ctl_list(struct pvr_iso_ctl_list *head);
struct pvr_iso_ctl_list *add_pvr_iso_ctl_list_after(struct pvr_iso_ctl_list *current);
void delete_pvr_iso_ctl_list(struct pvr_iso_ctl_list *current);
int count_pvr_iso_ctl_list(struct pvr_iso_ctl_list *head);
struct pvr_iso_ctl_list *set_pvr_iso_ctl_list_pointer(int index, struct pvr_iso_ctl_list *head);

int read_pvr_iso_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct pvr_iso_ctl_list *head);
int write_pvr_iso_ctl_list(FILE *fp, int level, const char *label, 
			struct pvr_iso_ctl_list *head);

#endif /* t_control_data_pvr_isosurf_list_h_ */
