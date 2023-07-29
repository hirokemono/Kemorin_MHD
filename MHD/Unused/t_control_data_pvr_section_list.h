/*
//  t_control_data_pvr_section_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#ifndef t_control_data_pvr_section_list_h_
#define t_control_data_pvr_section_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_real_IO.h"
#include "t_control_data_4_psf_c.h"
#include "m_PVR_control_labels_from_f.h"
#include "m_PSF_control_labels_from_f.h"

struct pvr_section_ctl_c{
	char *fname_sect_ctl;
	int iflag_psf_define_ctl;
	struct control_labels_f *label_pvr_section;
    struct control_labels_f *label_psf_ctl;
	
	struct psf_define_ctl_c *psf_def_c;
	struct real_ctl_item *opacity_ctl;
};

struct pvr_sect_ctl_list{
	struct pvr_section_ctl_c *pvr_sect_c;
	
	struct pvr_sect_ctl_list *_prev;
	struct pvr_sect_ctl_list *_next;
};

/* prototypes */
struct pvr_section_ctl_c * init_pvr_section_ctl_c();
void dealloc_pvr_section_ctl_c(struct pvr_section_ctl_c *pvr_sect_c);
int read_pvr_section_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct pvr_section_ctl_c *pvr_sect_c);
int write_pvr_section_ctl_c(FILE *fp, int level, const char *label, 
			struct pvr_section_ctl_c *pvr_sect_c);
void read_pvr_section_subfile_c(char buf[LENGTHBUF], struct pvr_section_ctl_c *pvr_sect_c);
void write_pvr_section_subfile_c(struct pvr_section_ctl_c *pvr_sect_c);


void init_pvr_section_ctl_list(struct pvr_sect_ctl_list *head);
void clear_pvr_section_ctl_list(struct pvr_sect_ctl_list *head);
struct pvr_sect_ctl_list *add_pvr_section_ctl_list_after(struct pvr_sect_ctl_list *current);
void delete_pvr_section_ctl_list(struct pvr_sect_ctl_list *current);
int count_pvr_section_ctl_list(struct pvr_sect_ctl_list *head);
struct pvr_sect_ctl_list *set_pvr_section_ctl_list_pointer(int index, struct pvr_sect_ctl_list *head);

void rename_pvr_section_subfile_list(struct pvr_sect_ctl_list *head);
void read_pvr_section_subfile_list(char buf[LENGTHBUF], struct pvr_sect_ctl_list *head);
void write_pvr_section_subfile_list(struct pvr_sect_ctl_list *head);

int read_pvr_section_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct pvr_sect_ctl_list *head);
int write_pvr_section_ctl_list(FILE *fp, int level, const char *label, 
			struct pvr_sect_ctl_list *head);



#endif /* t_control_data_pvr_section_list_h_ */
