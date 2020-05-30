/*
//  t_control_data_PSF_ctl_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_PSF_ctl_list_h_
#define t_control_data_PSF_ctl_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_4_psf_c.h"

struct sectioning_ctl_c{
	char *fname_psf_ctl;
	int iflag_psf_ctl;
	struct psf_ctl_c *psf_c;
};

struct PSF_ctl_list{
	struct sectioning_ctl_c *sections_c;
	
	struct PSF_ctl_list *_prev;
	struct PSF_ctl_list *_next;
};

/* prototypes */

struct sectioning_ctl_c * init_sectioning_ctl_c();
void dealloc_sectioning_ctl_c(struct sectioning_ctl_c *sections_c);
int read_sectioning_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct sectioning_ctl_c *sections_c);
int write_sectioning_ctl_c(FILE *fp, int level, const char *label, 
			struct sectioning_ctl_c *sections_c);
void read_sectioning_ctl_file_c(char buf[LENGTHBUF], struct sectioning_ctl_c *sections_c);
void write_sectioning_ctl_file_c(struct sectioning_ctl_c *sections_c);


void init_PSF_ctl_list(struct PSF_ctl_list *head);
void clear_PSF_ctl_list(struct PSF_ctl_list *head);
struct PSF_ctl_list *add_PSF_ctl_list_after(struct PSF_ctl_list *current);
void delete_PSF_ctl_list(struct PSF_ctl_list *current);
int count_PSF_ctl_list(struct PSF_ctl_list *head);
struct PSF_ctl_list *set_PSF_ctl_list_pointer(int index, struct PSF_ctl_list *head);

void rename_PSF_subfile_list(struct PSF_ctl_list *head);
void read_PSF_subfile_list(char buf[LENGTHBUF], struct PSF_ctl_list *head);
void write_PSF_subfile_list(struct PSF_ctl_list *head);

int read_PSF_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct PSF_ctl_list *head);
int write_PSF_ctl_list(FILE *fp, int level, const char *label, 
			struct PSF_ctl_list *head);

#endif /* t_control_data_PSF_ctl_list_h_ */

