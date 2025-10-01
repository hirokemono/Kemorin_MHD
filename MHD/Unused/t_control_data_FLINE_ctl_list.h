/*
//  t_control_data_FLINE_ctl_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#ifndef t_control_data_FLINE_ctl_list_h_
#define t_control_data_FLINE_ctl_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_4_fline_c.h"

struct fieldline_ctl_c{
	char *fname_fline_ctl;
	int iflag_fline_ctl;
	struct fline_ctl_c *fline_c;
};

struct FLINE_ctl_list{
	struct fieldline_ctl_c *fldlines_c;
	
	struct FLINE_ctl_list *_prev;
	struct FLINE_ctl_list *_next;
};

/* prototypes */

struct fieldline_ctl_c * init_fieldline_ctl_c();
void dealloc_fieldline_ctl_c(struct fieldline_ctl_c *fldlines_c);
int read_fieldline_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct fieldline_ctl_c *fldlines_c);
int write_fieldline_ctl_c(FILE *fp, int level, const char *label, 
			struct fieldline_ctl_c *fldlines_c);
void read_fieldline_ctl_file_c(char buf[LENGTHBUF], struct fieldline_ctl_c *fldlines_c);
void write_fieldline_ctl_file_c(struct fieldline_ctl_c *fldlines_c);


void init_FLINE_ctl_list(struct FLINE_ctl_list *head);
void clear_FLINE_ctl_list(struct FLINE_ctl_list *head);
struct FLINE_ctl_list *add_FLINE_ctl_list_after(struct FLINE_ctl_list *current);
void delete_FLINE_ctl_list(struct FLINE_ctl_list *current);
int count_FLINE_ctl_list(struct FLINE_ctl_list *head);
struct FLINE_ctl_list *set_FLINE_ctl_list_pointer(int index, struct FLINE_ctl_list *head);

void rename_FLINE_subfile_list(struct FLINE_ctl_list *head);
void read_FLINE_subfile_list(char buf[LENGTHBUF], struct FLINE_ctl_list *head);
void write_FLINE_subfile_list(struct FLINE_ctl_list *head);

int read_FLINE_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct FLINE_ctl_list *head);
int write_FLINE_ctl_list(FILE *fp, int level, const char *label, 
			struct FLINE_ctl_list *head);

#endif /* t_control_data_FLINE_ctl_list_h_ */
