/*
//  t_control_data_ISO_ctl_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_ISO_ctl_list_h_
#define t_control_data_ISO_ctl_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_4_iso_c.h"

struct isosurface_ctl_c{
	char *fname_iso_ctl;
	int iflag_iso_ctl;
	struct iso_ctl_c *iso_c;
};

struct ISO_ctl_list{
	struct isosurface_ctl_c *isosurfs_c;
	
	struct ISO_ctl_list *_prev;
	struct ISO_ctl_list *_next;
};

/* prototypes */

struct isosurface_ctl_c * init_isosurface_ctl_c();
void dealloc_isosurface_ctl_c(struct isosurface_ctl_c *isosurfs_c);
int read_isosurface_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct isosurface_ctl_c *isosurfs_c);
int write_isosurface_ctl_c(FILE *fp, int level, const char *label, 
			struct isosurface_ctl_c *isosurfs_c);
void read_isosurface_ctl_file_c(char buf[LENGTHBUF], struct isosurface_ctl_c *isosurfs_c);
void write_isosurface_ctl_file_c(struct isosurface_ctl_c *isosurfs_c);


void init_ISO_ctl_list(struct ISO_ctl_list *head);
void clear_ISO_ctl_list(struct ISO_ctl_list *head);
struct ISO_ctl_list *add_ISO_ctl_list_after(struct ISO_ctl_list *current);
void delete_ISO_ctl_list(struct ISO_ctl_list *current);
int count_ISO_ctl_list(struct ISO_ctl_list *head);
struct ISO_ctl_list *set_ISO_ctl_list_pointer(int index, struct ISO_ctl_list *head);

void rename_ISO_subfile_list(struct ISO_ctl_list *head);
void read_ISO_subfile_list(char buf[LENGTHBUF], struct ISO_ctl_list *head);
void write_ISO_subfile_list(struct ISO_ctl_list *head);

int read_ISO_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct ISO_ctl_list *head);
int write_ISO_ctl_list(FILE *fp, int level, const char *label, 
			struct ISO_ctl_list *head);

#endif /* t_control_data_ISO_ctl_list_h_ */

