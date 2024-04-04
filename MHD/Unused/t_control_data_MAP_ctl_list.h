/*
//  t_control_data_MAP_ctl_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_MAP_ctl_list_h_
#define t_control_data_MAP_ctl_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_4_psf_c.h"

struct map_rendering_ctl_c{
	char *fname_psf_ctl;
	int iflag_psf_ctl;
	struct psf_ctl_c *psf_c;
};

struct MAP_ctl_list{
	struct map_rendering_ctl_c *sections_c;
	
	struct MAP_ctl_list *_prev;
	struct MAP_ctl_list *_next;
};

/* prototypes */

struct map_rendering_ctl_c * init_map_rendering_ctl_c();
void dealloc_map_rendering_ctl_c(struct map_rendering_ctl_c *sections_c);
int read_map_rendering_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct map_rendering_ctl_c *sections_c);
int write_map_rendering_ctl_c(FILE *fp, int level, const char *label, 
			struct map_rendering_ctl_c *sections_c);
void read_map_rendering_ctl_file_c(char buf[LENGTHBUF], struct map_rendering_ctl_c *sections_c);
void write_map_rendering_ctl_file_c(struct map_rendering_ctl_c *sections_c);


void init_MAP_ctl_list(struct MAP_ctl_list *head);
void clear_MAP_ctl_list(struct MAP_ctl_list *head);
struct MAP_ctl_list *add_MAP_ctl_list_after(struct MAP_ctl_list *current);
void delete_MAP_ctl_list(struct MAP_ctl_list *current);
int count_MAP_ctl_list(struct MAP_ctl_list *head);
struct MAP_ctl_list *set_MAP_ctl_list_pointer(int index, struct MAP_ctl_list *head);

void rename_MAP_subfile_list(struct MAP_ctl_list *head);
void read_MAP_subfile_list(char buf[LENGTHBUF], struct MAP_ctl_list *head);
void write_MAP_subfile_list(struct MAP_ctl_list *head);

int read_MAP_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct MAP_ctl_list *head);
int write_MAP_ctl_list(FILE *fp, int level, const char *label, 
			struct MAP_ctl_list *head);

#endif /* t_control_data_MAP_ctl_list_h_ */

