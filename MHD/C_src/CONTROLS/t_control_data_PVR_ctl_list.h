/*
//  t_control_data_PVR_ctl_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_PVR_ctl_list_h_
#define t_control_data_PVR_ctl_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_4_pvr_c.h"

struct volume_rendering_ctl_c{
	char *fname_pvr_ctl;
	int iflag_pvr_ctl;
	struct pvr_ctl_c *pvr_c;
};

struct PVR_ctl_list{
	struct volume_rendering_ctl_c *v_render_c;
	
	struct PVR_ctl_list *_prev;
	struct PVR_ctl_list *_next;
};

/* prototypes */

struct volume_rendering_ctl_c * init_volume_rendering_ctl_c();
void dealloc_volume_rendering_ctl_c(struct volume_rendering_ctl_c *v_render_c);
int read_volume_rendering_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct volume_rendering_ctl_c *v_render_c);
int write_volume_rendering_ctl_c(FILE *fp, int level, const char *label, 
			struct volume_rendering_ctl_c *v_render_c);
void read_volume_rendering_ctl_file_c(char buf[LENGTHBUF], struct volume_rendering_ctl_c *v_render_c);
void write_volume_rendering_ctl_file_c(struct volume_rendering_ctl_c *v_render_c);


void init_PVR_ctl_list(struct PVR_ctl_list *head);
void clear_PVR_ctl_list(struct PVR_ctl_list *head);
struct PVR_ctl_list *add_PVR_ctl_list_after(struct PVR_ctl_list *current);
void delete_PVR_ctl_list(struct PVR_ctl_list *current);
int count_PVR_ctl_list(struct PVR_ctl_list *head);
struct PVR_ctl_list *set_PVR_ctl_list_pointer(int index, struct PVR_ctl_list *head);

void rename_PVR_subfile_list(struct PVR_ctl_list *head);
void read_PVR_subfile_list(char buf[LENGTHBUF], struct PVR_ctl_list *head);
void write_PVR_subfile_list(struct PVR_ctl_list *head);

int read_PVR_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct PVR_ctl_list *head);
int write_PVR_ctl_list(FILE *fp, int level, const char *label, 
			struct PVR_ctl_list *head);

#endif /* t_control_data_PVR_ctl_list_h_ */

