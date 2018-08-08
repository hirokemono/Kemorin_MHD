/*
//  t_control_data_lic_ctl_list.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_lic_ctl_list_h_
#define t_control_data_lic_ctl_list_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_LIC_pvr_c.h"

struct LIC_rendering_ctl_c{
	char *fname_lic_pvr_ctl;
	int iflag_lic_pvr_ctl;
	struct LIC_pvr_ctl_c *lic_pvr_c;
};

struct LIC_PVR_ctl_list{
	struct LIC_rendering_ctl_c *lic_render_c;
	
	struct LIC_PVR_ctl_list *_prev;
	struct LIC_PVR_ctl_list *_next;
};

/* prototypes */

void alloc_LIC_rendering_ctl_c(struct LIC_rendering_ctl_c *lic_render_c);
void dealloc_LIC_renderingctl_c(struct LIC_rendering_ctl_c *lic_render_c);
int read_LIC_rendering_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_rendering_ctl_c *lic_render_c);
int write_LIC_rendering_ctl_c(FILE *fp, int level, const char *label, 
			struct LIC_rendering_ctl_c *lic_render_c);
void read_LIC_rendering_ctl_file_c(char buf[LENGTHBUF], struct LIC_rendering_ctl_c *lic_render_c);
void write_LIC_rendering_ctl_file_c(struct LIC_rendering_ctl_c *lic_render_c);


int init_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *head);
int clear_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *head);
LIC_PVR_ctl_list *add_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current);
void delete_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current);
int count_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current)；

int read_LIC_PVR_ctl_list(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_PVR_ctl_list *current){
int write_LIC_PVR_ctl_list(FILE *fp, int level, const char *label, 
			struct LIC_PVR_ctl_list *current);

#endif /* t_control_data_lic_ctl_list_h_ */

