/*
//  t_control_data_vizs_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_vizs_c_h_
#define t_control_data_vizs_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_PSF_ctl_list.h"
#include "t_control_data_ISO_ctl_list.h"
#include "t_control_data_FLINE_ctl_list.h"
#include "t_control_data_PVR_ctl_list.h"
#include "t_control_data_LIC_ctl_list.h"


struct visualizers_ctl_c{
    int iflag_use;
    int maxlen;
    
    struct PSF_ctl_list psf_ctl_list;
    struct ISO_ctl_list iso_ctl_list;
    struct PVR_ctl_list pvr_ctl_list;
    struct LIC_PVR_ctl_list lic_ctl_list;
    struct FLINE_ctl_list fline_ctl_list;
};

/* prototypes */
void get_label_viz_ctl(int index, char *label);

struct visualizers_ctl_c * init_vizs_ctl_c();
void dealloc_vizs_ctl_c(struct visualizers_ctl_c *viz_c);
void read_vizs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c);
int write_vizs_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c);

void rename_vizs_ctl_subfiles(struct visualizers_ctl_c *viz_c);
void read_vizs_ctl_files_c(char buf[LENGTHBUF], struct visualizers_ctl_c *viz_c);
void write_vizs_ctl_files_c(struct visualizers_ctl_c *viz_c);

#endif /* t_control_data_vizs_c_h_ */
