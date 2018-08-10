/*
//  t_control_data_LIC_pvr_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/06.
*/

#ifndef t_control_data_LIC_pvr_c_h_
#define t_control_data_LIC_pvr_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_control_data_4_psf_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"
#include "t_control_data_4_pvr_c.h"
#include "t_control_data_LIC_c.h"

struct LIC_pvr_ctl_c{
	int maxlen;
	
	struct pvr_ctl_c *pvr_c;
	
	int iflag_lic_ctl;
	struct lic_ctl_c *lic_c;
};

/* prototypes */
void get_label_LIC_pvr_ctl(int index, char *label);

void alloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c);
void dealloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c);
int read_LIC_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c);
int write_LIC_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c);


void rename_LIC_pvr_ctl_subfiles(struct LIC_pvr_ctl_c *lic_pvr_c);
int read_LIC_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct LIC_pvr_ctl_c *lic_pvr_c);
int write_LIC_pvr_ctl_file_c(const char *file_name, struct LIC_pvr_ctl_c *lic_pvr_c);


#endif /* t_control_data_LIC_pvr_c_h_ */
