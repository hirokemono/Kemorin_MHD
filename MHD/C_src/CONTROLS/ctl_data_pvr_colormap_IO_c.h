/*
//  ctl_data_pvr_colormap_IO_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef CTL_DATA_PVR_COLORMAP_IO_C_H_
#define CTL_DATA_PVR_COLORMAP_IO_C_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_ctl_data_pvr_colormap_c.h"

/* prototypes */
void read_colormap_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct colormap_ctl_c *cmap_c);
int write_colormap_ctl_c(FILE *fp, int level, const char *label, 
			struct colormap_ctl_c *cmap_c);

void read_lighting_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lighting_ctl_c *light_c);
int write_lighting_ctl_c(FILE *fp, int level, const char *label, 
			struct lighting_ctl_c *light_c);

void read_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_colorbar_ctl_c *cbar_c);
int write_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colorbar_ctl_c *cbar_c);


void read_colormap_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pvr_colormap_bar_ctl_c *cmap_cbar_c);
int write_colormap_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c);

void read_colormap_file_c(const char *file_name, char buf[LENGTHBUF],
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c);
void write_colormap_file_c(const char *file_name, 
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c);

#endif /* CTL_DATA_PVR_COLORMAP_IO_C_H_ */
