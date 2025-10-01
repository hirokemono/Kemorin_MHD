/*
//  ctl_data_pvr_IO_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef CTL_DATA_PVR_IO_C_H_
#define CTL_DATA_PVR_IO_C_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_4_pvr_c.h"

#define NLBL_PVR_MOVIE_CTL      2
#define NLBL_PVR_CTL            19


/* prototypes */
void read_pvr_plot_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_plot_area_ctl_c *area_c);
int write_pvr_plot_area_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_plot_area_ctl_c *area_c);


void read_pvr_ctl_items(FILE *fp, char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_items(FILE *fp, int level, struct pvr_ctl_c *pvr_c);
void read_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_ctl_c *pvr_c);

void rename_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c);
void read_pvr_ctl_subfiles(char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c);
void write_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c);

void read_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_file_c(const char *file_name, struct pvr_ctl_c *pvr_c);

#endif /* CTL_DATA_PVR_IO_C_H_ */
