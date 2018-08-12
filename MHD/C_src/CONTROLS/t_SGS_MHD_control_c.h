/*
//  t_SGS_MHD_control_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#ifndef t_SGS_MHD_control_c_h_
#define t_SGS_MHD_control_c_h_

#define NLBL_SGS_MHD_CTL        10

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"

#include "t_ctl_data_4_platforms_c.h"
#include "t_ctl_data_SGS_MHD_model_c.h"
#include "t_control_data_sph_grid_c.h"

#include "t_control_data_vizs_c.h"
#include "t_ctl_data_4_sph_monitor_c.h"
#include "t_ctl_data_node_monitor_c.h"
#include "t_ctl_data_zonal_means_c.h"


struct SGS_MHD_control_c{
	int maxlen;
	
	int iflag_data_files_def;
	struct platform_data_control_c *files;
	int iflag_org_files_def;
	struct platform_data_control_c *org_files;
	int iflag_new_files_def;
	struct platform_data_control_c *new_files;
	
	int iflag_spherical_shell_ctl;
	char *shell_ctl_file_name;
	struct parallel_sph_shell_control_c *shell_ctl;
	
	int iflag_model;
	struct mhd_model_control_c *model_ctl;
	int iflag_control;
	struct sph_mhd_control_control_c *control_ctl;
	
	int iflag_sph_monitor_ctl;
	struct sph_monitor_control_c *smtr_ctl;
	int iflag_node_monitor_ctl;
	struct node_monitor_ctl_c *nmtr_ctl;
	
	int iflag_visual_control;
	struct visualizers_ctl_c *viz_c;
	int iflag_zonal_mean_control;
	struct sph_zonal_means_ctl_c *zm_ctls;
};

/* Prototypes */
void get_label_MHD_control_head(char *label);
void get_label_SGS_MHD_ctl(int index, char *label);

void alloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl);
void dealloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl);

int read_SGS_MHD_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct SGS_MHD_control_c *mhd_ctl);
void dealloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl);
int read_SGS_MHD_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct SGS_MHD_control_c *mhd_ctl);
int write_SGS_MHD_control_c(FILE *fp, int level, const char *label, 
			struct SGS_MHD_control_c *mhd_ctl);

void rename_SGS_MHD_ctl_subfile_c(struct SGS_MHD_control_c *mhd_ctl);
void read_SGS_MHD_ctl_subfile_c(char buf[LENGTHBUF], struct SGS_MHD_control_c *mhd_ctl);
void write_SGS_MHD_ctl_subfile_c(struct SGS_MHD_control_c *mhd_ctl);

void read_SGS_MHD_control_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct SGS_MHD_control_c *mhd_ctl);
void write_SGS_MHD_control_file_c(const char *file_name, struct SGS_MHD_control_c *mhd_ctl);


#endif /* t_SGS_MHD_control_c_h */
