/*
//  t_ctl_data_zonal_means_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#ifndef t_ctl_data_zonal_means_c_h_
#define t_ctl_data_zonal_means_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_control_data_4_psf_c.h"

struct sph_zonal_means_ctl_c{
	int maxlen;
	
	int iflag_zmean_section_controls;
	char *zmean_psf_file_name;
	struct psf_ctl_c *zmean_psf_c;
	int iflag_zrms_section_controls;
	char *zrms_psf_file_name;
	struct psf_ctl_c *zrms_psf_c;
};

/* Prototypes */

void alloc_sph_zonal_means_controls_c(struct sph_zonal_means_ctl_c *zm_ctls);
void dealloc_sph_zonal_means_controls_c(struct sph_zonal_means_ctl_c *zm_ctls);
int read_zonal_mean_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct sph_zonal_means_ctl_c *zm_ctls);
int write_zonal_mean_control_c(FILE *fp, int level, const char *label, 
			struct sph_zonal_means_ctl_c *zm_ctls);
void read_zonal_mean_psf_ctl_file_c(char buf[LENGTHBUF], struct sph_zonal_means_ctl_c *zm_ctls);
void write_zonal_mean_psf_ctl_file_c(struct sph_zonal_means_ctl_c *zm_ctls);


#endif /* t_ctl_data_zonal_means_c_h_ */
