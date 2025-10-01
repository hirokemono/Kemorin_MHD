/*
//  read_psf_bin_data_gz_c.h
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2020/06/12.
*/

#ifndef read_psf_bin_data_c_gz_h_
#define read_psf_bin_data_c_gz_h_

#include <stdio.h>
#include <stdlib.h>
#include "skip_comment_c.h"
#include "calypso_zlib_io_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "read_psf_bin_data_c.h"


/*  prototype  */

void read_psf_num_node_bin_gz(const char *gzip_name, struct psf_data *psf_z);
int read_alloc_psf_mesh_bin_gz(const char *gzip_name, struct psf_data *psf_z);
int read_alloc_psf_bin_gz(const char *gzip_name, double *time, struct psf_data *psf_z);
int read_alloc_iso_bin_gz(const char *gzip_name, double *time, struct psf_data *psf_z);

#endif /* read_psf_bin_data_c_gz_h_ */
