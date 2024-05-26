/*
//
//  read_psf_vtk_data_gz_c.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2013/09/22.
//
//
*/

#ifndef READ_PSF_VTK_DATA_GZ_
#define READ_PSF_VTK_DATA_GZ_


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "read_psf_data_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "calypso_zlib_io_c.h"
#include "skip_comment_c.h"

/* prototypes */
void read_num_node_vtg_gz(const char *file_name, struct psf_data *viz_s);
int read_psf_vtg_gz(const char *file_name, struct psf_data *viz_s);
int read_psf_vtd_gz(const char *file_name, struct psf_data *viz_s);
int read_kemoview_vtk_gz(const char *file_name, struct psf_data *viz_s);

#endif
