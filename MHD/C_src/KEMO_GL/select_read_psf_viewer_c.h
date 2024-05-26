/*
//
//  select_read_psf_viewer_c.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2013/09/21.
//
//
*/

#ifndef SELECCT_READ_PSF_VIEWER_C_
#define SELECCT_READ_PSF_VIEWER_C_

#include <stdio.h>

#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "m_psf_data_4_viewer_c.h"
#include "read_psf_data_viewer_c.h"
#include "read_psf_vtk_data_c.h"
#include "read_psf_data_gz_c.h"
#include "read_psf_vtk_data_gz_c.h"
#include "read_psf_bin_data_c.h"
#include "read_psf_bin_data_gz_c.h"

/* prototypes */

int check_gzip_kemoview_ucd_first(int iformat_ucd_file, int istep, double *time, 
                                  const char *ucd_header, struct psf_data *viz_s);
void check_gzip_psf_num_nod_first(int iformat_ucd_file, const char *ucd_header,
                                  struct psf_data *viz_s);
int check_gzip_psf_grd_first(int iformat_ucd_file, const char *ucd_header,
			struct psf_data *viz_s);
void check_gzip_psf_udt_first(int iformat_ucd_file, int istep, double *time, const char *ucd_header,
			struct psf_data *viz_s);

#endif
