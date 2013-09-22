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

#include "m_psf_data_4_viewer_c.h"
#include "m_kemoviewer_menu.h"
#include "read_psf_data_viewer_c.h"
#include "read_psf_vtk_data_c.h"
#include "read_psf_data_gz_c.h"
#include "read_psf_vtk_data_gz_c.h"

/* prototypes */

int check_gzip_kemoview_ucd_first(struct ucd_file_menu_val *ucd_m, struct psf_data *viz_s);
int check_gzip_psf_grd_first(struct ucd_file_menu_val *ucd_m, struct psf_data *viz_s);
void check_gzip_psf_udt_first(struct ucd_file_menu_val *ucd_m, struct psf_data *viz_s);

#endif
