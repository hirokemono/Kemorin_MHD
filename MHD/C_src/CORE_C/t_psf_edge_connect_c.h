/*
 *  t_psf_edge_connect_c.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/11/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef T_PSF_EDGE_CONNECT_C__
#define T_PSF_EDGE_CONNECT_C__

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "t_surf_edge_hash_c.h"

struct psf_edge_data_c{
	long nedge_viewer;
	
	int nnod_p_edge;
	int nedge_p_surf;
	
	long **ie_edge;
	long **iedge_4_sf;
	
	long *iedge_gl_view;
	
	double *xx_edge;
	double *edge_norm;
	double *edge_dir;
	double *edge_len;
};

/*  prototypes */
struct psf_edge_data_c * init_psf_edge_data_c(void);

struct psf_edge_data_c * init_all_edge_4_psf(const long nnod_viz, const long nele_viz,
											 const int nnod_4_ele_viz, long **ie_viz,
											 double **xx_viz, double **norm_nod);
void dealloc_edge_data_4_psf(const long nele_viz, struct psf_edge_data_c *psf_edge);

#endif
