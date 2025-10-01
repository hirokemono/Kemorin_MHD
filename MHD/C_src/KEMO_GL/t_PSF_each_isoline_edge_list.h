/*
 *  t_PSF_each_isoline_edge_list.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */


#ifndef T_PSF_EACH_ISOLINE_EDGE_LIST_
#define T_PSF_EACH_ISOLINE_EDGE_LIST_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "t_psf_edge_connect_c.h"
#include "cal_surface_center_normal_c.h"

struct isoline_mesh_work{
    long num_edge;
    long *inum_line;
    long *ineib_edge;
    double *xyzw_edge;
};
    
struct isoline_line_work{
    long num_line;
    long *iedge_itp;
    double *xyzw_line;
    double *dir_line;
    
    int ncorner;
    double f_color[8];
    double width;
    
    int *iflag_checked;
};


/* prototypes */

struct isoline_mesh_work * init_isoline_mesh_work(struct psf_edge_data_c *psf_edge);
void dealloc_isoline_mesh_work(struct isoline_mesh_work *wk_iso_mesh);

struct isoline_line_work * init_isoline_line_work(int nthreads, long *istack_threads);
void dealloc_isoline_line_work(struct isoline_line_work *wk_iso_line);

void set_isoline_edge_list(struct isoline_line_work *wk_iso_line, 
                           struct isoline_mesh_work *wk_iso_mesh);
void set_isoline_position_on_edge(struct isoline_line_work *wk_iso_line, 
                                  struct isoline_mesh_work *wk_iso_mesh);

void set_direction_for_isoline(struct psf_edge_data_c *psf_edge,
                               struct isoline_mesh_work *wk_iso_mesh, 
                               struct isoline_line_work *wk_iso_line);
void set_normal_for_isoline(double *xyzw_psf,
                            struct psf_edge_data_c *psf_edge,
                            struct isoline_mesh_work *wk_iso_mesh, 
                            struct isoline_line_work *wk_iso_line);

void adjust_direction_by_neighbor(struct isoline_mesh_work *wk_iso_mesh, 
                                  double *vect_line);

void set_isoline_color_in_wk(double color[4],
                             struct isoline_line_work *wk_iso_line);

#endif /* T_PSF_EACH_ISOLINE_EDGE_LIST_ */


