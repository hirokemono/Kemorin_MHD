/*
 *  set_each_isoline_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "set_each_isoline_to_buf.h"

static void copy_each_triangle_postion(long ntot_comp, long ie_viz[3],
                                       double *d_nod, long icomp,
                                       double d_tri[3]){
	int k;
	long inod;
	for(k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
	};
	return;
};

static void copy_each_triangle_map_postion(long ntot_comp, long ie_viz[3], 
                                           double *xyzw_viz, double *d_nod, long icomp,
                                           double xyzw_map[12], double d_tri[3]){
    double xyz_tri[9];
    double xyz_map[9];
	long inod;
	int k;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
		xyz_tri[3*k  ] = xyzw_viz[inod*IFOUR + 0];
		xyz_tri[3*k+1] = xyzw_viz[inod*IFOUR + 1];
		xyz_tri[3*k+2] = xyzw_viz[inod*IFOUR + 2];
	};

    projection_patch_4_map(xyz_tri, xyz_map);

    for (k = 0; k < 3; k++) {
        xyzw_map[4*k  ] = xyz_map[3*k  ];
        xyzw_map[4*k+1] = xyz_map[3*k+1];
        xyzw_map[4*k+2] = xyz_map[3*k+2];
        xyzw_map[4*k+3] = 1.0;
   }
    return;
};

long count_each_isoline_npatch(const long ist, const long ied,
                               const double v_line, long icomp,
                               struct psf_data *psf_s){
    double d_tri[3];
    long iele;
    
    long num_line = 0;
    for(iele=ist;iele<ied;iele++){
        copy_each_triangle_postion(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                   psf_s->d_nod, icomp, d_tri);
        /*  find isoline */
        num_line = num_line + find_isoline_on_triangle(d_tri, v_line);
    };
    return num_line;
};

long set_each_map_isoline_to_list(const long ist_line,
                                  const long ist, const long ied,
                                  double v_line, long icomp,
                                  struct psf_data *psf_s,
                                  struct psf_normals *psf_n,
                                  struct isoline_line_work *wk_iso_line){
    long iedge_out[2];
    double d_tri[3];
    double xyzw_map[12];
    double xyzw_out[8];
    
    int idraw, nd;
    long iele;
    
    long num_line = ist_line;
    for (iele = ist; iele < ied; iele++) {
        copy_each_triangle_map_postion(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                       psf_s->xyzw_viz, psf_s->d_nod, icomp,
                                       xyzw_map, d_tri);
        
        /*  find isoline */
        idraw = set_isoline_on_triangle(iedge_out, xyzw_out,
                                        iele, xyzw_map, d_tri, 
                                        v_line, psf_n->psf_edge);
		/* store isoline */
        if(idraw == 1){
            wk_iso_line->iedge_itp[2*num_line  ] = iedge_out[0];
            wk_iso_line->iedge_itp[2*num_line+1] = iedge_out[1];
            for(nd=0;nd<8;nd++){wk_iso_line->xyzw_line[8*num_line+nd] = xyzw_out[nd];}
            num_line = num_line + 1;
        };
    };
    return num_line;
};


static void copy_each_triangle_postion_norm(long ntot_comp, long ie_viz[3], 
                                            double *xyzw_viz, double *norm_nod,
											double *d_nod, long icomp,
                                            double xyzw_tri[12], double d_tri[3]){
	int k;
	long inod;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
        
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
        xyzw_tri[4*k  ] = xyzw_viz[inod*IFOUR + 0];
        xyzw_tri[4*k+1] = xyzw_viz[inod*IFOUR + 1];
        xyzw_tri[4*k+2] = xyzw_viz[inod*IFOUR + 2];
        xyzw_tri[4*k+3] = 1.0;
	};
	return;
};


long set_each_isoline_to_list(const long ist_line,
                              const long ist, const long ied,
                              double v_line, long icomp,
                              struct psf_data *psf_s,
                              struct psf_normals *psf_n,
                              struct isoline_line_work *wk_iso_line){
    long iedge_out[2];
	double d_tri[3], xyzw_out[8];
    double xyzw_tri[12];
	
	int idraw, nd;
	long iele;
    
	long num_line = ist_line;
	for (iele = ist; iele < ied; iele++) {
		copy_each_triangle_postion_norm(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                        psf_s->xyzw_viz, psf_n->norm_nod,
										psf_s->d_nod, icomp, xyzw_tri, d_tri);
		/*  find isoline */
        idraw = set_isoline_on_triangle(iedge_out, xyzw_out, 
                                        iele, xyzw_tri, d_tri,
                                        v_line, psf_n->psf_edge);
        
		/* store isoline */
        if(idraw == 1){
            wk_iso_line->iedge_itp[2*num_line  ] = iedge_out[0];
            wk_iso_line->iedge_itp[2*num_line+1] = iedge_out[1];
            for(nd=0;nd<8;nd++){wk_iso_line->xyzw_line[8*num_line+nd] = xyzw_out[nd];}
            num_line = num_line + 1;
        };
	};

	return num_line;
};

long set_each_isotube_to_buf(const long ist_tube,
                             const long ist, const long ied,
                             struct psf_data *psf_s,
                             struct isoline_line_work *wk_iso_line,
                             struct gl_strided_buffer *strided_buf,
                             struct gl_index_buffer *index_buf){
    long icou;
    long inum_tube = ist_tube;
    for(icou=ist; icou<ied; icou++){
        inum_tube = set_tube_node_index_buffer(inum_tube, wk_iso_line->ncorner,
                                               wk_iso_line->width,
                                               &wk_iso_line->xyzw_line[8*icou],
                                               &wk_iso_line->dir_line[8*icou],
                                               wk_iso_line->f_color,
                                               strided_buf, index_buf);
    };
    return inum_tube;
};

long set_each_isoline_to_buf(const long ist_tube,
                             const long ist, const long ied,
                             struct psf_data *psf_s,
                             struct isoline_line_work *wk_iso_line,
                             struct gl_strided_buffer *strided_buf){
    long icou;
    long inum_tube = ist_tube;
    for(icou=ist; icou<ied; icou++){
        inum_tube = set_line_strided_buffer(inum_tube,
                                            &wk_iso_line->xyzw_line[8*icou],
                                            wk_iso_line->f_color,
                                            strided_buf);
    };
    return inum_tube;
};
