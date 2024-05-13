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
static void copy_each_triangle_postion_norm(long ntot_comp, long ie_viz[3], 
                                            double *xyzw_viz, double *norm_nod,
											double *d_nod, long icomp,
                                            long inod_tri[3], double xyzw_tri[12], 
                                            double norm_tri[12], double d_tri[3]){
	int k;
	long inod;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
        
        inod_tri[k] =  ie_viz[k];
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
        xyzw_tri[4*k  ] = xyzw_viz[inod*IFOUR + 0];
        xyzw_tri[4*k+1] = xyzw_viz[inod*IFOUR + 1];
        xyzw_tri[4*k+2] = xyzw_viz[inod*IFOUR + 2];
        xyzw_tri[4*k+3] = 1.0;
        norm_tri[4*k  ] = norm_nod[4*inod+0];
        norm_tri[4*k+1] = norm_nod[4*inod+1];
        norm_tri[4*k+2] = norm_nod[4*inod+2];
        norm_tri[4*k+3] = 1.0;
	};
	return;
};

static void copy_each_triangle_map_postion(long ntot_comp, long ie_viz[3], double *xyzw_viz,
                                           double *d_nod, long icomp,
                                           long inod_tri[3], double xyzw_map[12],
                                           double norm_tri[12], double d_tri[3]){
    double xyz_tri[9];
    double xyz_map[9];
	long inod;
	int k;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
        inod_tri[k] =  ie_viz[k];
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
        norm_tri[4*k  ] = 0.0;
        norm_tri[4*k+1] = 0.0;
        norm_tri[4*k+2] = 1.0;
        norm_tri[4*k+3] = 1.0;
   }
    return;
};

long add_each_isoline_npatch(const long ist_patch,
                             const long ist, const long ied,
                             const double v_line, long icomp,
                             struct psf_data *psf_s){
    double d_tri[3];
    long iele;
    int idraw;
    
    long inum_patch = ist_patch;
    for(iele=ist;iele<ied;iele++){
        copy_each_triangle_postion(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                   psf_s->d_nod, icomp, d_tri);
        /*  find isoline */
        idraw = find_isoline_on_triangle(d_tri, v_line);
        /*  count isoline */
        inum_patch = inum_patch + 12 * idraw;
    };
    return inum_patch;
};

long set_each_isoline_to_buf(const long ist_patch,
                             const long ist, const long ied,
                             double width, double v_line,
                             long icomp, double *f_color,
                             struct psf_data *psf_s,
                             struct gl_strided_buffer *strided_buf){
    long inod_tri[3], iedge_itp[2];
    long inod_itp_psf[4], inod_itp_edge[4];
	double d_tri[3];
    double xyzw_tri[12], norm_tri[12];
    double xyzw_line[8], dir_line[8], norm_line[8], color_line[8];
	int hex_tube[12][3];
	
	int idraw, nd;
	long iele;
	
	long inum_patch = ist_patch;
	copy_hex_tube_pp(hex_tube);
	for (iele = ist; iele < ied; iele++) {
		copy_each_triangle_postion_norm(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                        psf_s->xyzw_viz, psf_s->norm_nod,
										psf_s->d_nod, icomp,
                                        inod_tri, xyzw_tri, norm_tri, d_tri);
		/*  find isoline */
		idraw = set_isoline_on_triangle(iedge_itp, inod_itp_edge, inod_itp_psf, 
                                        xyzw_line, dir_line, norm_line,
                                        iele, inod_tri, xyzw_tri, norm_tri, d_tri,
                                        v_line, psf_s->psf_edge);
        
		/* draw isoline */
        if(idraw == 1){
			for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
			for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
			inum_patch = append_line_tube_to_buf(inum_patch, hex_tube, width, color_line, 
                                                 xyzw_line, dir_line, norm_line,
                                                 strided_buf);
		};
	};

	return inum_patch;
};

long set_each_map_isoline_to_buf(const long ist_patch,
                                 const long ist, const long ied,
                                 double width, double v_line,
                                 long icomp, double *f_color,
                                 struct psf_data *psf_s,
                                 struct gl_strided_buffer *strided_buf){
    long inod_tri[3], iedge_itp[2];
    long inod_itp_psf[4], inod_itp_edge[4];
    double d_tri[3];
    double xyzw_map[12], norm_tri[12];
    double xyzw_line[8], dir_line[8], norm_line[8], color_line[8];
    int hex_tube[12][3];
    
    int idraw, nd;
    long iele;
    
    long inum_patch = ist_patch;
    copy_hex_tube_pp(hex_tube);
    for (iele = ist; iele < ied; iele++) {
        copy_each_triangle_map_postion(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                       psf_s->xyzw_viz, psf_s->d_nod, icomp,
                                       inod_tri, xyzw_map, norm_tri, d_tri);
        idraw = set_isoline_on_triangle(iedge_itp, inod_itp_edge, inod_itp_psf, 
                                        xyzw_line, dir_line, norm_line, 
                                        iele, inod_tri, xyzw_map, norm_tri, d_tri, 
                                        v_line, psf_s->psf_edge);
        /*  draw isoline */
        if(idraw == 1){
            for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
            for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
            inum_patch = append_line_tube_to_buf(inum_patch, hex_tube, width, color_line,
                                                 xyzw_line, dir_line, norm_line,
                                                 strided_buf);
        };
    };
    return inum_patch;
};


static void copy_each_triangle_postion_norm2(long ntot_comp, long ie_viz[3], 
                                            double *xyzw_viz, double *norm_nod,
											double *d_nod, long icomp,
                                            long inod_tri[3], double xyzw_tri[12], 
                                            double norm_tri[12], double d_tri[3]){
	int k;
	long inod;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
        
        inod_tri[k] =  ie_viz[k];
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
        xyzw_tri[4*k  ] = xyzw_viz[inod*IFOUR + 0];
        xyzw_tri[4*k+1] = xyzw_viz[inod*IFOUR + 1];
        xyzw_tri[4*k+2] = xyzw_viz[inod*IFOUR + 2];
        xyzw_tri[4*k+3] = 1.0;
        norm_tri[4*k  ] = norm_nod[4*inod+0];
        norm_tri[4*k+1] = norm_nod[4*inod+1];
        norm_tri[4*k+2] = norm_nod[4*inod+2];
        norm_tri[4*k+3] = 1.0;
	};
	return;
};


long set_each_isoline_test(const long ist_line,
                             const long ist, const long ied,
                             double width, double v_line,
                             long icomp, double *f_color,
                             struct psf_data *psf_s,
                           long *iedge_itp, double *xyzw_line){
    long inod_tri[3];
    long inod_itp_edge[4], inod_itp_psf[4];
	double d_tri[3];
    double xyzw_tri[12], norm_tri[12];
    double norm_line[8], color_line[8], dir_line[8];
	int hex_tube[12][3];
	
	int idraw, nd;
	long iele;
    long i, iedge;
    
	long num_line = ist_line;
	copy_hex_tube_pp(hex_tube);
	for (iele = ist; iele < ied; iele++) {
		copy_each_triangle_postion_norm2(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                        psf_s->xyzw_viz, psf_s->norm_nod,
										psf_s->d_nod, icomp,
                                        inod_tri, xyzw_tri, norm_tri, d_tri);
		/*  find isoline */
		idraw = set_isoline_on_triangle(&iedge_itp[2*(num_line-ist_line)],
                                        inod_itp_edge, inod_itp_psf,
                                        &xyzw_line[8*(num_line-ist_line)],
                                        dir_line, norm_line,
                                        iele, inod_tri, xyzw_tri, norm_tri, d_tri,
                                        v_line, psf_s->psf_edge);
        
		/* draw isoline */
        if(idraw == 1){num_line = num_line + 1;};
	};

	return num_line;
};

long set_each_isoline_to_buf2(const long ist_patch,
                             const long ist, const long ied,
                             struct psf_data *psf_s,
                              struct isoline_line_work *wk_iso_line,
                             struct gl_strided_buffer *strided_buf){
    int hex_tube[12][3];
    
    int idraw, nd;
    long iele, iedge1, iedge2;
    
    long inum_patch = ist_patch;
    copy_hex_tube_pp(hex_tube);
    long icou = 0;
    for (icou=ist; icou<ied; icou++){
           inum_patch = append_line_tube_to_buf(inum_patch, hex_tube,
                                                wk_iso_line->width,
                                                wk_iso_line->f_color,
                                                &wk_iso_line->xyzw_line[8*icou],
                                                &wk_iso_line->dir_line[8*icou],
                                                &wk_iso_line->norm_line[8*icou],
                                                strided_buf);
    };

    return inum_patch;
};
