/*
 *  draw_isolines_4_map.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/16.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "draw_isolines_4_map.h"

static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};


static int draw_isoline_map_triangle(int ist, double width, double v_line, int icomp,
									 double *f_color, struct psf_data *psf_s,
									 struct buffer_for_gl *gl_buf){
	int inum;
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	double xyz_map[9];
	
	int idraw;
	int inod, iele, k, nd, k1;
	
	inum = ist;
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		projection_patch_4_map(xx_tri, xyz_map);
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xyz_map, d_tri, v_line);
		/*  draw isoline */
		if ( idraw == 1 ){
			for(k1=0;k1<6;k1++){
				for(nd=0;nd<2;nd++) gl_buf->xy[6*inum+k1][nd] =  x_ribbon[3*k1+nd];
				for(nd=0;nd<4;nd++) gl_buf->rgba[6*inum+k1][nd] = f_color[nd];
			};
			inum = inum + 1;
		};
		
		if(2*inum>=NSIZE_GL_BUFFER){
			glDrawArrays(GL_TRIANGLES, IZERO, (6*inum));
			inum = 0;
		}
	};
	return inum;
};


static void draw_zeroline_4_map(struct psf_data *psf_s, struct psf_menu_val *psf_m,
								struct buffer_for_gl *gl_buf){
	int inum = 0;

	inum = draw_isoline_map_triangle(inum, 0.005, ZERO, psf_m->icomp_draw_psf, black,
							  psf_s, gl_buf);
	if(inum > 0) glDrawArrays(GL_TRIANGLES, IZERO, (6*inum));
	
	return;
}

static void draw_isolines_4_map(int ist, int ied, struct psf_data *psf_s,
                                struct psf_menu_val *psf_m,
								struct buffer_for_gl *gl_buf){
	int inum = 0;
	int j, nd;
	double v_line;
	double f_color[4];
	
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	for (j = (ist-1); j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
        
		if (psf_m->isoline_color == RAINBOW_LINE){	
			set_rainbow_color_code(psf_m->cmap_psf, v_line, f_color);
		};
		inum = draw_isoline_map_triangle(inum, 0.002, v_line, psf_m->icomp_draw_psf, f_color, 
								  psf_s, gl_buf);
	};
	if(inum > 0) glDrawArrays(GL_TRIANGLES, IZERO, (6*inum));

	return;
}


void draw_map_PSF_isoline(struct psf_data *psf_s, struct psf_menu_val *psf_m,
						  struct buffer_for_gl *gl_buf, int iflag_retina){
    int ierr;

	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITWO, GL_FLOAT, IZERO, gl_buf->xy);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	if(psf_m->draw_psf_grid  != 0){
        find_start_positive_lines(psf_m);
        if(psf_m->ist_positive_line > 1){
            draw_isolines_4_map(IONE, psf_m->ist_positive_line,
                                psf_s, psf_m, gl_buf);
        };
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            draw_isolines_4_map(psf_m->ist_positive_line,
                                psf_m->n_isoline, psf_s, psf_m, gl_buf);
        };
    };
	if(psf_m->draw_psf_zero  != 0){
        draw_zeroline_4_map(psf_s, psf_m, gl_buf);
    };
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	return;
}
