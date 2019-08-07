
/* draw_fieldlines.c */


#include "draw_fieldlines.h"

/*
static const GLfloat black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
*/


void draw_fieldtubes_c(struct psf_data *fline_s, struct fline_menu_val *fline_m,
					   struct buffer_for_gl *gl_buf) {
	int num_wall, inum_buf;
	int inod, iele, i, k, nd;
	int ncorner = ISIX;
	float xyz[9*2*ncorner], nor[9*2*ncorner], col[12*2*ncorner];
	float x_line[6], dir_line[6], color_line[8];
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
	
	inum_buf = 0;
	for (iele = 0; iele < fline_s->nele_viz; iele++) {
		for (k = 0; k < 2; k++) {
			inod = fline_s->ie_viz[iele][k] - 1;
			for (nd=0; nd<3; nd++) {
				x_line[3*k+nd] = (float) fline_s->xx_viz[inod][nd];
				dir_line[3*k+nd] = (float) fline_s->dir_nod[inod][nd];
			};
			for (nd=0; nd<4; nd++) {color_line[4*k+nd] = (float) fline_s->color_nod[inod][nd];};
		};
		
		num_wall = set_tube_vertex(ncorner, fline_m->fieldline_thick, x_line, dir_line, color_line,
								   xyz, nor, col);
		
		for (i=0; i<3*num_wall; i++) {
			for(nd=0;nd<3;nd++){gl_buf->xyz[3*inum_buf+i][nd] =  xyz[3*i+nd];};
			for(nd=0;nd<3;nd++){gl_buf->norm[3*inum_buf+i][nd] = nor[3*i+nd];};
			for(nd=0;nd<4;nd++){gl_buf->rgba[3*inum_buf+i][nd] = col[4*i+nd];};
		};
		inum_buf = inum_buf + num_wall;

		if(inum_buf >= (NSIZE_GL_BUFFER - num_wall) ){
			glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));
			inum_buf = 0;
		};
 
	};
	if(inum_buf > 0){glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));};
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	return;
}

void draw_fieldlines_c(struct psf_data *fline_s, struct fline_menu_val *fline_m,
					   struct buffer_for_gl *gl_buf) {
	int inod, iele, k;
	int num, icou, inum;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	icou = 0;
	while(icou<fline_s->nele_viz){
		if(icou+NSIZE_GL_BUFFER <= fline_s->nele_viz) {num = NSIZE_GL_BUFFER;}
		else {num = fline_s->nele_viz-icou;};

		for(inum=0; inum<num; inum++){
			iele = inum + icou;
			for(k=0;k<ITWO;k++){
				inod =fline_s->ie_viz[iele][k] - 1;
				gl_buf->xyz[ITWO*inum+k][0] = fline_s->xx_viz[inod][0];
				gl_buf->xyz[ITWO*inum+k][1] = fline_s->xx_viz[inod][1];
				gl_buf->xyz[ITWO*inum+k][2] = fline_s->xx_viz[inod][2];
				/*
				 for(nd=0;nd=3;nd++){
				 gl_buf->norm[ITWO*inum+k][nd] = fline_s->norm_nod[inod][nd];
				 };
				 */
				gl_buf->rgba[ITWO*inum+k][0] = fline_s->color_nod[inod][0];
				gl_buf->rgba[ITWO*inum+k][1] = fline_s->color_nod[inod][1];
				gl_buf->rgba[ITWO*inum+k][2] = fline_s->color_nod[inod][2];
				gl_buf->rgba[ITWO*inum+k][3] = fline_s->color_nod[inod][3];
			};
		};
		glDrawArrays(GL_LINES, IZERO, (ITWO*num));
		icou = icou + NSIZE_GL_BUFFER;
	};
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	
	return;
}

