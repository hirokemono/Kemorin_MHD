
/* draw_coastline.c */


#include  "draw_coastline.h"

void draw_coastline(double radius, struct buffer_for_gl *gl_buf){
	int iedge, inum, j, k, nd;
	int nedge_coast;
	double tp_coast[4], lake[2], f_color[4];
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	set_black_color_c(f_color);
	
	nedge_coast = get_nedge_coastline();
	inum = 0;
	for (iedge = 0; iedge < nedge_coast; iedge++) {

		get_coastline(iedge, tp_coast, lake);

		for (k = 0; k < 2; k++) {
			gl_buf->xyz[ITWO*inum+k][0] = (GLfloat) (radius * cos(tp_coast[2*k]) * cos(tp_coast[2*k+1]));
			gl_buf->xyz[ITWO*inum+k][1] = (GLfloat) (radius * cos(tp_coast[2*k]) * sin(tp_coast[2*k+1]));
			gl_buf->xyz[ITWO*inum+k][2] = (GLfloat) (radius * sin(tp_coast[2*k]));
		};
		inum = inum + 1;

		if(inum>=NSIZE_GL_BUFFER){
			glDrawArrays(GL_LINES, IZERO, (ITWO*inum));
			inum = 0;
		};
	};
	for (j=0; j<ITWO*inum; j++) {
		for(nd=0;nd<4;nd++){gl_buf->rgba[j][nd] = f_color[nd];}
	}
	if(inum>0){glDrawArrays(GL_LINES, IZERO, (ITWO*inum));};
	glEnable(GL_CULL_FACE);
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);	
	return;
}


void draw_map_coast(struct buffer_for_gl *gl_buf){
	int iedge, j, k, inum, nd;
	int nedge_coast;
	double tp_coast[4], lake[2], f_color[4];
	double rtp_flame[6], xy_coast[4];
	double pi;
	
	pi = TWO * acos(ZERO);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITWO, GL_FLOAT, IZERO, gl_buf->xy);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	set_black_color_c(f_color);
	
	rtp_flame[0] = ONE;
	rtp_flame[3] = ONE;
	nedge_coast = get_nedge_coastline();
	inum = 0;
	for (iedge = 0; iedge < nedge_coast; iedge++) {
		get_coastline(iedge, tp_coast, lake);
		for (k = 0; k < 2; k++) {
			rtp_flame[3*k+1] = -tp_coast[2*k  ]+HALF*pi;
			rtp_flame[3*k+2] =  tp_coast[2*k+1]+pi;
		};
		aitoff_c(ITWO, rtp_flame, xy_coast);
		
		gl_buf->xy[ITWO*inum  ][0] = xy_coast[0];
		gl_buf->xy[ITWO*inum  ][1] = xy_coast[1];
		gl_buf->xy[ITWO*inum+1][0] = xy_coast[2];
		gl_buf->xy[ITWO*inum+1][1] = xy_coast[3];
		inum = inum + 1;
		
		if(inum>=NSIZE_GL_BUFFER){
			glDrawArrays(GL_LINES, IZERO, (ITWO*inum));
			inum = 0;
		};
	};
	for (j=0; j<ITWO*inum; j++) {
		for(nd=0;nd<4;nd++){gl_buf->rgba[j][nd] = f_color[nd];}
	}
	if(inum>0){glDrawArrays(GL_LINES, IZERO, (ITWO*inum));};
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	return;
}

