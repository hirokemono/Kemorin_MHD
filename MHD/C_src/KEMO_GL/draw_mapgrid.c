
/* draw_mapgrid.c */


#include  "draw_mapgrid.h"


#define NUM_T   6
#define NUM_P   6
#define N_CURVE 180

double theta_p_grid[N_CURVE+1];
double phi_p_grid[NUM_P+1];
double theta_t_grid[NUM_T+1];
double phi_t_grid[N_CURVE+1];

void init_mapgrid_position(){
	int i, j;
	double pi;
	
	pi = TWO * acos(ZERO);
	
	for(i=0; i<N_CURVE+1; i++){theta_p_grid[i] = (double)i * pi / (double)N_CURVE;}
	for(j=0; j<NUM_P+1; j++) {phi_p_grid[j] = (double)j * TWO * pi / (double)NUM_P;}
	
	for(j=0; j<NUM_T-1; j++){theta_t_grid[j] = (double)(j+1) * pi / (double)NUM_T;}
	for(i=0; i<N_CURVE+1; i++){phi_t_grid[i] = (double)i * TWO * pi / (double)N_CURVE;}
	return;
}

void draw_flame_4_map(struct buffer_for_gl *gl_buf, int iflag_write_ps){
	int i, j, nd, inum, ierr;
	double rtp_flame[6], d_map_flame[4], f_color[4];
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITWO, GL_FLOAT, IZERO, gl_buf->xy);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	glEnable(GL_LINE_STIPPLE);
	glLineStipple(1,0x3333);
    if (iflag_write_ps == ON) {ierr = gl2psEnable(GL2PS_LINE_STIPPLE);};
	
	set_black_color_c(f_color);
	
	inum = 0;
	rtp_flame[0] = ONE;
	rtp_flame[3] = ONE;
	for(j=0; j<NUM_P+1; j++){
		rtp_flame[2] = phi_p_grid[j];
		rtp_flame[5] = phi_p_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[1] = theta_p_grid[i];
			rtp_flame[4] = theta_p_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			gl_buf->xy[ITWO*inum  ][0] = d_map_flame[0];
			gl_buf->xy[ITWO*inum  ][1] = d_map_flame[1];
			gl_buf->xy[ITWO*inum+1][0] = d_map_flame[2];
			gl_buf->xy[ITWO*inum+1][1] = d_map_flame[3];
			inum = inum + 1;
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		rtp_flame[1] = theta_t_grid[j] ;
		rtp_flame[4] = theta_t_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[2] = phi_t_grid[i];
			rtp_flame[5] = phi_t_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			gl_buf->xy[ITWO*inum  ][0] = d_map_flame[0];
			gl_buf->xy[ITWO*inum  ][1] = d_map_flame[1];
			gl_buf->xy[ITWO*inum+1][0] = d_map_flame[2];
			gl_buf->xy[ITWO*inum+1][1] = d_map_flame[3];
			inum = inum + 1;
		}
	}
	for (j=0; j<ITWO*inum; j++) {
		for(nd=0;nd<4;nd++){gl_buf->rgba[j][nd] = f_color[nd];}
	}
	if(inum>0){glDrawArrays(GL_LINES, IZERO, (ITWO*inum));};
    
    if (iflag_write_ps == ON) {ierr = gl2psDisable(GL2PS_LINE_STIPPLE);};
	glDisable(GL_LINE_STIPPLE);
	
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	return;
}

void draw_sph_flame(double radius, struct buffer_for_gl *gl_buf,
                    int iflag_write_ps){
	int i, j, k, nd, inum, ierr;
	double f_color[4];
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_LINE_STIPPLE);
	glLineStipple(1,0x3333);
    if (iflag_write_ps == ON) {ierr = gl2psEnable(GL2PS_LINE_STIPPLE);};
	
	set_black_color_c(f_color);
	
	inum = 0;
	for(j=0; j<NUM_P+1; j++){
		for(i=0; i<N_CURVE; i++){
			for (k=0; k<2; k++){
				gl_buf->xyz[ITWO*inum+k][0] = (GLfloat) (radius * sin(theta_p_grid[k+i]) * cos(phi_p_grid[j]));
				gl_buf->xyz[ITWO*inum+k][1] = (GLfloat) (radius * sin(theta_p_grid[k+i]) * sin(phi_p_grid[j]));
				gl_buf->xyz[ITWO*inum+k][2] = (GLfloat) (radius * cos(theta_p_grid[k+i]));
			};
			inum = inum + 1;
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		for(i=0; i<N_CURVE; i++){
			for (k=0; k<2; k++){
				gl_buf->xyz[ITWO*inum+k][0] = (GLfloat) (radius * sin(theta_t_grid[j]) * cos(phi_t_grid[i+k]));
				gl_buf->xyz[ITWO*inum+k][1] = (GLfloat) (radius * sin(theta_t_grid[j]) * sin(phi_t_grid[i+k]));
				gl_buf->xyz[ITWO*inum+k][2] = (GLfloat) (radius * cos(theta_t_grid[j]));
			};
			inum = inum + 1;
		}
	}
	
	for (j=0; j<ITWO*inum; j++) {
		for(nd=0;nd<4;nd++){gl_buf->rgba[j][nd] = f_color[nd];}
	}
	
	if(inum>0){glDrawArrays(GL_LINES, IZERO, (ITWO*inum));};
    
    if (iflag_write_ps == ON) {ierr = gl2psDisable(GL2PS_LINE_STIPPLE);};
	glDisable(GL_LINE_STIPPLE);
	
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	return;
}
