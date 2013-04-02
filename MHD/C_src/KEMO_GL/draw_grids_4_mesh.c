
/* draw_grids_4_mesh.c */

#include "draw_grids_4_mesh.h"

static int draw_each_grid(int iedge, struct viewer_mesh *mesh_s, double f_color[4],
						  int inum_buf, struct buffer_for_gl *gl_buf){
	int i1, i2, k1, nd;
	int inum;
	
	inum = inum_buf;
	for(k1=0;k1<(mesh_s->nnod_4_edge-1);k1++){
		i1 = mesh_s->ie_edge_viewer[iedge][k1  ]-1;
		i2 = mesh_s->ie_edge_viewer[iedge][k1+1]-1;
		for(nd=0;nd<3;nd++) {
			gl_buf->xyz[ITWO*inum  ][nd] =  mesh_s->xx_draw[i1][nd];
			gl_buf->xyz[ITWO*inum+1][nd] =  mesh_s->xx_draw[i2][nd];
		};
		for(nd=0;nd<4;nd++){
			gl_buf->rgba[ITWO*inum  ][nd] = f_color[nd];
			gl_buf->rgba[ITWO*inum+1][nd] = f_color[nd];
		}
		
		inum = inum + 1;
		if(inum>=NSIZE_GL_BUFFER){
			glDrawArrays(GL_LINES, IZERO, (ITWO*inum));
			inum = 0;
		};
	};
	
	return inum;
}

void draw_mesh_grid(int line_color, int color_mode, int color_loop, GLfloat single_color[4],
					int num_grp, int *istack_grp, int *item_grp, int igrp, int *iflag_domain, 
					struct viewer_mesh *mesh_s, struct buffer_for_gl *gl_buf){
	
	int ip, inum, iedge, ist, ied;
	int inum_buf;
	double f_color[4];
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	glDisable(GL_CULL_FACE);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);


	inum_buf = 0;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			set_grid_color_mode_c(line_color, color_mode, color_loop, ip, mesh_s->num_pe_sf, 
								  igrp, num_grp, single_color, f_color);
			
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(inum = ist; inum < ied; inum++){
				iedge = abs( item_grp[inum] ) - 1;
				inum_buf = draw_each_grid(iedge, mesh_s, f_color, inum_buf, gl_buf);
			};
		};
	};
	if(inum_buf > 0) {glDrawArrays(GL_LINES, IZERO, (ITWO*inum_buf));};

	glEnable(GL_CULL_FACE);
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	return;
}
