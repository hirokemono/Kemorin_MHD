
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"

static void set_each_mesh_tri_patch(int ie_local, int iele, int shading_mode, int polygon_mode, 
			double **xx_draw, int **ie_sf_viewer, int *node_quad_2_linear_tri, 
			double normal_ele[3], double normal_nod[9], 
			double f_color[4], int inum_buf, struct buffer_for_gl *gl_buf){
	int inod, k, kr, k1, nd;
	
	for (k=0; k<ITHREE; k++) {
		if (iele < 0) {kr = ITHREE - k - 1;}
		else {kr = k;};
		k1 = node_quad_2_linear_tri[3*ie_local+kr] - 1;
		inod = ie_sf_viewer[abs(iele)-1][k1]-1;
		for(nd=0;nd<3;nd++) {gl_buf->xyz[ITHREE*inum_buf+k][nd] = xx_draw[inod][nd];};
		for(nd=0;nd<4;nd++) {gl_buf->rgba[ITHREE*inum_buf+k][nd] = f_color[nd];};

		if (shading_mode == SMOOTH_SHADE) {
			for (nd = 0; nd < 3; nd++){gl_buf->norm[ITHREE*inum_buf+k][nd] = normal_nod[3*kr+nd];};
		} else {
			for (nd = 0; nd < 3; nd++){gl_buf->norm[ITHREE*inum_buf+k][nd] = normal_ele[nd];};
		}
	};
	
	if(polygon_mode == REVERSE_POLYGON){
		for (nd = 0; nd < 3; nd++){
			gl_buf->norm[ITHREE*inum_buf  ][nd] = -gl_buf->norm[ITHREE*inum_buf  ][nd];
			gl_buf->norm[ITHREE*inum_buf+1][nd] = -gl_buf->norm[ITHREE*inum_buf+1][nd];
			gl_buf->norm[ITHREE*inum_buf+2][nd] = -gl_buf->norm[ITHREE*inum_buf+2][nd];
		};
	}
	
	return;
};

void draw_mesh_patch(int shading_mode, int polygon_mode, int surface_color,
			int color_mode, int color_loop, double opacity, GLfloat single_color[4], 
			int num_grp, int *istack_grp, int *item_grp, 
			double **normal_ele, double **normal_nod, int *isort_grp, 
			int *ip_domain_far, int igrp, struct viewer_mesh *mesh_s, 
			int *iflag_domain, struct buffer_for_gl *gl_buf){
	int i, ip, icou, inum, ist, ied, j, jnum;
	int inum_buf = 0;
	double f_color[4];
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
	
	for(i = 0; i < mesh_s->num_pe_sf; i++){
		ip = ip_domain_far[i] - 1;
		if(iflag_domain[ip] != 0){
			set_patch_color_mode_c(surface_color, color_mode, color_loop, ip, mesh_s->num_pe_sf, 
						igrp, num_grp, opacity, single_color, f_color);
			
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(icou = ist; icou < ied; icou++){
				inum = isort_grp[icou];
				
				for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
					jnum = j + inum * mesh_s->nsurf_each_tri;
					/*
					printf("%d, %f %f %f \n", jnum, normal_ele[jnum][0],
								normal_ele[jnum][1], normal_ele[jnum][2]);
					 */
					set_each_mesh_tri_patch(j, item_grp[inum], shading_mode, polygon_mode,
								mesh_s->xx_draw, mesh_s->ie_sf_viewer, mesh_s->node_quad_2_linear_tri, 
								normal_ele[jnum], normal_nod[jnum], 
								f_color, inum_buf, gl_buf);
					
					inum_buf = inum_buf + 1;
					if(inum_buf>=NSIZE_GL_BUFFER){
						glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));
						inum_buf = 0;
					}
				};
			};
		};
	};
	
	if(inum_buf > 0) {glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));};
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);

	return;
}
