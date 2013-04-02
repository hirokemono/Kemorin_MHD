
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"

static void set_each_mesh_tri_patch(int ie_local, int iele, int shading_mode, int polygon_mode, 
								   double normal_ele[3], double normal_nod[12], struct viewer_mesh *mesh_s,
								   double f_color[4], int *inum_buf, struct buffer_for_gl *gl_buf){
	int inod, k, kr, k1, nd;
	
	for (k=0; k<ITHREE; k++) {
		if (iele < 0) {kr = ITHREE - k - 1;}
		else {kr = k;};
		k1 = mesh_s->node_quad_2_linear_sf[4*ie_local+kr] - 1;
		inod = mesh_s->ie_sf_viewer[abs(iele)-1][k1]-1;
		for(nd=0;nd<3;nd++) {gl_buf->xyz[ITHREE*(*inum_buf)+k][nd] =  mesh_s->xx_draw[inod][nd];};
		for(nd=0;nd<4;nd++) {gl_buf->rgba[ITHREE*(*inum_buf)+k][nd] = f_color[nd];};

		if (shading_mode == SMOOTH_SHADE) {
			for (nd = 0; nd < 3; nd++){gl_buf->norm[ITHREE*(*inum_buf)+k][nd] = normal_nod[3*kr+nd];};
		} else {
			for (nd = 0; nd < 3; nd++){gl_buf->norm[ITHREE*(*inum_buf)+k][nd] = normal_ele[nd];};
		}
	};
	
	if(polygon_mode == REVERSE_POLYGON){
		for (nd = 0; nd < 3; nd++){
			gl_buf->norm[ITHREE*(*inum_buf)  ][nd] = -gl_buf->norm[ITHREE*(*inum_buf)  ][nd];
			gl_buf->norm[ITHREE*(*inum_buf)+1][nd] = -gl_buf->norm[ITHREE*(*inum_buf)+1][nd];
			gl_buf->norm[ITHREE*(*inum_buf)+2][nd] = -gl_buf->norm[ITHREE*(*inum_buf)+2][nd];
		};
	}
	
	*inum_buf = *inum_buf + 1;
	if(*inum_buf>=NSIZE_GL_BUFFER){
		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*(*inum_buf)));
		*inum_buf = 0;
	}
	
	return;
};

static void set_each_mesh_quad_piece(int ie_local, int iele, int shading_mode, int polygon_mode, 
									double normal_ele[3], double normal_nod[12], struct viewer_mesh *mesh_s,
									double f_color[4], int *inum_buf, struct buffer_for_gl *gl_buf){
	int inod, k, kr, k1, nd;
	
	for (k=0; k<ITHREE; k++) {
		if (iele < 0) {kr = (IFOUR-k)%4;}
		else {kr = (k+2)%4;};
		k1 = mesh_s->node_quad_2_linear_sf[4*ie_local+kr] - 1;		
		inod = mesh_s->ie_sf_viewer[abs(iele)-1][k1]-1;
		for(nd=0;nd<3;nd++) {gl_buf->xyz[ITHREE*(*inum_buf)+k][nd] =  mesh_s->xx_draw[inod][nd];};
		for(nd=0;nd<4;nd++) {gl_buf->rgba[ITHREE*(*inum_buf)+k][nd] = f_color[nd];};
		
		if (shading_mode == SMOOTH_SHADE) {
			for (nd = 0; nd < 3; nd++){gl_buf->norm[ITHREE*(*inum_buf)+k][nd] = normal_nod[3*kr+nd];};
		} else {
			for (nd = 0; nd < 3; nd++){gl_buf->norm[ITHREE*(*inum_buf)+k][nd] = normal_ele[nd];};
		}
	};
	
	if(polygon_mode == REVERSE_POLYGON){
		for (nd = 0; nd < 3; nd++){
			gl_buf->norm[ITHREE*(*inum_buf)  ][nd] = -gl_buf->norm[ITHREE*(*inum_buf)  ][nd];
			gl_buf->norm[ITHREE*(*inum_buf)+1][nd] = -gl_buf->norm[ITHREE*(*inum_buf)+1][nd];
			gl_buf->norm[ITHREE*(*inum_buf)+2][nd] = -gl_buf->norm[ITHREE*(*inum_buf)+2][nd];
		};
	}
	
	*inum_buf = *inum_buf + 1;
	if(*inum_buf>=NSIZE_GL_BUFFER){
		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*(*inum_buf)));
		*inum_buf = 0;
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

	
	glEnable( GL_CULL_FACE );
	glShadeModel(GL_SMOOTH);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	if (polygon_mode == NORMAL_POLYGON) { 
		glPolygonMode(GL_FRONT, GL_FILL);
		glCullFace(GL_BACK);
		}
	else if(polygon_mode == REVERSE_POLYGON) { 
		glPolygonMode(GL_BACK, GL_FILL);
		glCullFace(GL_FRONT);
	};
	
	if(opacity < 1.0){
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	}
	
	if (mesh_s->nnod_4_surf != 8){
		for(i = 0; i < mesh_s->num_pe_sf; i++){
			ip = ip_domain_far[i] - 1;
			if(iflag_domain[ip] != 0){
				set_patch_color_mode_c(surface_color, color_mode, color_loop, ip, mesh_s->num_pe_sf, 
									   igrp, num_grp, opacity, single_color, f_color);
		
				ist = istack_grp[ip];
				ied = istack_grp[ip+1];
				for(icou = ist; icou < ied; icou++){
					inum = isort_grp[icou];
				
					for (j = 0; j < mesh_s->nsurf_each_sf; j++) {
						jnum = j + inum * mesh_s->nsurf_each_sf;
						set_each_mesh_tri_patch(j, item_grp[inum], shading_mode, polygon_mode,
														   normal_ele[jnum], normal_nod[jnum], mesh_s, 
														   f_color, &inum_buf, gl_buf);

						set_each_mesh_quad_piece(j, item_grp[inum], shading_mode, polygon_mode,
															normal_ele[jnum], normal_nod[jnum], mesh_s,
															f_color, &inum_buf, gl_buf);
					};
				};
			};
		};
		
	} else if (mesh_s->nnod_4_surf == 8){
		
		for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
			if(iflag_domain[ip] != 0){
				set_patch_color_mode_c(surface_color, color_mode, color_loop, ip, mesh_s->num_pe_sf, 
									   igrp, num_grp, opacity, single_color, f_color);
		
				ist = istack_grp[ip];
				ied = istack_grp[ip+1];
				for(icou = ist; icou < ied; icou++){
					inum = isort_grp[icou];

					jnum = inum * mesh_s->nsurf_each_sf;
					set_each_mesh_quad_piece(IZERO, item_grp[inum], shading_mode, polygon_mode,
														normal_ele[jnum], normal_nod[jnum], mesh_s,
														f_color, &inum_buf, gl_buf);
					
					for (j = 0; j < mesh_s->nsurf_each_sf; j++) {
						jnum = j + inum * mesh_s->nsurf_each_sf;
						set_each_mesh_tri_patch(j, item_grp[inum], shading_mode, polygon_mode,
														   normal_ele[jnum], normal_nod[jnum], mesh_s,
														   f_color, &inum_buf, gl_buf);
					};
				};
			};
		};
	};
	if(inum_buf > 0) {glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));};

	
	if(opacity < 1.0){
		glDisable(GL_MULTISAMPLE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	}
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	return;
}
