
/* draw_node_by_ico_c.c */

#include "draw_node_by_ico_c.h"


void draw_node_by_ico(int num_grp, int igrp, int *istack_grp, int *item_grp,
			struct viewer_mesh *mesh_s, double node_diam,
			int node_color, int color_mode, int color_loop, GLfloat single_color[4],
			int *iflag_domain, struct buffer_for_gl *gl_buf){
	double f_color[4];
	double xyz_patch[180], norm_patch[180];
	int inum_buf, num_ico;
	int i, nd, ip, inod, inum, ist, ied;

	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
	
	glShadeModel(GL_SMOOTH);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glCullFace(GL_BACK);
	
	set_node_color_mode_c(node_color, color_mode, color_loop, igrp, num_grp, single_color);

	inum_buf = 0;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			set_patch_color_mode_c(node_color, color_mode, color_loop, (ip+1),
				mesh_s->num_pe_sf, igrp, num_grp, ONE, single_color, f_color);
            printf("f_color %e %e %e %e \n", f_color[0],f_color[1], f_color[2], f_color[3]);
			
			for(inum = ist; inum < ied; inum++){
				inod = item_grp[inum]-1;
				num_ico = set_icosahedron_patch(node_diam, &mesh_s->xx_draw[inod][0],
												 xyz_patch, norm_patch);

				for (i=0; i<num_ico; i++) {
					for(nd=0;nd<3;nd++){gl_buf->xyz[inum_buf+i][nd] =  xyz_patch[3*i+nd];};
					for(nd=0;nd<3;nd++){gl_buf->norm[inum_buf+i][nd] = norm_patch[3*i+nd];};
					for(nd=0;nd<4;nd++){gl_buf->rgba[inum_buf+i][nd] = f_color[nd];};
				};
				
				inum_buf = inum_buf + num_ico;
				if( (inum_buf+num_ico) >= NSIZE_GL_BUFFER){
					glDrawArrays(GL_TRIANGLES, IZERO, inum_buf);
					inum_buf = 0;
				};
			};
		}
	};
	if(inum_buf > 0){
            glDrawArrays(GL_TRIANGLES, IZERO, inum_buf);
     };

	glEnable(GL_CULL_FACE);

	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	return;
}

