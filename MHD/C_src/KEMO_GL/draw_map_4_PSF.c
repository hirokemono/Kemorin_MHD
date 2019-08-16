
/* draw_map_4_PSF.c */

#include <OpenGL/gl3.h>
#include "draw_map_4_PSF.h"


static void set_psf_map_to_buf(int ist_psf, int num_buf, struct psf_data **psf_s, 
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
	int inum, iele, inod, ipsf, nd, k;
	double xx_tri[9], xyz_map[9];
	
	for(inum=0; inum<num_buf; inum++){
		ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
		iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s[ipsf]->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s[ipsf]->xx_viz[inod][2];
		};
		projection_patch_4_map(xx_tri, xyz_map);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			set_node_stride_VBO((ITHREE*inum+k), strided_buf);
			strided_buf->x_draw[0] = xyz_map[ITHREE*k  ];
			strided_buf->x_draw[1] = xyz_map[ITHREE*k+1];
			strided_buf->x_draw[2] = 0.0;
			
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = psf_s[ipsf]->color_nod[inod][nd];};
        };
    };
    return;
}


void draw_map_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct kemo_array_control *psf_a, const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = ied_psf - ist_psf;
	
	if(num_patch <= 0) return;
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	set_psf_map_to_buf(ist_psf, num_patch, psf_s, psf_a, psf_buf);
	
	glGenVertexArrays(1, &psf_VAO->id_VAO);
	glBindVertexArray(psf_VAO->id_VAO);
	
	glGenBuffers(1, &psf_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * psf_buf->num_nod_buf*psf_buf->ncomp_buf,
				 psf_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, psf_buf->istride,
						  (GLvoid*) (psf_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(psf_VAO);
	
	return;	
}


int draw_map_objects_VAO(struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf, struct buffer_for_gl *gl_buf){
    int i;
    int iflag_map = 0;
	GLdouble xwin, ywin;
	GLdouble orthogonal[16];
	    
	/* set shading mode */
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glDisable(GL_CULL_FACE);
	
	if(view_s->ny_window > view_s->nx_window) {
		xwin = 2.05;
		ywin = 2.05 * (GLdouble)view_s->ny_window / (GLdouble)view_s->nx_window;
	} else{
		xwin = 1.7 * (GLdouble)view_s->nx_window / (GLdouble)view_s->ny_window;
		ywin = 1.7;
	}
	
	orthogonal_glmat_c(-xwin, xwin, -ywin, ywin, -1.0, 1.0, orthogonal);
	
	/* set shading mode */
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	draw_map_patch_VAO(mesh_m->shading_mode, IZERO, psf_a->istack_solid_psf_patch,
				psf_s, psf_a, orthogonal, psf_VAO, kemo_shaders, psf_buf);
	
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_map = iflag_map + psf_a->iflag_loaded[i];
		
		if(psf_a->iflag_loaded[i] != 0){
			if( (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero) != 0){
				draw_map_PSF_isolines_VAO(psf_s[i], psf_m[i], view_s->iflag_retina,
							orthogonal, psf_VAO, kemo_shaders, psf_buf);
				
			};
		};
	};
	/*
	if(mesh_m->iflag_draw_coast != 0){
		draw_map_coast(gl_buf);
	};
	if(mesh_m->iflag_draw_sph_grid != 0){
		draw_flame_4_map(gl_buf);
	};
	load_projection_matrix(view_s);
	modify_view_by_struct(view_s);
	*/
	return iflag_map;
}

