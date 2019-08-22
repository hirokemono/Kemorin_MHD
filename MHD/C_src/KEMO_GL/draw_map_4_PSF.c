
/* draw_map_4_PSF.c */

#include "draw_map_4_PSF.h"

void set_map_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct kemo_array_control *psf_a,
			struct VAO_ids *psf_VAO, struct gl_strided_buffer *map_buf){
	int num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, map_buf);
	resize_strided_buffer(map_buf->num_nod_buf, map_buf->ncomp_buf, map_buf);
	
	set_psf_map_to_buf(ist_psf, ied_psf, psf_s, psf_a, map_buf);
	
	glBindVertexArray(psf_VAO->id_VAO);
	Const_VAO_4_Simple(psf_VAO, map_buf);
	glBindVertexArray(0);
	
	return;	
}

void set_map_PSF_isolines_VAO(struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, int iflag_retina,
			struct VAO_ids *psf_VAO, struct gl_strided_buffer *map_buf){
	int ncorner = 6;
	int i, iflag;
	int inum_buf;
	int num_patch = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
		if(iflag > 0){
			num_patch = num_patch + count_map_PSF_isoline(ncorner, psf_s[i], psf_m[i]);
		};
	};
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(psf_VAO->npoint_draw <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, map_buf);
	resize_strided_buffer(map_buf->num_nod_buf, map_buf->ncomp_buf, map_buf);
	
	inum_buf = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
		if(iflag > 0){
			inum_buf = set_map_PSF_isoline_to_buf(inum_buf, ncorner, 
						psf_s[i], psf_m[i], map_buf);
		};
	};
	
	glBindVertexArray(psf_VAO->id_VAO);
	Const_VAO_4_Simple(psf_VAO, map_buf);
	glBindVertexArray(0);
	return;
}

int check_draw_map(struct kemo_array_control *psf_a){
	int i;
    int iflag_map = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_map = iflag_map + psf_a->iflag_loaded[i];
	};
	return iflag_map;
};

void set_map_objects_VAO(int iflag_retina, 
						 struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
						 struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
						 struct VAO_ids **psf_VAO, struct VAO_ids **grid_VAO){
	set_color_code_for_psfs(psf_s, psf_m, psf_a);
	
	struct gl_strided_buffer *map_buf
				= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, map_buf);
	alloc_strided_buffer(map_buf->num_nod_buf, map_buf->ncomp_buf, map_buf);
	
	set_map_patch_VAO(mesh_m->shading_mode, IZERO, psf_a->istack_solid_psf_patch, 
					  psf_s, psf_a, psf_VAO[0], map_buf);
	
	set_map_PSF_isolines_VAO(psf_s, psf_m, psf_a, iflag_retina,
							psf_VAO[1], map_buf);
	
	map_coastline_grid_VBO(mesh_m, grid_VAO, map_buf);
	free(map_buf->v_buf);
	free(map_buf);
	
	return;
};

void draw_map_objects_VAO(struct mesh_menu_val *mesh_m, struct view_element *view_s, 
			struct VAO_ids **psf_VAO, struct VAO_ids **grid_VAO,
			struct kemoview_shaders *kemo_shaders){
	GLdouble xwin, ywin;
	GLdouble orthogonal[16];
	
	if(view_s->ny_window > view_s->nx_window) {
		xwin = 2.05;
		ywin = 2.05 * (GLdouble)view_s->ny_window / (GLdouble)view_s->nx_window;
	} else{
		xwin = 1.7 * (GLdouble)view_s->nx_window / (GLdouble)view_s->ny_window;
		ywin = 1.7;
	}
	
	orthogonal_glmat_c(-xwin, xwin, -ywin, ywin, -1.0, 1.0, orthogonal);
	
	
	/* set shading mode */
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		glUseProgram(kemo_shaders->test->programId);
		map_matrix_to_shader(kemo_shaders->test, orthogonal);
		
	if(psf_VAO[0]->npoint_draw > 0){
		glBindVertexArray(psf_VAO[0]->id_VAO);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO[0]->npoint_draw);
	};
	if(psf_VAO[1]->npoint_draw > 0){
		glBindVertexArray(psf_VAO[1]->id_VAO);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO[1]->npoint_draw);
	};
	
	if(grid_VAO[0]->npoint_draw > 0){
		glBindVertexArray(grid_VAO[0]->id_VAO);
		glDrawArrays(GL_LINES, IZERO, grid_VAO[0]->npoint_draw);
	};
	if(grid_VAO[1]->npoint_draw > 0){
		glBindVertexArray(grid_VAO[1]->id_VAO);
		glDrawArrays(GL_LINES, IZERO, grid_VAO[1]->npoint_draw);
	};
	return;
}

