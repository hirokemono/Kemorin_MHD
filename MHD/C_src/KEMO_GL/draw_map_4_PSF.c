
/* draw_map_4_PSF.c */

#include <OpenGL/gl3.h>
#include "draw_map_4_PSF.h"

void draw_map_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct kemo_array_control *psf_a, const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	set_psf_map_to_buf(ist_psf, ied_psf, psf_s, psf_a, psf_buf);
	Const_VAO_4_Simple(psf_VAO, psf_buf);
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	glGenVertexArrays(1, &psf_VAO->id_VAO);
	glBindVertexArray(psf_VAO->id_VAO);
	
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Simple_VAO(psf_VAO);
	
	return;	
}

void draw_map_PSF_isolines_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			int iflag_retina, const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int inum_line = 0;
	int num_patch = 2 * count_map_PSF_isoline(psf_s, psf_m);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	inum_line = set_map_PSF_isoline_to_buf(psf_s, psf_m, psf_buf);
	
	Const_VAO_4_Simple(psf_VAO, psf_buf);
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Simple_VAO(psf_VAO);
	
	return;
}


int draw_map_objects_VAO(struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
    int i;
    int iflag_map = 0;
	GLdouble xwin, ywin;
	GLdouble orthogonal[16];
	    
	/* set shading mode */
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
	
	
	struct gl_strided_buffer *line_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, line_buf);
	alloc_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	
	if(mesh_m->iflag_draw_coast != 0){
		draw_map_coastline_VBO(orthogonal, psf_VAO, kemo_shaders, line_buf);
	};
	if(mesh_m->iflag_draw_sph_grid != 0){
		draw_map_flame_VBO(orthogonal, psf_VAO, kemo_shaders, line_buf);
	};
	free(line_buf->v_buf);
	free(line_buf);
	
	return iflag_map;
}

