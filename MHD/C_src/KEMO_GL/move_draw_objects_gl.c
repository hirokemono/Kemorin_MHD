
/* move_draw_objects_gl.c */

#include <OpenGL/gl3.h>
#include "move_draw_objects_gl.h"

static int draw_solid_objects_4_psf(struct psf_data **psf_s, struct psf_menu_val **psf_m,
                             struct kemo_array_control *psf_a, struct view_element *view_s, 
                             struct buffer_for_gl *gl_buf){
    int i;
    int iflag_psf = 0;
    
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
        if(psf_a->iflag_loaded[i] != 0){
			
			if(psf_m[i]->draw_psf_vect  != 0){
				draw_arrow_4_PSF(psf_s[i], psf_m[i], gl_buf);
			};
			
			if( (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero) != 0){
				draw_PSF_isoline(psf_s[i], psf_m[i], gl_buf,
							view_s->iflag_retina);
			};
		};
	};
	
	return iflag_psf;
}

static void draw_solid_patch_4_psf(struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
                           struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
                           struct buffer_for_gl *gl_buf){
	glEnable(GL_TEXTURE_2D);
	draw_texure_4_PSF(mesh_m->shading_mode, 
				IZERO, psf_a->istack_solid_psf_txtur, 
				psf_s, psf_m, psf_a, gl_buf);
	glDisable(GL_TEXTURE_2D);
	
	draw_patch_4_PSF(mesh_m->shading_mode, psf_a->istack_solid_psf_txtur, psf_a->istack_solid_psf_patch, 
				psf_s, psf_m, psf_a, gl_buf);
    return;
}

static void draw_transparent_patch_4_psf(struct psf_data **psf_s, 
                                    struct mesh_menu_val *mesh_m, struct psf_menu_val **psf_m, 
                                    struct kemo_array_control *psf_a, struct view_element *view_s, struct buffer_for_gl *gl_buf){
    
    glEnable(GL_MULTISAMPLE);
    glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glDepthMask(GL_FALSE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	glEnable(GL_TEXTURE_2D);
    draw_texure_4_PSF(mesh_m->shading_mode, 
				psf_a->istack_solid_psf_patch, psf_a->istack_trans_psf_txtur, 
				psf_s, psf_m, psf_a, gl_buf);
	glDisable(GL_TEXTURE_2D);
	
	draw_patch_4_PSF(mesh_m->shading_mode,
				psf_a->istack_trans_psf_txtur, psf_a->ntot_psf_patch,
				psf_s, psf_m, psf_a, gl_buf);
	glEnable(GL_CULL_FACE);
	
    glDisable(GL_BLEND);
    glDepthMask(GL_TRUE);
    glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glDisable(GL_MULTISAMPLE);
    
    return;
}


void draw_objects(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
				  struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
				  struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
				  struct fline_menu_val *fline_m, struct view_element *view_s,
				  struct buffer_for_gl *gl_buf, struct gl_strided_buffer *strided_buf,
				  struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
	int i, iflag;
	int iflag_psf = 0;
	
	glDeleteLists(view_s->gl_drawID, 1);
	glNewList(view_s->gl_drawID, GL_COMPILE_AND_EXECUTE);
	
    /* Draw Solid Objects */
	
	glEnable(GL_COLOR_MATERIAL);
	if(mesh_m->iflag_draw_axis != 0){
		glShadeModel(GL_SMOOTH);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		glDisable(GL_CULL_FACE);
		draw_axis(view_s, (GLfloat) mesh_m->dist_domains);
		glEnable(GL_CULL_FACE);
	}
	
	if(fline_m->iflag_draw_fline != 0){
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		if(fline_m->fieldline_type == IFLAG_PIPE){
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glDisable(GL_CULL_FACE);
			glShadeModel(GL_SMOOTH);
			
			draw_fieldtubes_c(fline_s, fline_m, gl_buf);
			glEnable(GL_CULL_FACE);
		} else {
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			
			draw_fieldlines_c(fline_s, fline_m, gl_buf);
		};
	};
    
    if(mesh_m->iflag_view_type == VIEW_MAP) {
        set_color_code_for_psfs(psf_s, psf_m, psf_a);
        iflag_psf = draw_objects_4_map(psf_s, mesh_m, psf_m, psf_a, view_s, gl_buf);
    } else {
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		set_color_code_for_psfs(psf_s, psf_m, psf_a);
		
	/* set shading mode */
		glShadeModel(GL_SMOOTH);
		glDisable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		
		draw_solid_patch_4_psf(psf_s, mesh_m, psf_m, psf_a, gl_buf);
		
		iflag = draw_solid_objects_4_psf(psf_s, psf_m, psf_a, view_s, gl_buf);
		glEnable(GL_CULL_FACE);
		
		iflag_psf = iflag_psf + iflag;
	};
    
	
	if(mesh_m->iflag_draw_mesh != 0){
		glDisable(GL_CULL_FACE);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		
		draw_grids_4_domain(mesh_s, mesh_m, gl_buf);
		
		
		glShadeModel(GL_SMOOTH);
		glEnable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT, GL_FILL);
		
		draw_nodes_4_domain(mesh_s, mesh_m, gl_buf);
		
		if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
			glPolygonMode(GL_FRONT, GL_FILL);
			glCullFace(GL_BACK);
		}
		else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
			glPolygonMode(GL_BACK, GL_FILL);
			glCullFace(GL_FRONT);
		};
	
		draw_patches_4_domain(mesh_s, mesh_m, gl_buf);
	};
    
	if(mesh_m->iflag_view_type != VIEW_MAP) {
		if(mesh_m->iflag_draw_coast != 0)   {draw_coastline(mesh_m->radius_coast, gl_buf);};
		if(mesh_m->iflag_draw_sph_grid != 0){draw_sph_flame(mesh_m->radius_coast, gl_buf);};
	};
	
    /* Draw Transparent Objects */
	
    if(mesh_m->iflag_view_type != VIEW_MAP) {
        draw_transparent_patch_4_psf(psf_s, mesh_m, psf_m, psf_a, view_s, gl_buf);
    };
	
	if(mesh_m->iflag_draw_mesh != 0){
		glShadeModel(GL_SMOOTH);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		
		glEnable( GL_CULL_FACE );
		if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
			glPolygonMode(GL_FRONT, GL_FILL);
			glCullFace(GL_BACK);
		}
		else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
			glPolygonMode(GL_BACK, GL_FILL);
			glCullFace(GL_FRONT);
		};
		
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDepthMask(GL_FALSE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		/*glBlendFunc(GL_SRC_ALPHA, GL_ONE);*/
	
		draw_transparent_4_domain(mesh_s, mesh_m, view_s, gl_buf);
		
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	};
	
	
    /* Draw Color bar */
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
		if(psf_a->iflag_loaded[i] != 0){
			if(psf_m[i]->draw_psf_cbar > 0) {
				draw_colorbar_gl(view_s->iflag_retina,
                                 view_s->nx_window, view_s->ny_window,
                                 mesh_m->text_color, mesh_m->bg_color, psf_m[i]->cmap_psf);
				load_projection_matrix(view_s);
				rotate_view_by_struct(view_s);
			};
		};
	};
	
	glEnable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glDisable(GL_COLOR_MATERIAL);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		
		glEnable(GL_CULL_FACE);
		glEnable(GL_COLOR_MATERIAL);
		glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
		
//		drawCube_Element2(0.5f, strided_buf, cube_VAO);
		drawCube_flat(0.5f, strided_buf, cube_VAO);
		
		glDisable(GL_COLOR_MATERIAL);
	}
	
	glEndList();
	
	return;
}


void draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
				  struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
				  struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
				  struct fline_menu_val *fline_m, struct view_element *view_s,
				  struct buffer_for_gl *gl_buf, struct gl_strided_buffer *strided_buf,
				  struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
	int i, iflag;
	int iflag_psf = 0;
	
    /* Draw Solid Objects
	
	glEnable(GL_COLOR_MATERIAL);
	if(mesh_m->iflag_draw_axis != 0){
		glShadeModel(GL_SMOOTH);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		glDisable(GL_CULL_FACE);
		draw_axis(view_s, (GLfloat) mesh_m->dist_domains);
		glEnable(GL_CULL_FACE);
	}
	
	if(fline_m->iflag_draw_fline != 0){
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		if(fline_m->fieldline_type == IFLAG_PIPE){
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glDisable(GL_CULL_FACE);
			glShadeModel(GL_SMOOTH);
			
			draw_fieldtubes_c(fline_s, fline_m, gl_buf);
			glEnable(GL_CULL_FACE);
		} else {
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			
			draw_fieldlines_c(fline_s, fline_m, gl_buf);
		};
	};
    
    if(mesh_m->iflag_view_type == VIEW_MAP) {
        set_color_code_for_psfs(psf_s, psf_m, psf_a);
        iflag_psf = draw_objects_4_map(psf_s, mesh_m, psf_m, psf_a, view_s, gl_buf);
    } else {
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		set_color_code_for_psfs(psf_s, psf_m, psf_a);
		
	/* set shading mode
		glShadeModel(GL_SMOOTH);
		glDisable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		
		draw_solid_patch_4_psf(psf_s, mesh_m, psf_m, psf_a, gl_buf);
		
		iflag = draw_solid_objects_4_psf(psf_s, psf_m, psf_a, view_s, gl_buf);
		glEnable(GL_CULL_FACE);
		
		iflag_psf = iflag_psf + iflag;
	};
    
	
	if(mesh_m->iflag_draw_mesh != 0){
		glDisable(GL_CULL_FACE);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		
		draw_grids_4_domain(mesh_s, mesh_m, gl_buf);
		
		
		glShadeModel(GL_SMOOTH);
		glEnable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT, GL_FILL);
		
		draw_nodes_4_domain(mesh_s, mesh_m, gl_buf);
		
		if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
			glPolygonMode(GL_FRONT, GL_FILL);
			glCullFace(GL_BACK);
		}
		else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
			glPolygonMode(GL_BACK, GL_FILL);
			glCullFace(GL_FRONT);
		};
	
		draw_patches_4_domain(mesh_s, mesh_m, gl_buf);
	};
    
	if(mesh_m->iflag_view_type != VIEW_MAP) {
		if(mesh_m->iflag_draw_coast != 0)   {draw_coastline(mesh_m->radius_coast, gl_buf);};
		if(mesh_m->iflag_draw_sph_grid != 0){draw_sph_flame(mesh_m->radius_coast, gl_buf);};
	};
	
    /* Draw Transparent Objects
	
    if(mesh_m->iflag_view_type != VIEW_MAP) {
        draw_transparent_patch_4_psf(psf_s, mesh_m, psf_m, psf_a, view_s, gl_buf);
    };
	
	if(mesh_m->iflag_draw_mesh != 0){
		glShadeModel(GL_SMOOTH);
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		
		glEnable( GL_CULL_FACE );
		if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
			glPolygonMode(GL_FRONT, GL_FILL);
			glCullFace(GL_BACK);
		}
		else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
			glPolygonMode(GL_BACK, GL_FILL);
			glCullFace(GL_FRONT);
		};
		
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDepthMask(GL_FALSE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
		draw_transparent_4_domain(mesh_s, mesh_m, view_s, gl_buf);
		
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	};
	
	
    /* Draw Color bar
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
		if(psf_a->iflag_loaded[i] != 0){
			if(psf_m[i]->draw_psf_cbar > 0) {
				draw_colorbar_gl(view_s->iflag_retina,
                                 view_s->nx_window, view_s->ny_window,
                                 mesh_m->text_color, mesh_m->bg_color, psf_m[i]->cmap_psf);
				load_projection_matrix(view_s);
				rotate_view_by_struct(view_s);
			};
		};
	};
	
	glEnable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glDisable(GL_COLOR_MATERIAL);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		
		update_projection_struct(view_s);
		modify_view_by_struct(view_s);
		
		glUseProgram(kemo_shaders->test->programId);
		
		transfer_matrix_to_shader(kemo_shaders->test, view_s);
		
		struct gl_strided_buffer *gl_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		gl_buf->num_nod_buf = 8;
		gl_buf->ncomp_buf = 12;
		
		gl_buf->ist_xyz =  0;
		gl_buf->ist_norm = 3;
		gl_buf->ist_tex =  6;
		gl_buf->ist_csurf = 8;
		gl_buf->v_buf = (GLfloat *) malloc(gl_buf->num_nod_buf*gl_buf->ncomp_buf*sizeof(GLfloat));
		
		set_cubeVBO(0.5f, cube_VAO, gl_buf);
		
		glBindVertexArray(cube_VAO->id_VAO);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
		glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);
		
		free(gl_buf->v_buf);
		free(gl_buf);
//		DestroyVBO(cube_VAO);
	}
	
	return;
}
