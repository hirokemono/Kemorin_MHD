
/* move_draw_objects_gl.c */

#include <OpenGL/gl3.h>
#include "move_draw_objects_gl.h"

struct kemoview_VAOs * init_kemoview_VAOs(){
	int i;
	struct kemoview_VAOs *kemo_VAOs
			= (struct kemoview_VAOs *) malloc(sizeof(struct kemoview_VAOs));
    if(kemo_VAOs == NULL){
        printf("malloc error for kemoview_VAOs\n");
        exit(0);
    }
	
	kemo_VAOs->cube_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->mesh_solid_VAO = (struct VAO_ids **) malloc(3*sizeof(struct VAO_ids));
	for(i=0;i<3;i++){
		kemo_VAOs->mesh_solid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	kemo_VAOs->mesh_trans_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->fline_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->psf_solid_VAO = (struct VAO_ids **) malloc(6*sizeof(struct VAO_ids));
	for(i=0;i<6;i++){
		kemo_VAOs->psf_solid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	kemo_VAOs->psf_trans_VAO = (struct VAO_ids **) malloc(2*sizeof(struct VAO_ids));
	for(i=0;i<2;i++){
		kemo_VAOs->psf_trans_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};

	kemo_VAOs->grid_VAO = (struct VAO_ids **) malloc(5*sizeof(struct VAO_ids));
	for(i=0;i<5;i++){
		kemo_VAOs->grid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};

	kemo_VAOs->cbar_VAO = (struct VAO_ids **) malloc(5*sizeof(struct VAO_ids));
	for(i=0;i<2;i++){
		kemo_VAOs->cbar_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	return kemo_VAOs;
};

void dealloc_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
	int i;
	free(kemo_VAOs->cube_VAO);
	
	for(i=0;i<3;i++){
		free(kemo_VAOs->mesh_solid_VAO[i]);
	};
	free(kemo_VAOs->mesh_solid_VAO);
	free(kemo_VAOs->mesh_trans_VAO);
	
	free(kemo_VAOs->fline_VAO);
	
	for(i=0;i<6;i++){
		free(kemo_VAOs->psf_solid_VAO[i]);
	};
	free(kemo_VAOs->psf_solid_VAO);
	for(i=0;i<2;i++){
		free(kemo_VAOs->psf_trans_VAO[i]);
	};
	free(kemo_VAOs->psf_trans_VAO);

	for(i=0;i<5;i++){
		free(kemo_VAOs->grid_VAO[i]);
	};
	free(kemo_VAOs->grid_VAO);

	for(i=0;i<2;i++){
		free(kemo_VAOs->cbar_VAO[i]);
	};
	free(kemo_VAOs->cbar_VAO);
	return;
};


void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage){
    glReadBuffer(GL_FRONT);
    glPixelStorei(GL_PACK_ALIGNMENT, IONE);
    glReadPixels(IZERO, IZERO, (GLsizei) num_x, (GLsizei) num_y,
                 GL_RGB, GL_UNSIGNED_BYTE,(GLubyte *) glimage);
}

void draw_objects(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct kemoview_VAOs *kemo_VAOs, struct kemoview_shaders *kemo_shaders){
	glDeleteLists(view_s->gl_drawID, 1);
	glNewList(view_s->gl_drawID, GL_COMPILE_AND_EXECUTE);
	
	glEndList();
	
	return;
}


void draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct kemoview_VAOs *kemo_VAOs, struct kemoview_shaders *kemo_shaders){
	int i;
	int iflag_psf = 0;
	
    /* Draw Solid Objects */
	
	update_projection_struct(view_s);
	rotate_view_by_struct(view_s);
//	modify_view_by_struct(view_s);
	
	if(mesh_m->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		iflag_psf = check_draw_map(psf_a);
		draw_map_objects_VAO(mesh_m, view_s, 
					kemo_VAOs->psf_solid_VAO, kemo_VAOs->grid_VAO, kemo_shaders);
	} else {
		draw_axis_VAO(view_s, kemo_VAOs->grid_VAO[2], kemo_shaders);
		draw_fieldlines_VAO(fline_m, view_s, kemo_VAOs->fline_VAO, kemo_shaders);
		
		 iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		 iflag_psf = iflag_psf + check_draw_psf(psf_a);
		 draw_PSF_solid_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a, view_s, 
									kemo_VAOs->psf_solid_VAO, kemo_shaders);
		
		if(mesh_m->iflag_draw_mesh != 0){
			draw_solid_mesh_VAO(mesh_m, view_s, 
						kemo_VAOs->mesh_solid_VAO, kemo_shaders);
		};
		
		draw_coastline_grid_VBO(view_s, kemo_VAOs->grid_VAO, kemo_shaders);
		
		draw_PSF_trans_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a, 
								   view_s, kemo_VAOs->psf_trans_VAO, kemo_shaders);
		if(mesh_m->iflag_draw_mesh != 0){
			set_trans_mesh_VAO(mesh_s, mesh_m, view_s, kemo_VAOs->mesh_trans_VAO);
			draw_trans_mesh_VAO(mesh_m, view_s, 
						kemo_VAOs->mesh_trans_VAO, kemo_shaders);
		};
	};
	
    /* Draw Color bar */
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
	};
	draw_colorbar_VAO(psf_a->cbar_wk, kemo_VAOs->cbar_VAO, kemo_shaders);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		draw_initial_cube(view_s, kemo_VAOs->cube_VAO, kemo_shaders);
	}
	return;
}

void update_draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct kemoview_VAOs *kemo_VAOs, struct kemoview_shaders *kemo_shaders){
	int i;
	int iflag_psf = 0;
	
    /* Draw Solid Objects */
	
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
		
	
	if(mesh_m->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		iflag_psf = check_draw_map(psf_a);
		set_map_objects_VAO(view_s->iflag_retina, psf_s, mesh_m, psf_m, psf_a, 
							 kemo_VAOs->psf_solid_VAO, kemo_VAOs->grid_VAO);
		draw_map_objects_VAO(mesh_m, view_s,
					kemo_VAOs->psf_solid_VAO, kemo_VAOs->grid_VAO, kemo_shaders);
	} else {
		glGenVertexArrays(1, &kemo_VAOs->grid_VAO[2]->id_VAO);
		set_axis_VAO(mesh_m, view_s, kemo_VAOs->grid_VAO[2]);
		draw_axis_VAO(view_s, kemo_VAOs->grid_VAO[2], kemo_shaders);
		
		sel_fieldlines_VAO(fline_s, fline_m, kemo_VAOs->fline_VAO);
		draw_fieldlines_VAO(fline_m, view_s, kemo_VAOs->fline_VAO, kemo_shaders);
		
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		iflag_psf = iflag_psf + check_draw_psf(psf_a);
		set_PSF_solid_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a,
								  kemo_VAOs->psf_solid_VAO);
		draw_PSF_solid_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a, view_s,
								  kemo_VAOs->psf_solid_VAO, kemo_shaders);
		
	
		if(mesh_m->iflag_draw_mesh != 0){
			set_solid_mesh_VAO(mesh_s, mesh_m, kemo_VAOs->mesh_solid_VAO);
			draw_solid_mesh_VAO(mesh_m, view_s, 
						kemo_VAOs->mesh_solid_VAO, kemo_shaders);
		};
		
		set_coastline_grid_VBO(mesh_m, kemo_VAOs->grid_VAO);
		draw_coastline_grid_VBO(view_s, kemo_VAOs->grid_VAO, kemo_shaders);
		
		/* Draw Transparent Objects */
		draw_PSF_trans_objects_VAO(mesh_m->shading_mode, 
					psf_s, psf_m, psf_a, view_s, kemo_VAOs->psf_trans_VAO, kemo_shaders);
		
		if(mesh_m->iflag_draw_mesh != 0){
			set_trans_mesh_VAO(mesh_s, mesh_m, view_s, kemo_VAOs->mesh_trans_VAO);
			draw_trans_mesh_VAO(mesh_m, view_s, 
						kemo_VAOs->mesh_trans_VAO, kemo_shaders);
		};
	};
	
	
    /* Draw Color bar */
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
	};
	
	set_colorbar_VAO(view_s->iflag_retina, view_s->nx_window, view_s->ny_window,
				mesh_m->text_color, mesh_m->bg_color, psf_m, psf_a, kemo_VAOs->cbar_VAO);
	draw_colorbar_VAO(psf_a->cbar_wk, kemo_VAOs->cbar_VAO, kemo_shaders);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		set_initial_cube_VAO(view_s, kemo_VAOs->cube_VAO);
		draw_initial_cube(view_s, kemo_VAOs->cube_VAO, kemo_shaders);
	}
	
	return;
}
