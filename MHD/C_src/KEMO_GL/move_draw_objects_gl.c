
/* move_draw_objects_gl.c */

#include "move_draw_objects_gl.h"

struct kemoview_VAOs * init_kemoview_VAOs(void){
	int i;
	struct kemoview_VAOs *kemo_VAOs
			= (struct kemoview_VAOs *) malloc(sizeof(struct kemoview_VAOs));
    if(kemo_VAOs == NULL){
        printf("malloc error for kemoview_VAOs\n");
        exit(0);
    }
	
	kemo_VAOs->cube_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    kemo_VAOs->msg_VAO =  (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->mesh_solid_VAO = (struct VAO_ids **) malloc(3*sizeof(struct VAO_ids *));
	for(i=0;i<3;i++){
		kemo_VAOs->mesh_solid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	kemo_VAOs->mesh_trans_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->fline_VAO = (struct VAO_ids **) malloc(2*sizeof(struct VAO_ids *));
	for(i=0;i<2;i++){
		kemo_VAOs->fline_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	
	kemo_VAOs->psf_solid_VAO = (struct VAO_ids **) malloc(4*sizeof(struct VAO_ids *));
	for(i=0;i<4;i++){
		kemo_VAOs->psf_solid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	kemo_VAOs->psf_trans_VAO = (struct VAO_ids **) malloc(2*sizeof(struct VAO_ids *));
	for(i=0;i<2;i++){
		kemo_VAOs->psf_trans_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	
	kemo_VAOs->grid_VAO = (struct VAO_ids **) malloc(5*sizeof(struct VAO_ids *));
	for(i=0;i<5;i++){
		kemo_VAOs->grid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	
	kemo_VAOs->cbar_VAO = (struct VAO_ids **) malloc(3*sizeof(struct VAO_ids *));
	for(i=0;i<5;i++){
		kemo_VAOs->cbar_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
    kemo_VAOs->time_VAO =  (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->map_VAO = (struct VAO_ids **) malloc(4*sizeof(struct VAO_ids *));
	for(i=0;i<4;i++){
		kemo_VAOs->map_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	return kemo_VAOs;
};

void dealloc_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
	int i;
	free(kemo_VAOs->cube_VAO);
    free(kemo_VAOs->msg_VAO);
	
	for(i=0;i<3;i++){free(kemo_VAOs->mesh_solid_VAO[i]);};
	free(kemo_VAOs->mesh_solid_VAO);
	free(kemo_VAOs->mesh_trans_VAO);
	
	for(i=0;i<2;i++){free(kemo_VAOs->fline_VAO[i]);};
	free(kemo_VAOs->fline_VAO);
	
	for(i=0;i<4;i++){free(kemo_VAOs->psf_solid_VAO[i]);};
	free(kemo_VAOs->psf_solid_VAO);
	for(i=0;i<2;i++){free(kemo_VAOs->psf_trans_VAO[i]);};
	free(kemo_VAOs->psf_trans_VAO);

	for(i=0;i<5;i++){free(kemo_VAOs->grid_VAO[i]);};
	free(kemo_VAOs->grid_VAO);

	for(i=0;i<5;i++){free(kemo_VAOs->cbar_VAO[i]);};
	free(kemo_VAOs->cbar_VAO);
	free(kemo_VAOs->time_VAO);

	for(i=0;i<4;i++){free(kemo_VAOs->map_VAO[i]);};
	free(kemo_VAOs->map_VAO);
	return;
};

void assign_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
    int i;
    for(i=0;i<3;i++){glGenVertexArrays(1, &kemo_VAOs->grid_VAO[i]->id_VAO);};
    for(i=0;i<2;i++){glGenVertexArrays(1, &kemo_VAOs->fline_VAO[i]->id_VAO);};
    for(i=0;i<4;i++){glGenVertexArrays(1, &kemo_VAOs->psf_solid_VAO[i]->id_VAO);};
    for(i=0;i<3;i++){glGenVertexArrays(1, &kemo_VAOs->mesh_solid_VAO[i]->id_VAO);};
    for(i=0;i<2;i++){glGenVertexArrays(1, &kemo_VAOs->psf_trans_VAO[i]->id_VAO);};
	glGenVertexArrays(1, &kemo_VAOs->mesh_trans_VAO->id_VAO);
    for(i=0;i<5;i++){glGenVertexArrays(1, &kemo_VAOs->cbar_VAO[i]->id_VAO);};
	glGenVertexArrays(1, &kemo_VAOs->time_VAO->id_VAO);
    for(i=0;i<4;i++){glGenVertexArrays(1, &kemo_VAOs->map_VAO[i]->id_VAO);};
    glGenVertexArrays(1, &kemo_VAOs->cube_VAO->id_VAO);
    glGenVertexArrays(1, &kemo_VAOs->msg_VAO->id_VAO);
};

void clear_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
    int i;
    for(i=0;i<3;i++){Destroy_VAO(kemo_VAOs->grid_VAO[i]);};
    for(i=0;i<2;i++){Destroy_VAO(kemo_VAOs->fline_VAO[i]);};
    for(i=0;i<4;i++){Destroy_VAO(kemo_VAOs->psf_solid_VAO[i]);};
    for(i=0;i<3;i++){Destroy_VAO(kemo_VAOs->mesh_solid_VAO[i]);};
    for(i=0;i<2;i++){Destroy_VAO(kemo_VAOs->psf_trans_VAO[i]);};
	Destroy_VAO(kemo_VAOs->mesh_trans_VAO);
    for(i=0;i<3;i++){Destroy_VAO(kemo_VAOs->cbar_VAO[i]);};
	Destroy_VAO(kemo_VAOs->time_VAO);
    for(i=0;i<4;i++){Destroy_VAO(kemo_VAOs->map_VAO[i]);};
    Destroy_VAO(kemo_VAOs->cube_VAO);
    Destroy_VAO(kemo_VAOs->msg_VAO);
};

void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage){
    glReadBuffer(GL_FRONT);
    glPixelStorei(GL_PACK_ALIGNMENT, IONE);
    glReadPixels(IZERO, IZERO, (GLsizei) num_x, (GLsizei) num_y,
                 GL_RGB, GL_UNSIGNED_BYTE,(GLubyte *) glimage);
}

static void quick_draw_objects(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline, 
                               struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                               struct kemoview_VAOs *kemo_VAOs, struct kemoview_shaders *kemo_shaders){
	int iflag_psf = 0;
    /* Set transfer matrices */
    double *orthogonal = orthogonal_projection_mat_c(0.0, view_s->nx_frame,
                                                     0.0, view_s->ny_frame,
                                                     -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(view_s);
    struct transfer_matrices *map_matrices = init_projection_matrix_for_map(view_s->nx_frame, view_s->ny_frame);
    free(orthogonal);

/* Draw Solid Objects */
	if(view_s->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a, view_s);
		draw_map_objects_VAO(map_matrices, kemo_VAOs->map_VAO, kemo_shaders);
	} else {
        glDisable(GL_CULL_FACE);
		drawgl_patch_with_phong(view_matrices, kemo_VAOs->grid_VAO[2], kemo_shaders);
		drawgl_lines(view_matrices, kemo_VAOs->fline_VAO[1], kemo_shaders);

		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, view_s);
		draw_PSF_solid_objects_VAO(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                                   view_matrices, kemo_VAOs->psf_solid_VAO, kemo_shaders);
		
/*  Draw mesh data */
        drawgl_lines(view_matrices, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);

        glDisable(GL_CULL_FACE);
		drawgl_patch_with_phong(view_matrices, kemo_VAOs->mesh_solid_VAO[2], kemo_shaders);
		draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_matrices, 
                            kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);

		drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[0], kemo_shaders);
		drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[1], kemo_shaders);

/*  Stop drawing transparent objects  */
		draw_PSF_trans_objects_VAO(kemo_psf->psf_m, kemo_psf->psf_a, view_matrices, 
                                   kemo_VAOs->psf_trans_VAO, kemo_shaders);
		if(kemo_mesh->mesh_m->iflag_draw_mesh != 0){
			draw_trans_mesh_VAO(view_matrices, kemo_VAOs->mesh_trans_VAO, kemo_shaders);
		};
	};
	
    /* Draw Color bar and time label*/
	draw_colorbar_VAO(kemo_psf->psf_a->cbar_wk, &kemo_VAOs->cbar_VAO[0],
                      cbar_matrices, kemo_shaders);
	draw_timelabel_VAO(kemo_psf->psf_a->tlabel_wk, kemo_VAOs->time_VAO,
                       cbar_matrices, kemo_shaders);
	
/* Draw message */
    if(kemo_mesh->msg_wk->message_opacity > 0.0){
	    draw_message_VAO(kemo_mesh->msg_wk, kemo_VAOs->msg_VAO, 
                         cbar_matrices, kemo_shaders);
    };

/* draw example cube for empty data */
	if(kemo_VAOs->cube_VAO->npoint_draw > 0){
        struct initial_cube_lighting *init_light = init_inital_cube_lighting();
		draw_initial_cube(view_matrices, init_light, kemo_VAOs->cube_VAO, kemo_shaders);
	}
    free(map_matrices);
    free(view_matrices);
    free(cbar_matrices);
	return;
}

static void update_draw_objects(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline, 
			struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
			struct kemoview_VAOs *kemo_VAOs, struct kemoview_shaders *kemo_shaders){
    int i;
	int iflag;
	int iflag_psf = 0;

/* Set transfer matrices */
    double *orthogonal = orthogonal_projection_mat_c(0.0, view_s->nx_frame,
                                                     0.0, view_s->ny_frame,
                                                     -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(view_s);
    struct transfer_matrices *map_matrices = init_projection_matrix_for_map(view_s->nx_frame, view_s->ny_frame);
    free(orthogonal);

/* Draw Solid Objects */
	
	if(view_s->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, view_s);
		iflag_psf = check_draw_map(kemo_psf->psf_a);
		set_map_objects_VAO(view_s, kemo_psf->psf_d, 
                            kemo_mesh->mesh_m, kemo_psf->psf_m, kemo_psf->psf_a, kemo_VAOs->map_VAO);
        map_coastline_grid_VBO(kemo_mesh->mesh_m, &kemo_VAOs->map_VAO[2]);
        
		draw_map_objects_VAO(map_matrices, kemo_VAOs->map_VAO, kemo_shaders);
	} else {
		set_axis_VAO(kemo_mesh->mesh_m, view_s, kemo_VAOs->grid_VAO[2]);
        glDisable(GL_CULL_FACE);
		drawgl_patch_with_phong(view_matrices, kemo_VAOs->grid_VAO[2], kemo_shaders);
		
		sel_fieldlines_VAO(kemo_fline->fline_d, kemo_fline->fline_m, kemo_VAOs->fline_VAO);
		if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
            glDisable(GL_CULL_FACE);
			drawgl_patch_with_phong(view_matrices, kemo_VAOs->fline_VAO[0], kemo_shaders);
		} else {
			drawgl_lines(view_matrices, kemo_VAOs->fline_VAO[1], kemo_shaders);
		};

 		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
                                                kemo_psf->psf_a, view_s);
		iflag_psf = iflag_psf + check_draw_psf(kemo_psf->psf_a);
		set_PSF_solid_objects_VAO(view_s, kemo_psf->psf_d, kemo_psf->psf_m,
                                  kemo_psf->psf_a, kemo_VAOs->psf_solid_VAO);
		draw_PSF_solid_objects_VAO(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a, 
                                   view_matrices, kemo_VAOs->psf_solid_VAO, kemo_shaders);
		draw_PSF_isolines_VAO(view_matrices, kemo_VAOs->psf_solid_VAO, kemo_shaders);
	
		if(kemo_mesh->mesh_m->iflag_draw_mesh != 0){
			set_solid_mesh_VAO(kemo_mesh->mesh_d, kemo_mesh->mesh_m,
						view_s, kemo_VAOs->mesh_solid_VAO);

			drawgl_lines(view_matrices, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);
            
            glDisable(GL_CULL_FACE);
			drawgl_patch_with_phong(view_matrices, kemo_VAOs->mesh_solid_VAO[2], kemo_shaders);
			draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_matrices, 
						kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);
		} else {
			kemo_VAOs->mesh_solid_VAO[0]->npoint_draw = 0;
			kemo_VAOs->mesh_solid_VAO[1]->npoint_draw = 0;
			kemo_VAOs->mesh_solid_VAO[2]->npoint_draw = 0;
		};
		
		set_coastline_grid_VBO(kemo_mesh->mesh_m, kemo_VAOs->grid_VAO);
		drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[0], kemo_shaders);
		drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[1], kemo_shaders);

		/* Draw Transparent Objects */
		set_PSF_trans_objects_VAO(view_s, kemo_psf->psf_d, 
                                  kemo_psf->psf_m, kemo_psf->psf_a,
                                  kemo_VAOs->psf_trans_VAO);
		draw_PSF_trans_objects_VAO(kemo_psf->psf_m, kemo_psf->psf_a, view_matrices, 
                                   kemo_VAOs->psf_trans_VAO, kemo_shaders);
		
		if(kemo_mesh->mesh_m->iflag_draw_mesh != 0){
			set_trans_mesh_VAO(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
						kemo_VAOs->mesh_trans_VAO);
			draw_trans_mesh_VAO(view_matrices, kemo_VAOs->mesh_trans_VAO, kemo_shaders);
		};
	};
	
	
/* Draw Color bar and time label*/
	for(i=0;i<kemo_psf->psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + kemo_psf->psf_a->iflag_loaded[i];
	};
	
	
	set_colorbar_VAO(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
				kemo_mesh->mesh_m->text_color, kemo_mesh->mesh_m->bg_color, 
				kemo_psf->psf_m, kemo_psf->psf_a,
				&kemo_VAOs->cbar_VAO[0]);
	set_timelabel_VAO(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
					  kemo_mesh->mesh_m->text_color, kemo_mesh->mesh_m->bg_color, 
					  kemo_psf->psf_m, kemo_psf->psf_a,
					  kemo_VAOs->time_VAO);

    draw_colorbar_VAO(kemo_psf->psf_a->cbar_wk, &kemo_VAOs->cbar_VAO[0], 
                      cbar_matrices, kemo_shaders);
	draw_timelabel_VAO(kemo_psf->psf_a->tlabel_wk, kemo_VAOs->time_VAO,
                       cbar_matrices, kemo_shaders);

	/* Draw message */
    if(kemo_mesh->msg_wk->message_opacity > 0.0){
        struct gl_strided_buffer *cbar_buf
            = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	    set_message_VAO(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                        kemo_mesh->msg_wk, kemo_VAOs->msg_VAO, cbar_buf);
        draw_message_VAO(kemo_mesh->msg_wk, kemo_VAOs->msg_VAO,
                         cbar_matrices, kemo_shaders);
        free(cbar_buf->v_buf);
        free(cbar_buf);
    };

    /* draw example cube for empty data */
	iflag = kemo_mesh->mesh_m->iflag_draw_mesh + iflag_psf + kemo_fline->fline_m->iflag_draw_fline;
	if(iflag == 0){
        struct initial_cube_lighting *init_light = init_inital_cube_lighting();
        struct gl_index_buffer *cube_index_buf = alloc_gl_index_buffer(12, 3);
        struct gl_strided_buffer *cube_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
        const_initial_cube_buffer(cube_buf, cube_index_buf);
        set_initial_cube_VAO(cube_buf, cube_index_buf, kemo_VAOs->cube_VAO);
		draw_initial_cube(view_matrices, init_light, kemo_VAOs->cube_VAO, kemo_shaders);
        free(cube_index_buf);
        free(cube_buf);
	} else {
		kemo_VAOs->cube_VAO->npoint_draw = 0;
	}
    free(view_matrices);
    free(map_matrices);
    free(cbar_matrices);
	return;
}


void quick_draw_objects_gl3(struct kemoviewer_type *kemoview){
	quick_draw_objects(kemoview->kemo_psf, kemoview->kemo_fline, 
				kemoview->kemo_mesh, kemoview->view_s, 
				kemoview->kemo_VAOs, kemoview->kemo_shaders);
	return;
};

void update_draw_objects_gl3(struct kemoviewer_type *kemoview){
	update_draw_objects(kemoview->kemo_psf, kemoview->kemo_fline,
				kemoview->kemo_mesh, kemoview->view_s, 
				kemoview->kemo_VAOs, kemoview->kemo_shaders);
	return;
}
