
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
    return;
}

static void quick_draw_objects(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline, 
                               struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                               struct phong_lights *lights, struct kemoview_VAOs *kemo_VAOs,
                               struct kemoview_shaders *kemo_shaders){
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
		drawgl_patch_with_phong(view_matrices, lights, kemo_shaders, kemo_VAOs->grid_VAO[2]);
		drawgl_lines(view_matrices, kemo_VAOs->fline_VAO[1], kemo_shaders);

		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, view_s);
		draw_PSF_solid_objects_VAO(view_matrices, lights,
                                   kemo_VAOs->psf_solid_VAO, kemo_shaders);
		
/*  Draw mesh data */
        drawgl_lines(view_matrices, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);

        glDisable(GL_CULL_FACE);
		drawgl_patch_with_phong(view_matrices, lights, kemo_shaders, kemo_VAOs->mesh_solid_VAO[2]);
		draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_matrices, 
                            lights, kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);

		drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[0], kemo_shaders);
		drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[1], kemo_shaders);

/*  Stop drawing transparent objects  */
		draw_PSF_trans_objects_VAO(view_matrices, lights,
                                   kemo_VAOs->psf_trans_VAO, kemo_shaders);
		if(kemo_mesh->mesh_m->iflag_draw_mesh != 0){
			draw_trans_mesh_VAO(view_matrices, lights,
                                kemo_VAOs->mesh_trans_VAO, kemo_shaders);
		};
	};
	
/* Draw Color bar and time label*/
    draw_2D_box_patch_VAO(cbar_matrices,    kemo_VAOs->cbar_VAO[0], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->cbar_VAO[1], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->cbar_VAO[2], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->cbar_VAO[3], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->time_VAO, kemo_shaders);
/* Draw message */
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->msg_VAO, kemo_shaders);
/* draw example cube for empty data */
    draw_initial_cube(view_matrices, kemo_VAOs->cube_VAO, kemo_shaders);

    free(map_matrices);
    free(view_matrices);
    free(cbar_matrices);
	return;
}

static void full_draw_objects(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline,
                              struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                              struct phong_lights *lights, struct kemoview_VAOs *kemo_VAOs,
                              struct kemoview_shaders *kemo_shaders){
    double *orthogonal = orthogonal_projection_mat_c(0.0, view_s->nx_frame,
                                                     0.0, view_s->ny_frame,
                                                     -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(view_s);
    struct transfer_matrices *map_matrices = init_projection_matrix_for_map(view_s->nx_frame, view_s->ny_frame);
    free(orthogonal);

    if(view_s->iflag_view_type == VIEW_MAP){
        draw_map_objects_VAO(map_matrices, kemo_VAOs->map_VAO, kemo_shaders);
    }else{
        glDisable(GL_CULL_FACE);
        drawgl_patch_with_phong(view_matrices, lights, kemo_shaders,
                                kemo_VAOs->grid_VAO[2]);

        if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
            glDisable(GL_CULL_FACE);
            drawgl_patch_with_phong(view_matrices, lights, kemo_shaders,
                                    kemo_VAOs->fline_VAO[0]);
        } else {
            drawgl_lines(view_matrices, kemo_VAOs->fline_VAO[1], kemo_shaders);
        };
        draw_PSF_solid_objects_VAO(view_matrices, lights,
                                   kemo_VAOs->psf_solid_VAO, kemo_shaders);
        drawgl_lines(view_matrices, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);

        glDisable(GL_CULL_FACE);
        drawgl_patch_with_phong(view_matrices, lights, kemo_shaders,
                                kemo_VAOs->mesh_solid_VAO[2]);
        draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_matrices,
                            lights, kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);

        drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[0], kemo_shaders);
        drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[1], kemo_shaders);
    
        draw_PSF_trans_objects_VAO(view_matrices, lights,
                                   kemo_VAOs->psf_trans_VAO, kemo_shaders);
        draw_trans_mesh_VAO(view_matrices,  lights, 
                            kemo_VAOs->mesh_trans_VAO, kemo_shaders);
    }
/* Draw message */
    draw_2D_box_patch_VAO(cbar_matrices,    kemo_VAOs->cbar_VAO[0], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->cbar_VAO[1], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->cbar_VAO[2], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->cbar_VAO[3], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->time_VAO, kemo_shaders);
/* draw message */
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->msg_VAO, kemo_shaders);
/* draw example cube for empty data */
    draw_initial_cube(view_matrices, kemo_VAOs->cube_VAO, kemo_shaders);

    free(map_matrices);
    free(view_matrices);
    free(cbar_matrices);
    return;
}


static void set_transparent_objects_to_VAO(struct kemoview_buffers *kemo_buffers,
                                           struct kemoview_VAOs *kemo_VAOs,
                                           struct kemoview_shaders *kemo_shaders){
/* Set Transparent Objects */
    set_PSF_trans_objects_VAO(kemo_buffers->PSF_trns_buf, kemo_buffers->PSF_ttxur_buf,
                              kemo_VAOs->psf_trans_VAO);
    Const_VAO_4_Phong(kemo_VAOs->mesh_trans_VAO, kemo_buffers->mesh_trns_buf);
    return;
};

static void set_draw_objects_to_VAO(struct kemoview_psf *kemo_psf,
                                    struct view_element *view_s,
                                    struct kemoview_buffers *kemo_buffers,
                                    struct kemoview_VAOs *kemo_VAOs,
                                    struct kemoview_shaders *kemo_shaders){
    if(view_s->iflag_view_type == VIEW_MAP){
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[0], kemo_buffers->MAP_solid_buf);
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[1], kemo_buffers->MAP_isoline_buf);
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[2], kemo_buffers->coast_buf);
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[3], kemo_buffers->sph_grid_buf);
    }else{
        Const_VAO_4_Phong(kemo_VAOs->grid_VAO[2], kemo_buffers->axis_buf);
        
        Const_VAO_4_Phong(kemo_VAOs->fline_VAO[0], kemo_buffers->FLINE_tube_buf);
        Const_VAO_4_Simple(kemo_VAOs->fline_VAO[1], kemo_buffers->FLINE_line_buf);
        
        const_PSF_gl_texure_name(kemo_psf->psf_a->psf_texure, kemo_shaders);
        set_PSF_solid_objects_VAO(kemo_buffers->PSF_solid_buf, kemo_buffers->PSF_stxur_buf,
                                  kemo_buffers->PSF_isoline_buf, kemo_buffers->PSF_arrow_buf,
                                  kemo_VAOs->psf_solid_VAO);
        
        Const_VAO_4_Phong(kemo_VAOs->mesh_solid_VAO[0],  kemo_buffers->mesh_solid_buf);
        Const_VAO_4_Simple(kemo_VAOs->mesh_solid_VAO[1], kemo_buffers->mesh_grid_buf);
        Const_VAO_4_Phong(kemo_VAOs->mesh_solid_VAO[2],  kemo_buffers->mesh_node_buf);
        
        Const_VAO_4_Simple(kemo_VAOs->grid_VAO[0], kemo_buffers->coast_buf);
        Const_VAO_4_Simple(kemo_VAOs->grid_VAO[1], kemo_buffers->sph_grid_buf);
        
        /* Set Transparent Objects */
        set_transparent_objects_to_VAO(kemo_buffers, kemo_VAOs, kemo_shaders);
    };
    
    Const_VAO_4_Simple(kemo_VAOs->cbar_VAO[0], kemo_buffers->cbar_buf);
    const_texture_VBO(kemo_buffers->cbar_min_image->npix_img[0], kemo_buffers->cbar_min_image->npix_img[1],
                      kemo_buffers->cbar_min_image->imgBMP, kemo_VAOs->cbar_VAO[1], kemo_buffers->min_buf);
    const_texture_VBO(kemo_buffers->cbar_max_image->npix_img[0], kemo_buffers->cbar_max_image->npix_img[1],
                      kemo_buffers->cbar_max_image->imgBMP, kemo_VAOs->cbar_VAO[2], kemo_buffers->max_buf);
    const_texture_VBO(kemo_buffers->cbar_zero_image->npix_img[0], kemo_buffers->cbar_zero_image->npix_img[1],
                      kemo_buffers->cbar_zero_image->imgBMP, kemo_VAOs->cbar_VAO[3], kemo_buffers->zero_buf);
    const_texture_VBO(kemo_buffers->tlabel_image->npix_img[0], kemo_buffers->tlabel_image->npix_img[1],
                      kemo_buffers->tlabel_image->imgBMP, kemo_VAOs->time_VAO, kemo_buffers->time_buf);
    
    const_texture_VBO(kemo_buffers->message_image->npix_img[0],
                      kemo_buffers->message_image->npix_img[1],
                      kemo_buffers->message_image->imgBMP,
                      kemo_VAOs->msg_VAO, kemo_buffers->msg_buf);
    set_initial_cube_VAO(kemo_buffers->cube_buf, kemo_buffers->cube_index_buf, kemo_VAOs->cube_VAO);
    return;
};


static void update_draw_objects(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline,
                                struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                                struct kemoview_buffers *kemo_buffers,
                                struct kemoview_VAOs *kemo_VAOs,
                                struct kemoview_shaders *kemo_shaders){
/* Set Vertex buffers */
    if(view_s->iflag_draw_mode == SIMPLE_DRAW){
        quick_draw_objects(kemo_psf, kemo_fline, kemo_mesh, 
                           view_s, kemo_buffers->kemo_lights,
                           kemo_VAOs, kemo_shaders);

    }else if(view_s->iflag_draw_mode == FAST_DRAW){
        if(view_s->iflag_view_type != VIEW_MAP){
            set_transparent_buffers(kemo_psf, kemo_mesh, view_s, kemo_buffers);
            set_transparent_objects_to_VAO(kemo_buffers, kemo_VAOs, kemo_shaders);
        }
        full_draw_objects(kemo_psf, kemo_fline, kemo_mesh, view_s,
                          kemo_buffers->kemo_lights, kemo_VAOs, kemo_shaders);
    }else{
        set_kemoviewer_buffers(kemo_psf, kemo_fline, kemo_mesh, view_s, kemo_buffers);
        set_draw_objects_to_VAO(kemo_psf, view_s, kemo_buffers,
                                kemo_VAOs, kemo_shaders);
        full_draw_objects(kemo_psf, kemo_fline, kemo_mesh, view_s,
                          kemo_buffers->kemo_lights, kemo_VAOs, kemo_shaders);
    }
	return;
}

void update_draw_objects_gl3(struct kemoviewer_type *kemoview,
                             struct kemoviewer_gl_type *kemo_gl){
	update_draw_objects(kemoview->kemo_psf, kemoview->kemo_fline,
                        kemoview->kemo_mesh, kemoview->view_s, kemoview->kemo_buffers,
                        kemo_gl->kemo_VAOs, kemo_gl->kemo_shaders);
	return;
}
