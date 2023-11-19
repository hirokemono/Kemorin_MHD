
/* move_draw_objects_gl.c */

#include "move_draw_objects_gl.h"

struct kemoview_buffers * init_kemoview_buffers(void)
{
    int n_point;

    struct kemoview_buffers *kemo_buffers = (struct kemoview_buffers *) malloc(sizeof(struct kemoview_buffers));
    if(kemo_buffers == NULL){
        printf("malloc error for kemoview_buffers\n");
        exit(0);
    }
    
    kemo_buffers->cube_buf = init_strided_buffer(8);
    kemo_buffers->cube_index_buf = alloc_gl_index_buffer(12, 3);
    CubeNode_to_buf(0.5f, kemo_buffers->cube_buf, kemo_buffers->cube_index_buf);

    n_point = 1024;
    kemo_buffers->PSF_solid_buf =   init_strided_buffer(n_point);
    kemo_buffers->PSF_stxur_buf =   init_strided_buffer(n_point);
    kemo_buffers->PSF_trns_buf =    init_strided_buffer(n_point);
    kemo_buffers->PSF_ttxur_buf =   init_strided_buffer(n_point);
    kemo_buffers->PSF_isoline_buf = init_strided_buffer(n_point);
    kemo_buffers->PSF_arrow_buf =   init_strided_buffer(n_point);

    kemo_buffers->MAP_solid_buf =   init_strided_buffer(n_point);
    kemo_buffers->MAP_isoline_buf = init_strided_buffer(n_point);

    kemo_buffers->FLINE_line_buf = init_strided_buffer(n_point);
    kemo_buffers->FLINE_tube_buf = init_strided_buffer(n_point);

    kemo_buffers->mesh_solid_buf = init_strided_buffer(n_point);
    kemo_buffers->mesh_grid_buf =  init_strided_buffer(n_point);
    kemo_buffers->mesh_node_buf =  init_strided_buffer(n_point);
    kemo_buffers->mesh_trns_buf =  init_strided_buffer(n_point);

    n_point = ITWO * count_coastline_buf();
    kemo_buffers->coast_buf =    init_strided_buffer(n_point);
    n_point = ITWO * count_sph_flame();
    kemo_buffers->sph_grid_buf = init_strided_buffer(n_point);
    
    kemo_buffers->ncorner_axis = ISIX;
    n_point = ITHREE * count_axis_to_buf(kemo_buffers->ncorner_axis);
    kemo_buffers->axis_buf = init_strided_buffer(n_point);

    n_point = count_colorbar_box_VAO(IONE, 128);
    kemo_buffers->cbar_buf = init_strided_buffer(n_point);

    n_point = ITWO * ITHREE;
    kemo_buffers->min_buf =  init_strided_buffer(n_point);
    kemo_buffers->max_buf =  init_strided_buffer(n_point);
    kemo_buffers->zero_buf = init_strided_buffer(n_point);
    kemo_buffers->time_buf = init_strided_buffer(n_point);
    kemo_buffers->msg_buf =  init_strided_buffer(n_point);
    return kemo_buffers;
};

void dealloc_kemoview_buffers(struct kemoview_buffers *kemo_buffers)
{
    dealloc_gl_index_buffer(kemo_buffers->cube_index_buf);
    dealloc_strided_buffer(kemo_buffers->cube_buf);

    dealloc_strided_buffer(kemo_buffers->mesh_solid_buf);
    dealloc_strided_buffer(kemo_buffers->mesh_grid_buf);
    dealloc_strided_buffer(kemo_buffers->mesh_node_buf);
    dealloc_strided_buffer(kemo_buffers->mesh_trns_buf);

    dealloc_strided_buffer(kemo_buffers->FLINE_line_buf);
    dealloc_strided_buffer(kemo_buffers->FLINE_tube_buf);

    dealloc_strided_buffer(kemo_buffers->PSF_ttxur_buf);
    dealloc_strided_buffer(kemo_buffers->PSF_stxur_buf);
    dealloc_strided_buffer(kemo_buffers->PSF_solid_buf);
    dealloc_strided_buffer(kemo_buffers->PSF_trns_buf);
    dealloc_strided_buffer(kemo_buffers->PSF_isoline_buf);
    dealloc_strided_buffer(kemo_buffers->PSF_arrow_buf);

    dealloc_strided_buffer(kemo_buffers->MAP_solid_buf);
    dealloc_strided_buffer(kemo_buffers->MAP_isoline_buf);

    dealloc_strided_buffer(kemo_buffers->coast_buf);
    dealloc_strided_buffer(kemo_buffers->sph_grid_buf);

    dealloc_strided_buffer(kemo_buffers->cbar_buf);
    dealloc_strided_buffer(kemo_buffers->min_buf);
    dealloc_strided_buffer(kemo_buffers->max_buf);
    dealloc_strided_buffer(kemo_buffers->zero_buf);
    dealloc_strided_buffer(kemo_buffers->time_buf);
    dealloc_strided_buffer(kemo_buffers->msg_buf);

    dealloc_strided_buffer(kemo_buffers->axis_buf);

    free(kemo_buffers);
    return;
};


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
                               struct kemoview_buffers *kemo_buffers,
                               struct kemoview_VAOs *kemo_VAOs,
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

static void update_draw_objects(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline, 
                                struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                                struct kemoview_buffers *kemo_buffers,
                                struct kemoview_VAOs *kemo_VAOs,
                                struct kemoview_shaders *kemo_shaders){
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

        set_map_coastline_buffer(kemo_mesh->mesh_m, kemo_buffers->coast_buf);
        set_map_flame_buffer(kemo_mesh->mesh_m, kemo_buffers->sph_grid_buf);

        set_map_patch_buffer(IZERO, kemo_psf->psf_a->istack_solid_psf_patch,
                             kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                             kemo_buffers->MAP_solid_buf);
        set_map_PSF_isolines_buffer(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a, view_s,
                                    kemo_buffers->MAP_isoline_buf);
    } else {
        double axis_radius = 4.0;
        set_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                        kemo_mesh->mesh_m->dist_domains,
                        kemo_buffers->ncorner_axis, axis_radius,
                        kemo_buffers->axis_buf);
        
        const_fieldlines_buffer(kemo_fline->fline_d, kemo_fline->fline_m,
                                kemo_buffers->FLINE_tube_buf, kemo_buffers->FLINE_line_buf);
        
        
        iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
                                                kemo_psf->psf_a, view_s);
        iflag_psf = iflag_psf + check_draw_psf(kemo_psf->psf_a);
        const_PSF_solid_objects_buffer(view_s, kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                                       kemo_buffers->PSF_solid_buf, kemo_buffers->PSF_stxur_buf,
                                       kemo_buffers->PSF_isoline_buf, kemo_buffers->PSF_arrow_buf);
        
        const_solid_mesh_buffer(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->mesh_solid_buf, kemo_buffers->mesh_grid_buf,
                                kemo_buffers->mesh_node_buf);
        
        set_coastline_buffer(kemo_mesh->mesh_m, kemo_buffers->coast_buf);
        set_sph_flame_buffer(kemo_mesh->mesh_m, kemo_buffers->sph_grid_buf);
        
        /* Draw Transparent Objects */
        const_PSF_trans_objects_buffer(view_s, kemo_psf->psf_d,
                                       kemo_psf->psf_m, kemo_psf->psf_a,
                                       kemo_buffers->PSF_trns_buf, kemo_buffers->PSF_ttxur_buf);
        const_trans_mesh_buffer(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->mesh_trns_buf);
    }
    
    
    if(view_s->iflag_view_type == VIEW_MAP){
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[0], kemo_buffers->MAP_solid_buf);
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[1], kemo_buffers->MAP_isoline_buf);
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[2], kemo_buffers->coast_buf);
        Const_VAO_4_Simple(kemo_VAOs->map_VAO[3], kemo_buffers->sph_grid_buf);
    }else{
        Const_VAO_4_Phong(kemo_VAOs->grid_VAO[2], kemo_buffers->axis_buf);
        
        Const_VAO_4_Phong(kemo_VAOs->fline_VAO[0], kemo_buffers->FLINE_tube_buf);
        Const_VAO_4_Simple(kemo_VAOs->fline_VAO[1], kemo_buffers->FLINE_line_buf);
        
        set_PSF_solid_objects_VAO(kemo_buffers->PSF_solid_buf, kemo_buffers->PSF_stxur_buf,
                                  kemo_buffers->PSF_isoline_buf, kemo_buffers->PSF_arrow_buf,
                                  kemo_VAOs->psf_solid_VAO);
        
        Const_VAO_4_Phong(kemo_VAOs->mesh_solid_VAO[0],  kemo_buffers->mesh_solid_buf);
        Const_VAO_4_Simple(kemo_VAOs->mesh_solid_VAO[1], kemo_buffers->mesh_grid_buf);
        Const_VAO_4_Phong(kemo_VAOs->mesh_solid_VAO[2],  kemo_buffers->mesh_node_buf);

        Const_VAO_4_Simple(kemo_VAOs->grid_VAO[0], kemo_buffers->coast_buf);
        Const_VAO_4_Simple(kemo_VAOs->grid_VAO[1], kemo_buffers->sph_grid_buf);

/* Draw Transparent Objects */
		set_PSF_trans_objects_VAO(kemo_buffers->PSF_trns_buf, kemo_buffers->PSF_ttxur_buf,
                                  kemo_VAOs->psf_trans_VAO);
        Const_VAO_4_Phong(kemo_VAOs->mesh_trans_VAO, kemo_buffers->mesh_trns_buf);
	};

	
/* Draw Color bar and time label*/
	for(i=0;i<kemo_psf->psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + kemo_psf->psf_a->iflag_loaded[i];
	};
	const_colorbar_buffer(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                          kemo_mesh->mesh_m->text_color, kemo_mesh->mesh_m->bg_color,
                          kemo_psf->psf_m, kemo_psf->psf_a,
                          kemo_buffers->cbar_buf, kemo_buffers->min_buf,
                          kemo_buffers->max_buf, kemo_buffers->zero_buf);
    const_timelabel_buffer(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                           kemo_mesh->mesh_m->text_color, kemo_mesh->mesh_m->bg_color,
                           kemo_psf->psf_a, kemo_buffers->time_buf);
	/* Draw message */
    const_message_buffer(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                         kemo_mesh->msg_wk, kemo_buffers->msg_buf);
    /* draw example cube for empty data */
	iflag = kemo_mesh->mesh_m->iflag_draw_mesh + iflag_psf + kemo_fline->fline_m->iflag_draw_fline;
	if(iflag == 0){
        kemo_VAOs->cube_VAO->npoint_draw = kemo_buffers->cube_index_buf->nsize_buf;
	} else {
		kemo_VAOs->cube_VAO->npoint_draw = 0;
	}
    
    set_colorbar_VAO(kemo_psf->psf_a->cbar_wk, kemo_VAOs->cbar_VAO,
                     kemo_buffers->cbar_buf, kemo_buffers->min_buf,
                     kemo_buffers->max_buf, kemo_buffers->zero_buf);
    set_time_text_VAO(kemo_psf->psf_a->tlabel_wk, kemo_VAOs->time_VAO,
                      kemo_buffers->time_buf);
    
    set_message_VAO(kemo_mesh->msg_wk, kemo_buffers->msg_buf, kemo_VAOs->msg_VAO);
    set_initial_cube_VAO(kemo_buffers->cube_buf, kemo_buffers->cube_index_buf, kemo_VAOs->cube_VAO);

    
    if(view_s->iflag_view_type == VIEW_MAP){
        draw_map_objects_VAO(map_matrices, kemo_VAOs->map_VAO, kemo_shaders);
    }else{
        glDisable(GL_CULL_FACE);
        drawgl_patch_with_phong(view_matrices, kemo_VAOs->grid_VAO[2], kemo_shaders);

        if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
            glDisable(GL_CULL_FACE);
            drawgl_patch_with_phong(view_matrices, kemo_VAOs->fline_VAO[0], kemo_shaders);
        } else {
            drawgl_lines(view_matrices, kemo_VAOs->fline_VAO[1], kemo_shaders);
        };
        draw_PSF_solid_objects_VAO(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                                   view_matrices, kemo_VAOs->psf_solid_VAO, kemo_shaders);
        drawgl_lines(view_matrices, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);

        glDisable(GL_CULL_FACE);
        drawgl_patch_with_phong(view_matrices, kemo_VAOs->mesh_solid_VAO[2], kemo_shaders);
        draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_matrices,
                    kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);

        drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[0], kemo_shaders);
        drawgl_lines(view_matrices, kemo_VAOs->grid_VAO[1], kemo_shaders);

        draw_PSF_trans_objects_VAO(kemo_psf->psf_m, kemo_psf->psf_a, view_matrices,
                                   kemo_VAOs->psf_trans_VAO, kemo_shaders);
        draw_trans_mesh_VAO(view_matrices, kemo_VAOs->mesh_trans_VAO, kemo_shaders);
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
    
    free(view_matrices);
    free(map_matrices);
    free(cbar_matrices);
	return;
}


void quick_draw_objects_gl3(struct kemoviewer_type *kemoview){
	quick_draw_objects(kemoview->kemo_psf, kemoview->kemo_fline, 
                       kemoview->kemo_mesh, kemoview->view_s, kemoview->kemo_glbufs,
                       kemoview->kemo_VAOs, kemoview->kemo_shaders);
	return;
};

void update_draw_objects_gl3(struct kemoviewer_type *kemoview){
	update_draw_objects(kemoview->kemo_psf, kemoview->kemo_fline,
				kemoview->kemo_mesh, kemoview->view_s, kemoview->kemo_glbufs,
				kemoview->kemo_VAOs, kemoview->kemo_shaders);
	return;
}
