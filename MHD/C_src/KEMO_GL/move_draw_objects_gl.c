
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
	
	kemo_VAOs->cbar_VAO = (struct VAO_ids **) malloc(2*sizeof(struct VAO_ids *));
	for(i=0;i<2;i++){
		kemo_VAOs->cbar_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	
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
	
	for(i=0;i<3;i++){
		free(kemo_VAOs->mesh_solid_VAO[i]);
	};
	free(kemo_VAOs->mesh_solid_VAO);
	free(kemo_VAOs->mesh_trans_VAO);
	
	for(i=0;i<2;i++){
		free(kemo_VAOs->fline_VAO[i]);
	};
	free(kemo_VAOs->fline_VAO);
	
	for(i=0;i<4;i++){
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

	for(i=0;i<4;i++){
		free(kemo_VAOs->map_VAO[i]);
	};
	free(kemo_VAOs->map_VAO);
	return;
};

void assign_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
	glGenVertexArrays(1, &kemo_VAOs->grid_VAO[0]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->grid_VAO[1]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->grid_VAO[2]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->fline_VAO[0]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->fline_VAO[1]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->psf_solid_VAO[1]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->psf_solid_VAO[0]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->psf_solid_VAO[3]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->psf_solid_VAO[2]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->mesh_solid_VAO[1]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->mesh_solid_VAO[2]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->mesh_solid_VAO[0]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->psf_trans_VAO[1]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->psf_trans_VAO[0]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->mesh_trans_VAO->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->cbar_VAO[0]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->cbar_VAO[1]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->map_VAO[0]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->map_VAO[1]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->map_VAO[2]->id_VAO);
	glGenVertexArrays(1, &kemo_VAOs->map_VAO[3]->id_VAO);
    glGenVertexArrays(1, &kemo_VAOs->cube_VAO->id_VAO);
    glGenVertexArrays(1, &kemo_VAOs->msg_VAO->id_VAO);
};

void clear_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
	Destroy_VAO(kemo_VAOs->grid_VAO[0]);
	Destroy_VAO(kemo_VAOs->grid_VAO[1]);
	Destroy_VAO(kemo_VAOs->grid_VAO[2]);
	Destroy_VAO(kemo_VAOs->fline_VAO[0]);
	Destroy_VAO(kemo_VAOs->fline_VAO[1]);
	Destroy_VAO(kemo_VAOs->psf_solid_VAO[1]);
	Destroy_VAO(kemo_VAOs->psf_solid_VAO[0]);
	Destroy_VAO(kemo_VAOs->psf_solid_VAO[3]);
	Destroy_VAO(kemo_VAOs->psf_solid_VAO[2]);
	Destroy_VAO(kemo_VAOs->mesh_solid_VAO[1]);
	Destroy_VAO(kemo_VAOs->mesh_solid_VAO[2]);
	Destroy_VAO(kemo_VAOs->mesh_solid_VAO[0]);
	Destroy_VAO(kemo_VAOs->psf_trans_VAO[1]);
	Destroy_VAO(kemo_VAOs->psf_trans_VAO[0]);
	Destroy_VAO(kemo_VAOs->mesh_trans_VAO);
	Destroy_VAO(kemo_VAOs->cbar_VAO[0]);
	Destroy_VAO(kemo_VAOs->cbar_VAO[1]);
	Destroy_VAO(kemo_VAOs->map_VAO[0]);
	Destroy_VAO(kemo_VAOs->map_VAO[1]);
	Destroy_VAO(kemo_VAOs->map_VAO[2]);
	Destroy_VAO(kemo_VAOs->map_VAO[3]);
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
	
    /* Draw Solid Objects */
	if(view_s->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a, view_s);
		draw_map_objects_VAO(view_s, kemo_VAOs->map_VAO, kemo_shaders);
	} else {
        glDisable(GL_CULL_FACE);
		drawgl_patch_with_phong(view_s, kemo_VAOs->grid_VAO[2], kemo_shaders);
		
		drawgl_lines(view_s, kemo_VAOs->fline_VAO[1], kemo_shaders);
		
		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, view_s);
		draw_PSF_solid_objects_VAO(kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, view_s, kemo_VAOs->psf_solid_VAO, kemo_shaders);
		
		drawgl_lines(view_s, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);
        glDisable(GL_CULL_FACE);
		drawgl_patch_with_phong(view_s, kemo_VAOs->mesh_solid_VAO[2], kemo_shaders);
		draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_s, 
					kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);
		
		drawgl_lines(view_s, kemo_VAOs->grid_VAO[0], kemo_shaders);
		drawgl_lines(view_s, kemo_VAOs->grid_VAO[1], kemo_shaders);
			
/*  Stop drawing transparent objects  */
		draw_PSF_trans_objects_VAO(kemo_psf->psf_m, kemo_psf->psf_a, 
					view_s, kemo_VAOs->psf_trans_VAO, kemo_shaders);
		if(kemo_mesh->mesh_m->iflag_draw_mesh != 0){
			draw_trans_mesh_VAO(view_s, kemo_VAOs->mesh_trans_VAO, kemo_shaders);
		};
	};
	
    /* Draw Color bar */
	draw_colorbar_VAO(kemo_psf->psf_a->cbar_wk, kemo_VAOs->cbar_VAO, kemo_shaders);
	
    /* Draw message */
    if(kemo_mesh->msg_wk->message_opacity > 0.0){
	    draw_message_VAO(kemo_mesh->msg_wk, kemo_VAOs->msg_VAO, kemo_shaders);
    };
    /* draw example cube for empty data */
	if(kemo_VAOs->cube_VAO->npoint_draw > 0){
		draw_initial_cube(view_s, kemo_VAOs->cube_VAO, kemo_shaders);
	}
	return;
}

static void update_draw_objects(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline, 
			struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
			struct kemoview_VAOs *kemo_VAOs, struct kemoview_shaders *kemo_shaders){
	int i;
	int iflag;
	int iflag_psf = 0;
	
    /* Draw Solid Objects */
	
	if(view_s->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, view_s);
		iflag_psf = check_draw_map(kemo_psf->psf_a);
		set_map_objects_VAO(view_s, kemo_psf->psf_d, 
					kemo_mesh->mesh_m, kemo_psf->psf_m, kemo_psf->psf_a, kemo_VAOs->map_VAO);
		draw_map_objects_VAO(view_s, kemo_VAOs->map_VAO, kemo_shaders);
	} else {
		set_axis_VAO(kemo_mesh->mesh_m, view_s, kemo_VAOs->grid_VAO[2]);
        glDisable(GL_CULL_FACE);
		drawgl_patch_with_phong(view_s, kemo_VAOs->grid_VAO[2], kemo_shaders);
		
		sel_fieldlines_VAO(kemo_fline->fline_d, kemo_fline->fline_m, kemo_VAOs->fline_VAO);
		if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
            glDisable(GL_CULL_FACE);
			drawgl_patch_with_phong(view_s, kemo_VAOs->fline_VAO[0], kemo_shaders);
		} else {
			drawgl_lines(view_s, kemo_VAOs->fline_VAO[1], kemo_shaders);
		};

 		iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, view_s);
		iflag_psf = iflag_psf + check_draw_psf(kemo_psf->psf_a);
		set_PSF_solid_objects_VAO(view_s, kemo_psf->psf_d, kemo_psf->psf_m,
					kemo_psf->psf_a, kemo_VAOs->psf_solid_VAO);
		draw_PSF_solid_objects_VAO(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a, 
					view_s, kemo_VAOs->psf_solid_VAO, kemo_shaders);
		draw_PSF_isolines_VAO(view_s, kemo_VAOs->psf_solid_VAO, kemo_shaders);
	
		if(kemo_mesh->mesh_m->iflag_draw_mesh != 0){
			set_solid_mesh_VAO(kemo_mesh->mesh_d, kemo_mesh->mesh_m,
						view_s, kemo_VAOs->mesh_solid_VAO);

			drawgl_lines(view_s, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);
            glDisable(GL_CULL_FACE);
			drawgl_patch_with_phong(view_s, kemo_VAOs->mesh_solid_VAO[2], kemo_shaders);
			draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_s, 
						kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);
		} else {
			kemo_VAOs->mesh_solid_VAO[0]->npoint_draw = 0;
			kemo_VAOs->mesh_solid_VAO[1]->npoint_draw = 0;
			kemo_VAOs->mesh_solid_VAO[2]->npoint_draw = 0;
		};
		
		set_coastline_grid_VBO(kemo_mesh->mesh_m, kemo_VAOs->grid_VAO);
		drawgl_lines(view_s, kemo_VAOs->grid_VAO[0], kemo_shaders);
		drawgl_lines(view_s, kemo_VAOs->grid_VAO[1], kemo_shaders);
		
		/* Draw Transparent Objects */
		set_PSF_trans_objects_VAO(view_s, 
					kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
					kemo_VAOs->psf_trans_VAO);
		draw_PSF_trans_objects_VAO(kemo_psf->psf_m, kemo_psf->psf_a, 
					view_s, kemo_VAOs->psf_trans_VAO, kemo_shaders);
		
		if(kemo_mesh->mesh_m->iflag_draw_mesh != 0){
			set_trans_mesh_VAO(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
						kemo_VAOs->mesh_trans_VAO);
			draw_trans_mesh_VAO(view_s, kemo_VAOs->mesh_trans_VAO, kemo_shaders);
		};
	};
	
	
    /* Draw Color bar */
	for(i=0;i<kemo_psf->psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + kemo_psf->psf_a->iflag_loaded[i];
	};
	
	
	set_colorbar_VAO(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
				kemo_mesh->mesh_m->text_color, kemo_mesh->mesh_m->bg_color, 
				kemo_psf->psf_m, kemo_psf->psf_a,
				kemo_VAOs->cbar_VAO);
	draw_colorbar_VAO(kemo_psf->psf_a->cbar_wk, kemo_VAOs->cbar_VAO, kemo_shaders);
	
    /* Draw message */
    if(kemo_mesh->msg_wk->message_opacity > 0.0){
	    set_message_VAO(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                        kemo_mesh->msg_wk, kemo_VAOs->msg_VAO);
        draw_message_VAO(kemo_mesh->msg_wk, kemo_VAOs->msg_VAO, kemo_shaders);
    };
    /* draw example cube for empty data */
	iflag = kemo_mesh->mesh_m->iflag_draw_mesh + iflag_psf + kemo_fline->fline_m->iflag_draw_fline;
	if(iflag == 0){
		set_initial_cube_VAO(view_s, kemo_VAOs->cube_VAO);
		draw_initial_cube(view_s, kemo_VAOs->cube_VAO, kemo_shaders);
	} else {
		kemo_VAOs->cube_VAO->npoint_draw = 0;
	}
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
