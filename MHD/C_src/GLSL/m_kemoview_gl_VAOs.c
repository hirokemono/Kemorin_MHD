/*
 *  m_kemoview_gl_VAOs.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "m_kemoview_gl_VAOs.h"



struct kemoview_VAOs * init_kemoview_VAOs(void){
	int i;
	struct kemoview_VAOs *kemo_VAOs
			= (struct kemoview_VAOs *) malloc(sizeof(struct kemoview_VAOs));
    if(kemo_VAOs == NULL){
        printf("malloc error for kemoview_VAOs\n");
        exit(0);
    }
	
	kemo_VAOs->cube_VAO =    (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    kemo_VAOs->msg_VAO =     (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    kemo_VAOs->screen_VAO =  (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->mesh_solid_VAO = (struct VAO_ids **) malloc(3*sizeof(struct VAO_ids *));
	for(i=0;i<3;i++){
		kemo_VAOs->mesh_solid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	kemo_VAOs->mesh_trans_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_VAOs->fline_VAO = (struct VAO_ids **) malloc(2*sizeof(struct VAO_ids *));
	for(i=0;i<2;i++){
		kemo_VAOs->fline_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	
    kemo_VAOs->psf_solid_index_VAO = (struct VAO_ids **) malloc(4*sizeof(struct VAO_ids *));
    for(i=0;i<4;i++){
        kemo_VAOs->psf_solid_index_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    };
    kemo_VAOs->psf_trans_index_VAO = (struct VAO_ids **) malloc(4*sizeof(struct VAO_ids *));
    for(i=0;i<2;i++){
        kemo_VAOs->psf_trans_index_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    };

    kemo_VAOs->psf_solid_VAO = (struct VAO_ids **) malloc(4*sizeof(struct VAO_ids *));
	for(i=0;i<4;i++){
		kemo_VAOs->psf_solid_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
	kemo_VAOs->psf_trans_VAO = (struct VAO_ids **) malloc(2*sizeof(struct VAO_ids *));
	for(i=0;i<2;i++){
		kemo_VAOs->psf_trans_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
    kemo_VAOs->psf_liness_VAO =      (struct VAO_ids *) malloc(sizeof(struct VAO_ids));

    kemo_VAOs->axis_VAO =      (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	kemo_VAOs->grid_line_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    kemo_VAOs->grid_tube_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));

	kemo_VAOs->cbar_VAO = (struct VAO_ids **) malloc(4*sizeof(struct VAO_ids *));
	for(i=0;i<4;i++){
		kemo_VAOs->cbar_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
    kemo_VAOs->time_VAO =  (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
    kemo_VAOs->map_index_VAO =  (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	kemo_VAOs->map_VAO = (struct VAO_ids **) malloc(4*sizeof(struct VAO_ids *));
	for(i=0;i<4;i++){
		kemo_VAOs->map_VAO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	};
    
    kemo_VAOs->screen_FBO = (struct VAO_ids **) malloc(2*sizeof(struct VAO_ids *));
    for(i=0;i<3;i++){
        kemo_VAOs->screen_FBO[i] = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    };
    return kemo_VAOs;
};

void dealloc_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
	int i;
    free(kemo_VAOs->screen_VAO);
	free(kemo_VAOs->cube_VAO);
    free(kemo_VAOs->msg_VAO);
	
	for(i=0;i<3;i++){free(kemo_VAOs->mesh_solid_VAO[i]);};
	free(kemo_VAOs->mesh_solid_VAO);
	free(kemo_VAOs->mesh_trans_VAO);
	
	for(i=0;i<2;i++){free(kemo_VAOs->fline_VAO[i]);};
	free(kemo_VAOs->fline_VAO);
	
    for(i=0;i<4;i++){free(kemo_VAOs->psf_solid_index_VAO[i]);};
    free(kemo_VAOs->psf_solid_index_VAO);
    for(i=0;i<2;i++){free(kemo_VAOs->psf_trans_index_VAO[i]);};
    free(kemo_VAOs->psf_trans_index_VAO);

    for(i=0;i<4;i++){free(kemo_VAOs->psf_solid_VAO[i]);};
    free(kemo_VAOs->psf_solid_VAO);

	for(i=0;i<2;i++){free(kemo_VAOs->psf_trans_VAO[i]);};
    free(kemo_VAOs->psf_trans_VAO);
    free(kemo_VAOs->psf_liness_VAO);

    free(kemo_VAOs->axis_VAO);
	free(kemo_VAOs->grid_line_VAO);
    free(kemo_VAOs->grid_tube_VAO);

	for(i=0;i<4;i++){free(kemo_VAOs->cbar_VAO[i]);};
	free(kemo_VAOs->cbar_VAO);
	free(kemo_VAOs->time_VAO);

    free(kemo_VAOs->map_index_VAO);
	for(i=0;i<4;i++){free(kemo_VAOs->map_VAO[i]);};
	free(kemo_VAOs->map_VAO);

    for(i=0;i<3;i++){free(kemo_VAOs->screen_FBO[i]);};
    free(kemo_VAOs->screen_FBO);
return;
};

void assign_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
    int i;
    glGenVertexArrays(1, &(kemo_VAOs->axis_VAO->id_VAO));
    glGenVertexArrays(1, &(kemo_VAOs->grid_line_VAO->id_VAO));
    glGenVertexArrays(1, &(kemo_VAOs->grid_tube_VAO->id_VAO));
    for(i=0;i<2;i++){glGenVertexArrays(1, &(kemo_VAOs->fline_VAO[i]->id_VAO));};

    for(i=0;i<4;i++){glGenVertexArrays(1, &(kemo_VAOs->psf_solid_index_VAO[i]->id_VAO));};
    for(i=0;i<2;i++){glGenVertexArrays(1, &(kemo_VAOs->psf_trans_index_VAO[i]->id_VAO));};

    for(i=0;i<4;i++){glGenVertexArrays(1, &(kemo_VAOs->psf_solid_VAO[i]->id_VAO));};

    glGenVertexArrays(1, &(kemo_VAOs->psf_liness_VAO->id_VAO));

    for(i=0;i<3;i++){glGenVertexArrays(1, &(kemo_VAOs->mesh_solid_VAO[i]->id_VAO));};
    for(i=0;i<2;i++){glGenVertexArrays(1, &(kemo_VAOs->psf_trans_VAO[i]->id_VAO));};
	glGenVertexArrays(1, &(kemo_VAOs->mesh_trans_VAO->id_VAO));
    for(i=0;i<4;i++){glGenVertexArrays(1, &(kemo_VAOs->cbar_VAO[i]->id_VAO));};
	glGenVertexArrays(1, &(kemo_VAOs->time_VAO->id_VAO));

    glGenVertexArrays(1, &(kemo_VAOs->map_index_VAO->id_VAO));
    for(i=0;i<4;i++){glGenVertexArrays(1, &(kemo_VAOs->map_VAO[i]->id_VAO));};

    glGenVertexArrays(1, &(kemo_VAOs->cube_VAO->id_VAO));
    glGenVertexArrays(1, &(kemo_VAOs->msg_VAO->id_VAO));
    glGenVertexArrays(1, &(kemo_VAOs->screen_VAO->id_VAO));
};

void clear_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs){
    int i;
    Destroy_VAO(kemo_VAOs->axis_VAO);
    Destroy_VAO(kemo_VAOs->grid_line_VAO);
    Destroy_VAO(kemo_VAOs->grid_tube_VAO);
    for(i=0;i<2;i++){Destroy_VAO(kemo_VAOs->fline_VAO[i]);};
    
    for(i=0;i<4;i++){Destroy_VAO(kemo_VAOs->psf_solid_index_VAO[i]);};
    for(i=0;i<2;i++){Destroy_VAO(kemo_VAOs->psf_trans_index_VAO[i]);};

    for(i=0;i<4;i++){Destroy_VAO(kemo_VAOs->psf_solid_VAO[i]);};

    Destroy_VAO(kemo_VAOs->psf_liness_VAO);

    for(i=0;i<3;i++){Destroy_VAO(kemo_VAOs->mesh_solid_VAO[i]);};
    for(i=0;i<2;i++){Destroy_VAO(kemo_VAOs->psf_trans_VAO[i]);};
    
	Destroy_VAO(kemo_VAOs->mesh_trans_VAO);
    for(i=0;i<3;i++){Destroy_VAO(kemo_VAOs->cbar_VAO[i]);};
	Destroy_VAO(kemo_VAOs->time_VAO);

    Destroy_VAO(kemo_VAOs->map_index_VAO);
    for(i=0;i<4;i++){Destroy_VAO(kemo_VAOs->map_VAO[i]);};
    
    Destroy_VAO(kemo_VAOs->cube_VAO);
    Destroy_VAO(kemo_VAOs->msg_VAO);
    Destroy_VAO(kemo_VAOs->screen_VAO);
};

static void set_mesh_buffer_to_VAO(struct MESH_buffers *MESH_bufs,
                                   struct VAO_ids **mesh_solid_VAO){
    Const_Phong_VAO(mesh_solid_VAO[0],  MESH_bufs->mesh_solid_buf);
    Const_Simple_VAO(mesh_solid_VAO[1], MESH_bufs->mesh_grid_buf);
    Const_Phong_VAO(mesh_solid_VAO[2],  MESH_bufs->mesh_node_buf);
}

static void set_map_buffer_to_VAO(struct gl_strided_buffer *PSF_node_buf,
                                  struct MAP_buffers *MAP_bufs,
                                  struct kemoview_VAOs *kemo_VAOs){
    Const_Simple_Index_VAO(kemo_VAOs->map_index_VAO, PSF_node_buf,
                           MAP_bufs->MAP_solid_index_buf);

    Const_Simple_VAO(kemo_VAOs->map_VAO[0], MAP_bufs->MAP_solid_buf);
    Const_Simple_VAO(kemo_VAOs->map_VAO[1], MAP_bufs->MAP_isoline_buf);
    
    Const_Simple_VAO(kemo_VAOs->map_VAO[2], MAP_bufs->MAP_coast_tube_buf);
    Const_Simple_VAO(kemo_VAOs->map_VAO[3], MAP_bufs->MAP_coast_line_buf);
}

static void set_fieldline_buffer_to_VAO(struct FieldLine_buffers *Fline_bufs,
                                        struct VAO_ids **fline_VAO){
    Const_Phong_VAO(fline_VAO[0], Fline_bufs->FLINE_tube_buf);
    Const_Simple_VAO(fline_VAO[1], Fline_bufs->FLINE_line_buf);
}

static void set_draw_messages_to_VAO(struct MESSAGE_buffers *MESSAGE_bufs,
                                     struct kemoview_VAOs *kemo_VAOs){
    Const_Simple_VAO(kemo_VAOs->cbar_VAO[0], MESSAGE_bufs->cbar_buf);
    Const_texture_VAO(MESSAGE_bufs->cbar_min_buf->image,  MESSAGE_bufs->cbar_min_buf->vertex,  kemo_VAOs->cbar_VAO[1]);
    Const_texture_VAO(MESSAGE_bufs->cbar_max_buf->image,  MESSAGE_bufs->cbar_max_buf->vertex,  kemo_VAOs->cbar_VAO[2]);
    Const_texture_VAO(MESSAGE_bufs->cbar_zero_buf->image, MESSAGE_bufs->cbar_zero_buf->vertex, kemo_VAOs->cbar_VAO[3]);
    Const_texture_VAO(MESSAGE_bufs->timelabel_buf->image, MESSAGE_bufs->timelabel_buf->vertex, kemo_VAOs->time_VAO);
    
    Const_texture_VAO(MESSAGE_bufs->message_buf->image,   MESSAGE_bufs->message_buf->vertex,  kemo_VAOs->msg_VAO);
    return;
}

void set_transparent_objects_to_VAO(struct kemoview_buffers *kemo_buffers,
                                    struct kemoview_VAOs *kemo_VAOs){
/* Set Transparent Objects */
    set_PSF_trans_objects_VAO(kemo_buffers->PSF_node_buf, kemo_buffers->PSF_transes,
                              kemo_VAOs->psf_trans_VAO, kemo_VAOs->psf_trans_index_VAO);
    Const_Phong_VAO(kemo_VAOs->mesh_trans_VAO, kemo_buffers->mesh_trns_buf);
    return;
};


void set_draw_objects_to_VAO(struct kemoview_psf *kemo_psf,
                             struct view_element *view_s,
                             struct kemoview_buffers *kemo_buffers,
                             struct kemoview_VAOs *kemo_VAOs,
                             struct kemoview_shaders *kemo_shaders){
    if(view_s->iflag_view_type == VIEW_MAP){
        set_map_buffer_to_VAO(kemo_buffers->PSF_node_buf,
                              kemo_buffers->MAP_bufs,
                              kemo_VAOs);
    }else{
        set_fieldline_buffer_to_VAO(kemo_buffers->Fline_bufs, kemo_VAOs->fline_VAO);
        const_PSF_gl_texure_name(kemo_psf->psf_a->ipsf_texured,
                                 kemo_psf->psf_a->psf_texure, kemo_shaders);
        set_PSF_solid_objects_VAO(kemo_buffers->PSF_node_buf, kemo_buffers->PSF_solids,
                                  kemo_VAOs->psf_solid_VAO, kemo_VAOs->psf_solid_index_VAO);
        set_PSF_line_objects_VAO(kemo_buffers->PSF_lines,
                                 kemo_VAOs->psf_solid_VAO,
                                 kemo_VAOs->grid_line_VAO,
                                 kemo_VAOs->grid_tube_VAO);

        set_mesh_buffer_to_VAO(kemo_buffers->MESH_bufs, kemo_VAOs->mesh_solid_VAO);

        /* Set Transparent Objects */
        set_transparent_objects_to_VAO(kemo_buffers, kemo_VAOs);
    };
    set_draw_messages_to_VAO(kemo_buffers->MESSAGE_bufs, kemo_VAOs);
    set_initial_cube_VAO(kemo_buffers->initial_bufs, kemo_VAOs->cube_VAO);
    return;
};

