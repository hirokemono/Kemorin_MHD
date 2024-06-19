/*
 *  move_draw_objects_gl.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "move_draw_objects_gl.h"

void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage){
    glReadBuffer(GL_FRONT);
    glPixelStorei(GL_PACK_ALIGNMENT, IONE);
    glReadPixels(IZERO, IZERO, (GLsizei) num_x, (GLsizei) num_y,
                 GL_RGB, GL_UNSIGNED_BYTE,(GLubyte *) glimage);
    return;
}


static void full_draw_objects(struct kemoview_mul_psf *kemo_mul_psf, struct kemoview_fline *kemo_fline,
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

/* draw example cube for empty data */
    draw_initial_cube(view_matrices, lights, kemo_shaders, kemo_VAOs->cube_VAO);
    if(kemo_VAOs->cube_VAO->npoint_draw > 0){
        free(map_matrices);
        free(view_matrices);
        free(cbar_matrices);
        return;
    };
    
    
    if(view_s->iflag_view_type == VIEW_MAP){
        draw_map_objects_VAO(map_matrices, kemo_VAOs->map_VAO, kemo_VAOs->map_index_VAO, kemo_shaders);
    }else{


        glDisable(GL_CULL_FACE);
        drawgl_elements_with_phong(view_matrices, lights, kemo_shaders,
                                   kemo_VAOs->axis_VAO);

        if(kemo_VAOs->psf_solid_index_VAO[4]->npoint_draw == 0){
            drawgl_lines(view_matrices, kemo_VAOs->psf_lines_VAO, kemo_shaders);
        };
        if(kemo_VAOs->fline_VAO[0]->npoint_draw == 0){
            drawgl_lines(view_matrices, kemo_VAOs->fline_VAO[1], kemo_shaders);
        };
        if(kemo_VAOs->tracer_index_VAO->npoint_draw == 0){
            drawgl_points(view_matrices, kemo_VAOs->tracer_VAO, kemo_shaders);
        };

        glDisable(GL_CULL_FACE);
        drawgl_elements_with_phong(view_matrices, lights, kemo_shaders,
                                   kemo_VAOs->fline_VAO[0]);
        drawgl_elements_with_phong(view_matrices, lights, kemo_shaders,
                                   kemo_VAOs->tracer_index_VAO);

        draw_PSF_solid_objects_VAO(view_matrices, lights,
                                   kemo_VAOs->psf_solid_VAO,
                                   kemo_VAOs->psf_solid_index_VAO,
                                   kemo_shaders);

        glDisable(GL_CULL_FACE);
        drawgl_lines(view_matrices, kemo_VAOs->mesh_solid_VAO[1], kemo_shaders);
        drawgl_elements_with_phong(view_matrices, lights, kemo_shaders,
                                   kemo_VAOs->mesh_solid_VAO[2]);
        draw_solid_mesh_VAO(kemo_mesh->mesh_m->polygon_mode, view_matrices,
                            lights, kemo_VAOs->mesh_solid_VAO[0], kemo_shaders);

/*  draw coastlines */
        drawgl_elements_with_phong(view_matrices, lights, kemo_shaders,
                                   kemo_VAOs->grid_tube_VAO);
        if(kemo_VAOs->grid_tube_VAO->npoint_draw == 0){
            drawgl_lines(view_matrices, kemo_VAOs->grid_line_VAO, kemo_shaders);
        }
    
        draw_PSF_trans_objects_VAO(view_matrices, lights,
                                   kemo_VAOs->psf_trans_VAO,
                                   kemo_VAOs->psf_trans_index_VAO,
                                   kemo_shaders);
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

    free(map_matrices);
    free(view_matrices);
    free(cbar_matrices);
    return;
}

void update_draw_objects_gl(struct kemoview_mul_psf *kemo_mul_psf, 
                            struct kemoview_fline *kemo_fline,
                            struct kemoview_tracer *kemo_tracer,
                            struct kemoview_mesh *kemo_mesh, 
                            struct view_element *view_s,
                            struct kemoview_buffers *kemo_buffers,
                            struct kemoview_VAOs *kemo_VAOs,
                            struct kemoview_shaders *kemo_shaders){
/* Set Vertex buffers */
    if(view_s->iflag_draw_mode == MOVIE_DRAW
       && view_s->iflag_view_type != VIEW_MAP){
        set_fast_buffers(kemo_mul_psf, kemo_fline, kemo_mesh,
                         view_s, kemo_buffers);
        
        Const_Phong_Index_VAO(kemo_VAOs->axis_VAO, kemo_buffers->axis_buf,
                              kemo_buffers->axis_index_buf);
        set_transparent_objects_to_VAO(kemo_buffers, kemo_VAOs);
    }else if(view_s->iflag_draw_mode == SIMPLE_DRAW){
            kemo_VAOs->psf_solid_index_VAO[4]->npoint_draw =   0;
            kemo_VAOs->psf_solid_index_VAO[5]->npoint_draw =   0;
            kemo_VAOs->fline_VAO[0]->npoint_draw =       0;
//            kemo_VAOs->tracer_index_VAO->npoint_draw =      0;
            kemo_VAOs->grid_tube_VAO->npoint_draw =      0;
            kemo_VAOs->mesh_solid_VAO[2]->npoint_draw =  0;
            Const_Phong_Index_VAO(kemo_VAOs->axis_VAO, kemo_buffers->axis_buf,
                              kemo_buffers->axis_index_buf);

            set_transparent_buffers(kemo_mul_psf, kemo_mesh, view_s, kemo_buffers);
            set_transparent_objects_to_VAO(kemo_buffers, kemo_VAOs);
    }else{
            set_kemoviewer_buffers(kemo_mul_psf, kemo_fline, kemo_tracer,
                                   kemo_mesh, view_s, kemo_buffers);
            set_draw_objects_to_VAO(kemo_mul_psf, view_s, kemo_buffers,
                                    kemo_VAOs, kemo_shaders);
    }
    full_draw_objects(kemo_mul_psf, kemo_fline, kemo_mesh, view_s,
                      kemo_buffers->kemo_lights, kemo_VAOs, kemo_shaders);
	return;
}

