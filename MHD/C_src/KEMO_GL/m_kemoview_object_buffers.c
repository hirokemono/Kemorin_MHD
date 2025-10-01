/*
//  m_kemoview_object_buffers.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "m_kemoview_object_buffers.h"


struct kemoview_buffers * init_kemoview_buffers(void)
{
    long n_point = 1024;
    
    struct kemoview_buffers *kemo_buffers = (struct kemoview_buffers *) malloc(sizeof(struct kemoview_buffers));
    if(kemo_buffers == NULL){
        printf("malloc error for kemoview_buffers\n");
        exit(0);
    }
    
    kemo_buffers->kemo_lights = (struct phong_lights *) malloc(sizeof(struct phong_lights));
    if(kemo_buffers->kemo_lights == NULL){
        printf("malloc error in phong_lights \n");
        exit(0);
    };
    
    kemo_buffers->nthreads = 8;
    
    kemo_buffers->initial_bufs = init_initial_cube_buffers();
    
    kemo_buffers->PSF_node_buf = init_strided_buffer(n_point);
    kemo_buffers->PSF_transes =  init_PSF_trans_buffers();
    kemo_buffers->PSF_solids =   init_PSF_solid_buffers();
    kemo_buffers->PSF_lines = init_PSF_line_buffers();
    kemo_buffers->MAP_bufs = init_MAP_buffers();
    
    kemo_buffers->Fline_bufs =  init_FieldLine_buffers();
    kemo_buffers->Tracer_bufs = init_Tracer_buffers();

    kemo_buffers->MESH_bufs =     init_MESH_buffers();
    kemo_buffers->mesh_trns_buf = init_strided_buffer(n_point);

    kemo_buffers->axis_buf =        init_strided_buffer(n_point);
    kemo_buffers->axis_index_buf =  init_gl_index_buffer(12,3);

    kemo_buffers->MESSAGE_bufs = init_MESSAGE_buffers();

    n_point = ITWO * ITHREE;
    kemo_buffers->screen_buf =  init_strided_buffer(n_point);
    
    return kemo_buffers;
};

void dealloc_kemoview_buffers(struct kemoview_buffers *kemo_buffers)
{
    dealloc_initial_cube_buffers(kemo_buffers->initial_bufs);
    dealloc_MESSAGE_buffers(kemo_buffers->MESSAGE_bufs);
    
    dealloc_MESH_buffers(kemo_buffers->MESH_bufs);
    dealloc_strided_buffer(kemo_buffers->mesh_trns_buf);

    dealloc_Tracer_buffers(kemo_buffers->Tracer_bufs);
    dealloc_FieldLine_buffers(kemo_buffers->Fline_bufs);

    dealloc_PSF_trans_buffers(kemo_buffers->PSF_transes);
    dealloc_PSF_solid_buffers(kemo_buffers->PSF_solids);
    dealloc_strided_buffer(kemo_buffers->PSF_node_buf);
    dealloc_MAP_buffers(kemo_buffers->MAP_bufs);
    
    dealloc_PSF_line_buffers(kemo_buffers->PSF_lines);

    dealloc_strided_buffer(kemo_buffers->screen_buf);
    dealloc_strided_buffer(kemo_buffers->axis_buf);
    dealloc_gl_index_buffer(kemo_buffers->axis_index_buf);

    free(kemo_buffers->kemo_lights);
    free(kemo_buffers);
    return;
};

void set_number_of_threads(int input, struct kemoview_buffers *kemo_buffers){
    if(0 < input  && input < 256){kemo_buffers->nthreads = input;};
}
int send_number_of_threads(struct kemoview_buffers *kemo_buffers){
    return kemo_buffers->nthreads;
}

void set_initial_cube_drawing(int iflag, struct view_element *view_s,
                              struct initial_cube_buffers *initial_bufs){
        if(iflag == 0 || view_s->iflag_light_check > 0){
        initial_bufs->cube_index_buf->ntot_vertex = initial_bufs->cube_index_buf->num_ele_buf
                                                   * initial_bufs->cube_index_buf->num_each_ele;
    } else {
        initial_bufs->cube_index_buf->ntot_vertex = 0;
    }
    return;
};

void set_kemoviewer_buffers(struct kemoview_mul_psf *kemo_mul_psf,
                            struct kemoview_fline *kemo_fline,
                            struct kemoview_tracer *kemo_tracer,
                            struct kemoview_mesh *kemo_mesh,
                            struct view_element *view_s,
                            struct kemoview_buffers *kemo_buffers)
{
    int iflag;
    int iflag_psf = sort_by_patch_distance_psfs(kemo_mul_psf->psf_d,
                                                kemo_mul_psf->psf_m,
                                                kemo_mul_psf->psf_a,
                                                view_s);
/* Set isolines thickness*/
    if(view_s->ncorner_tube <= 0){view_s->ncorner_tube = 12;};
    kemo_fline->fline_m->ncorner_viz_line = view_s->ncorner_tube;

    
/* Set Colors for PSF patches*/
    const_PSF_node_stack(kemo_mul_psf->psf_d, kemo_mul_psf->psf_a);
    set_color_code_for_psfs(kemo_mul_psf->psf_d, kemo_mul_psf->psf_m,
                            kemo_mul_psf->psf_a);

    if(kemo_fline->fline_m->iflag_draw_viz > 0){
        set_color_code_for_fieldlines(kemo_fline->fline_d, kemo_fline->fline_m);
    }
    if(kemo_tracer->tracer_m->iflag_draw_viz > 0){
        set_color_code_for_fieldlines(kemo_tracer->tracer_d, kemo_tracer->tracer_m);
    }

    if(view_s->iflag_view_type == VIEW_MAP) {
        iflag_psf = check_draw_map(kemo_mul_psf->psf_a);
        const_map_buffers(kemo_buffers->nthreads, kemo_mul_psf,
                          kemo_mesh->mesh_m, view_s, 
                          kemo_buffers->PSF_node_buf,
                          kemo_buffers->MAP_bufs);
    } else {
/* Set Axis data into buffer */
        if(kemo_mesh->mesh_m->iflag_axis_position == LOWER_LEFT_AXIS){
            set_lower_flex_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                                       kemo_mesh->mesh_m->dist_domains,
                                       view_s->width_axis,
                                       kemo_buffers->axis_buf,
                                       kemo_buffers->axis_index_buf);
        }else{
            set_flex_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                                 kemo_mesh->mesh_m->dist_domains,
                                 view_s->width_axis,
                                 kemo_buffers->axis_buf,
                                 kemo_buffers->axis_index_buf);
        }
        
        iflag_psf = iflag_psf + check_draw_psf(kemo_mul_psf->psf_a);

        const_PSF_node_buffer(kemo_buffers->nthreads,
                              kemo_mul_psf->psf_d, kemo_mul_psf->psf_n,
                              kemo_mul_psf->psf_a, kemo_buffers->PSF_node_buf);
        
        const_PSF_solid_objects_buffer(kemo_buffers->nthreads, view_s,
                                       kemo_mul_psf->psf_d,
                                       kemo_mul_psf->psf_n,
                                       kemo_mul_psf->psf_m,
                                       kemo_mul_psf->psf_a,
                                       kemo_buffers->PSF_solids);
        const_PSF_isolines_buffer(kemo_buffers->nthreads, view_s,
                                  kemo_mul_psf->psf_d, kemo_mul_psf->psf_n,
                                  kemo_mul_psf->psf_m, kemo_mul_psf->psf_a,
                                  kemo_mesh->mesh_m,
                                  kemo_buffers->PSF_lines);

        const_PSF_trans_objects_buffer(kemo_buffers->nthreads, view_s,
                                       kemo_mul_psf->psf_d,
                                       kemo_mul_psf->psf_n,
                                       kemo_mul_psf->psf_m,
                                       kemo_mul_psf->psf_a,
                                       kemo_buffers->PSF_transes);
        
        const_fieldlines_buffer(kemo_buffers->nthreads, view_s,
                                kemo_fline->fline_d,
                                kemo_fline->fline_dir,
                                kemo_fline->fline_m,
                                kemo_buffers->Fline_bufs);
        
        const_tracer_buffer(kemo_buffers->nthreads, view_s,
                            kemo_tracer->tracer_d,
                            kemo_tracer->tracer_m,
                            kemo_buffers->Tracer_bufs);
        
        const_solid_mesh_buffer(kemo_buffers->nthreads,
                                kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->MESH_bufs);
        const_trans_mesh_buffer(kemo_buffers->nthreads,
                                kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->mesh_trns_buf);
        
    };
    const_message_buffers(view_s->iflag_retina,
                          view_s->nx_frame, view_s->ny_frame,
                          kemo_mesh->text_color, kemo_mesh->bg_color,
                          kemo_mul_psf, kemo_fline, kemo_tracer,
                          kemo_mesh, view_s,
                          kemo_buffers->MESSAGE_bufs);
    
    const_screen_buffer(view_s->iflag_view_type, view_s->nx_frame, view_s->ny_frame,
                        kemo_buffers->screen_buf);

    /* draw example cube for empty data */
    
    iflag = kemo_mesh->mesh_m->iflag_draw_mesh + iflag_psf
            + kemo_fline->fline_m->iflag_draw_viz
            + kemo_tracer->tracer_m->iflag_draw_viz;
    set_initial_cube_drawing(iflag, view_s, kemo_buffers->initial_bufs);
    return;
};

void set_transparent_buffers(struct kemoview_mul_psf *kemo_mul_psf,
                             struct kemoview_mesh *kemo_mesh,
                             struct view_element *view_s,
                             struct kemoview_buffers *kemo_buffers)
{
    int iflag_psf = sort_by_patch_distance_psfs(kemo_mul_psf->psf_d, kemo_mul_psf->psf_m,
                                                kemo_mul_psf->psf_a, view_s);
    iflag_psf = iflag_psf + check_draw_psf(kemo_mul_psf->psf_a);
    
    const_PSF_trans_objects_buffer(kemo_buffers->nthreads, view_s,
                                   kemo_mul_psf->psf_d,
                                   kemo_mul_psf->psf_n,
                                   kemo_mul_psf->psf_m,
                                   kemo_mul_psf->psf_a,
                                   kemo_buffers->PSF_transes);

    const_trans_mesh_buffer(kemo_buffers->nthreads,
                            kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                            kemo_buffers->mesh_trns_buf);
    return;
};

void set_fast_buffers(struct kemoview_mul_psf *kemo_mul_psf, struct kemoview_fline *kemo_fline,
                      struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                      struct kemoview_buffers *kemo_buffers)
{
    if(kemo_mesh->mesh_m->iflag_axis_position == LOWER_LEFT_AXIS){
        set_lower_fixed_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                                    kemo_mesh->mesh_m->dist_domains,
                                    view_s->width_axis,
                                    kemo_buffers->axis_buf,
                                    kemo_buffers->axis_index_buf);
    }
     
    set_transparent_buffers(kemo_mul_psf, kemo_mesh, view_s, kemo_buffers);
    return;
};
