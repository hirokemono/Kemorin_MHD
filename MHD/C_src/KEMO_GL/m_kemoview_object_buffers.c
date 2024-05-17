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
    
    kemo_buffers->cube_buf =        init_strided_buffer(n_point);
    kemo_buffers->cube_index_buf = alloc_gl_index_buffer(12, 3);

    kemo_buffers->PSF_solid_buf =   init_strided_buffer(n_point);
    kemo_buffers->PSF_stxur_buf =   init_strided_buffer(n_point);
    kemo_buffers->PSF_trns_buf =    init_strided_buffer(n_point);
    kemo_buffers->PSF_ttxur_buf =   init_strided_buffer(n_point);
    kemo_buffers->PSF_isoline_buf = init_strided_buffer(n_point);
    kemo_buffers->PSF_isotube_buf = init_strided_buffer(n_point);
    kemo_buffers->PSF_arrow_buf =   init_strided_buffer(n_point);

    kemo_buffers->MAP_solid_buf =   init_strided_buffer(n_point);
    kemo_buffers->MAP_isoline_buf = init_strided_buffer(n_point);

    kemo_buffers->FLINE_line_buf =  init_strided_buffer(n_point);
    kemo_buffers->FLINE_tube_buf =  init_strided_buffer(n_point);

    kemo_buffers->mesh_solid_buf =  init_strided_buffer(n_point);
    kemo_buffers->mesh_grid_buf =   init_strided_buffer(n_point);
    kemo_buffers->mesh_node_buf =   init_strided_buffer(n_point);
    kemo_buffers->mesh_trns_buf =   init_strided_buffer(n_point);

    kemo_buffers->iflag_coastline_tube = ON;
    kemo_buffers->coast_line_buf = init_strided_buffer(n_point);
    kemo_buffers->coast_tube_buf = init_strided_buffer(n_point);
    
    kemo_buffers->axis_buf =  init_strided_buffer(n_point);

    n_point = count_colorbar_box_buffer(IONE, 128);
    kemo_buffers->cbar_buf =        init_strided_buffer(n_point);

    kemo_buffers->cbar_min_buf =  alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, (ITWO * ITHREE), NCHARA_CBOX);
    kemo_buffers->cbar_max_buf =  alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, (ITWO * ITHREE), NCHARA_CBOX);
    kemo_buffers->cbar_zero_buf = alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, (ITWO * ITHREE), NCHARA_CBOX);
    kemo_buffers->timelabel_buf = alloc_line_text_image(IWIDTH_TLABEL, IHIGHT_TXT, (ITWO * ITHREE), NCHARA_CBOX);
    kemo_buffers->message_buf =   alloc_line_text_image(IWIDTH_MSG, IHIGHT_MSG,    (ITWO * ITHREE), NCHARA_MSG);

    n_point = ITWO * ITHREE;
    kemo_buffers->screen_buf =  init_strided_buffer(n_point);
    
    return kemo_buffers;
};

void dealloc_kemoview_buffers(struct kemoview_buffers *kemo_buffers)
{
    dealloc_line_text_image(kemo_buffers->message_buf);
    dealloc_line_text_image(kemo_buffers->timelabel_buf);
    dealloc_line_text_image(kemo_buffers->cbar_zero_buf);
    dealloc_line_text_image(kemo_buffers->cbar_max_buf);
    dealloc_line_text_image(kemo_buffers->cbar_min_buf);

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
    dealloc_strided_buffer(kemo_buffers->PSF_isotube_buf);
    dealloc_strided_buffer(kemo_buffers->PSF_arrow_buf);

    dealloc_strided_buffer(kemo_buffers->MAP_solid_buf);
    dealloc_strided_buffer(kemo_buffers->MAP_isoline_buf);

    dealloc_strided_buffer(kemo_buffers->coast_tube_buf);
    dealloc_strided_buffer(kemo_buffers->coast_line_buf);

    dealloc_strided_buffer(kemo_buffers->cbar_buf);

    dealloc_strided_buffer(kemo_buffers->screen_buf);

    dealloc_strided_buffer(kemo_buffers->axis_buf);

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

void set_kemoviewer_buffers(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline,
                            struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                            struct kemoview_buffers *kemo_buffers)
{
    int iflag;
    int iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
                                                kemo_psf->psf_a, view_s);
/* Set isolines thickness*/
    if(view_s->width_tube <= 0.0){view_s->width_tube = set_tube_radius_by_view(view_s, 1.5);};
    if(view_s->ncorner_tube <= 0){view_s->ncorner_tube = 12;};
    kemo_fline->fline_m->fieldline_ncorner = view_s->ncorner_tube;

    if(view_s->iflag_view_type == VIEW_MAP) {
        iflag_psf = check_draw_map(kemo_psf->psf_a);
        
        set_map_patch_buffer(kemo_buffers->nthreads,
                             IZERO, kemo_psf->psf_a->istack_solid_psf_patch,
                             kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                             kemo_buffers->MAP_solid_buf);
        set_map_PSF_isolines_buffer(kemo_buffers->nthreads,
                                    kemo_psf->psf_d, kemo_psf->psf_m,
                                    kemo_psf->psf_a, view_s,
                                    kemo_buffers->MAP_isoline_buf);
        
        set_map_coastline_line_buffer(kemo_mesh->mesh_m, kemo_buffers->coast_line_buf);
        if(kemo_buffers->iflag_coastline_tube){
            set_map_coastline_tube_buffer(view_s->ncorner_tube, view_s->width_tube,
                                          kemo_mesh->mesh_m, kemo_buffers->coast_tube_buf);
            kemo_buffers->coast_line_buf->num_nod_buf = 0;
        }else{
            kemo_buffers->coast_tube_buf->num_nod_buf = 0;
        }
        
    } else {
/* Set Axis data into buffer */
        double axis_radius = 4.0;
        set_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                        kemo_mesh->mesh_m->dist_domains, axis_radius,
                        kemo_buffers->axis_buf);
        
        iflag_psf = iflag_psf + check_draw_psf(kemo_psf->psf_a);
        set_color_code_for_psfs(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a);
        
        kemo_buffers->num_isoline_buf = const_PSF_solid_objects_buffer(kemo_buffers->nthreads,
                                                                       view_s, kemo_psf->psf_d,
                                                                       kemo_psf->psf_m, kemo_psf->psf_a,
                                                                       kemo_buffers->PSF_solid_buf,
                                                                       kemo_buffers->PSF_stxur_buf,
                                                                       kemo_buffers->PSF_isoline_buf,
                                                                       kemo_buffers->PSF_isotube_buf,
                                                                       kemo_buffers->PSF_arrow_buf);
        kemo_buffers->PSF_isoline_buf->num_nod_buf = 0;

        const_PSF_trans_objects_buffer(kemo_buffers->nthreads,
                                       view_s, kemo_psf->psf_d,
                                       kemo_psf->psf_m, kemo_psf->psf_a,
                                       kemo_buffers->PSF_trns_buf,
                                       kemo_buffers->PSF_ttxur_buf);

        set_coastline_line_buffer(kemo_mesh->mesh_m, kemo_buffers->coast_line_buf);
        if(kemo_buffers->iflag_coastline_tube){
            set_coastline_tube_buffer(view_s->ncorner_tube, view_s->width_tube,
                                      kemo_mesh->mesh_m, kemo_buffers->coast_tube_buf);
            kemo_buffers->coast_line_buf->num_nod_buf = 0;
        }else{
            kemo_buffers->coast_tube_buf->num_nod_buf = 0;
        }
        
        const_fieldlines_buffer(kemo_buffers->nthreads,
                                kemo_fline->fline_d,
                                kemo_fline->fline_m,
                                kemo_buffers->FLINE_tube_buf,
                                kemo_buffers->FLINE_line_buf);
        
        const_solid_mesh_buffer(kemo_buffers->nthreads,
                                kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->mesh_solid_buf,
                                kemo_buffers->mesh_grid_buf,
                                kemo_buffers->mesh_node_buf);
        const_trans_mesh_buffer(kemo_buffers->nthreads,
                                kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->mesh_trns_buf);
        
    };
    const_timelabel_buffer(view_s->iflag_retina,
                           view_s->nx_frame, view_s->ny_frame,
                           kemo_mesh->text_color, kemo_mesh->bg_color,
                           kemo_psf->psf_a,
                           kemo_buffers->timelabel_buf);
    
    const_colorbar_buffer(view_s->iflag_retina,
                          view_s->nx_frame, view_s->ny_frame,
                          kemo_mesh->text_color, kemo_mesh->bg_color,
                          kemo_psf->psf_m, kemo_psf->psf_a,
                          kemo_buffers->cbar_min_buf,
                          kemo_buffers->cbar_max_buf,
                          kemo_buffers->cbar_zero_buf,
                          kemo_buffers->cbar_buf);
    
    const_message_buffer(view_s->iflag_retina,
                         view_s->nx_frame, view_s->ny_frame,
                         kemo_buffers->message_buf->vertex,
                         kemo_buffers->message_buf);
    
    const_screen_buffer(view_s->iflag_view_type, view_s->nx_frame, view_s->ny_frame,
                        kemo_buffers->screen_buf);

    /* draw example cube for empty data */
    
    iflag = kemo_mesh->mesh_m->iflag_draw_mesh + iflag_psf + kemo_fline->fline_m->iflag_draw_fline;
    if(iflag == 0){
        kemo_buffers->cube_buf->num_nod_buf = kemo_buffers->cube_index_buf->nsize_buf;
    } else {
        kemo_buffers->cube_buf->num_nod_buf = 0;
    }
    return;
};

void set_transparent_buffers(struct kemoview_psf *kemo_psf,
                             struct kemoview_mesh *kemo_mesh,
                             struct view_element *view_s,
                             struct kemoview_buffers *kemo_buffers)
{
    int iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
                                                kemo_psf->psf_a, view_s);
    iflag_psf = iflag_psf + check_draw_psf(kemo_psf->psf_a);
    
    const_PSF_trans_objects_buffer(kemo_buffers->nthreads,
                                   view_s, kemo_psf->psf_d,
                                   kemo_psf->psf_m, kemo_psf->psf_a,
                                   kemo_buffers->PSF_trns_buf,
                                   kemo_buffers->PSF_ttxur_buf);
    const_trans_mesh_buffer(kemo_buffers->nthreads,
                            kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                            kemo_buffers->mesh_trns_buf);
    return;
};

void set_fast_buffers(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline,
                      struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                      struct kemoview_buffers *kemo_buffers)
{
    double axis_radius = 4.0;
    set_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                    kemo_mesh->mesh_m->dist_domains, axis_radius,
                    kemo_buffers->axis_buf);
    set_transparent_buffers(kemo_psf, kemo_mesh, view_s, kemo_buffers);
    
    kemo_buffers->coast_line_buf->num_nod_buf
        = ITWO * count_coastline_line_buffer(kemo_mesh->mesh_m);
    kemo_buffers->coast_tube_buf->num_nod_buf = 0;

    kemo_buffers->PSF_isoline_buf->num_nod_buf = kemo_buffers->num_isoline_buf;
    kemo_buffers->PSF_isotube_buf->num_nod_buf = 0;

    if(kemo_fline->fline_m->iflag_draw_fline > 0){
        kemo_buffers->FLINE_line_buf->num_nod_buf
        = ITWO * count_fieldlines_to_buf(kemo_fline->fline_d);
        kemo_buffers->FLINE_tube_buf->num_nod_buf = 0;
    }
    return;
};

