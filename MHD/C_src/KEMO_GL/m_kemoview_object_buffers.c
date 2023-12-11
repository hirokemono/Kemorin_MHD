/*
//  m_kemoview_object_buffers.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "m_kemoview_object_buffers.h"

struct kemoview_buffers * init_kemoview_buffers(void)
{
    int n_point = 1024;

    struct kemoview_buffers *kemo_buffers = (struct kemoview_buffers *) malloc(sizeof(struct kemoview_buffers));
    if(kemo_buffers == NULL){
        printf("malloc error for kemoview_buffers\n");
        exit(0);
    }
    if((kemo_buffers->kemo_lights = (struct phong_lights *) malloc(sizeof(struct phong_lights))) == NULL){
        printf("malloc error in phong_lights \n");
        exit(0);
    };

    kemo_buffers->cube_buf = init_strided_buffer(n_point);
    kemo_buffers->cube_index_buf = alloc_gl_index_buffer(12, 3);
    CubeNode_to_buf(0.5f, kemo_buffers->cube_buf, kemo_buffers->cube_index_buf);

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

    kemo_buffers->coast_buf =    init_strided_buffer((n_point));
    kemo_buffers->sph_grid_buf = init_strided_buffer(n_point);
    
    kemo_buffers->ncorner_axis = ISIX*8;
    kemo_buffers->axis_buf = init_strided_buffer(n_point);

    n_point = count_colorbar_box_buffer(IONE, 128);
    kemo_buffers->cbar_buf = init_strided_buffer(n_point);

    n_point = ITWO * ITHREE;
    kemo_buffers->min_buf =  init_strided_buffer(n_point);
    kemo_buffers->max_buf =  init_strided_buffer(n_point);
    kemo_buffers->zero_buf = init_strided_buffer(n_point);
    kemo_buffers->time_buf = init_strided_buffer(n_point);
    kemo_buffers->msg_buf =  init_strided_buffer(n_point);

    kemo_buffers->screen_buf =  init_strided_buffer(n_point);
    
    kemo_buffers->cbar_min_image =  alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, NCHARA_CBOX);
    kemo_buffers->cbar_max_image =  alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, NCHARA_CBOX);
    kemo_buffers->cbar_zero_image = alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, NCHARA_CBOX);
    kemo_buffers->tlabel_image =    alloc_line_text_image(IWIDTH_TLABEL, IHIGHT_TXT, NCHARA_CBOX);
    kemo_buffers->message_image =   alloc_line_text_image(IWIDTH_MSG, IHIGHT_MSG, NCHARA_MSG);
    return kemo_buffers;
};

void dealloc_kemoview_buffers(struct kemoview_buffers *kemo_buffers)
{
    dealloc_line_text_image(kemo_buffers->message_image);
    dealloc_line_text_image(kemo_buffers->tlabel_image);
    dealloc_line_text_image(kemo_buffers->cbar_zero_image);
    dealloc_line_text_image(kemo_buffers->cbar_max_image);
    dealloc_line_text_image(kemo_buffers->cbar_min_image);

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

    dealloc_strided_buffer(kemo_buffers->screen_buf);

    dealloc_strided_buffer(kemo_buffers->axis_buf);

    free(kemo_buffers->kemo_lights);
    free(kemo_buffers);
    return;
};

void set_kemoviewer_buffers(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline,
                            struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                            struct kemoview_buffers *kemo_buffers)
{
    int iflag;
    int iflag_psf = 0;
    /* Set transfer matrices */
    if(view_s->iflag_view_type == VIEW_MAP) {
        iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
                                                kemo_psf->psf_a, view_s);
        iflag_psf = check_draw_map(kemo_psf->psf_a);
        
        set_map_patch_buffer(IZERO, kemo_psf->psf_a->istack_solid_psf_patch,
                             kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                             kemo_buffers->MAP_solid_buf);
        set_map_PSF_isolines_buffer(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                                    view_s, kemo_buffers->MAP_isoline_buf);
        
        set_map_coastline_buffer(kemo_mesh->mesh_m, kemo_buffers->coast_buf);
        set_map_flame_buffer(kemo_mesh->mesh_m, kemo_buffers->sph_grid_buf);
    } else {
/* Set Axis data into buffer */
        double axis_radius = 4.0;
        set_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                        kemo_mesh->mesh_m->dist_domains,
                        kemo_buffers->ncorner_axis, axis_radius,
                        kemo_buffers->axis_buf);
        
        iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
                                                kemo_psf->psf_a, view_s);
        iflag_psf = iflag_psf + check_draw_psf(kemo_psf->psf_a);
        set_color_code_for_psfs(kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a);
        
        const_PSF_solid_objects_buffer(view_s, kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                                       kemo_buffers->PSF_solid_buf, kemo_buffers->PSF_stxur_buf,
                                       kemo_buffers->PSF_isoline_buf, kemo_buffers->PSF_arrow_buf);
        const_PSF_trans_objects_buffer(view_s, kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                                       kemo_buffers->PSF_trns_buf, kemo_buffers->PSF_ttxur_buf);

        set_coastline_buffer(kemo_mesh->mesh_m, kemo_buffers->coast_buf);
        set_sph_flame_buffer(kemo_mesh->mesh_m, kemo_buffers->sph_grid_buf);
        
        const_fieldlines_buffer(kemo_fline->fline_d, kemo_fline->fline_m,
                                kemo_buffers->FLINE_tube_buf, kemo_buffers->FLINE_line_buf);
        
        const_solid_mesh_buffer(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->mesh_solid_buf, kemo_buffers->mesh_grid_buf,
                                kemo_buffers->mesh_node_buf);
        const_trans_mesh_buffer(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                                kemo_buffers->mesh_trns_buf);
        
    };
    const_timelabel_buffer(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                           kemo_mesh->text_color, kemo_mesh->bg_color,
                           kemo_psf->psf_a, kemo_buffers->tlabel_image, kemo_buffers->time_buf);
    
    const_colorbar_buffer(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                          kemo_mesh->text_color, kemo_mesh->bg_color,
                          kemo_psf->psf_m, kemo_psf->psf_a,
                          kemo_buffers->min_buf,  kemo_buffers->cbar_min_image,
                          kemo_buffers->max_buf,  kemo_buffers->cbar_max_image,
                          kemo_buffers->zero_buf, kemo_buffers->cbar_zero_image,
                          kemo_buffers->cbar_buf);
    
    const_message_buffer(view_s->iflag_retina, view_s->nx_frame, view_s->ny_frame,
                         kemo_buffers->msg_buf, kemo_buffers->message_image);
    
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

void set_transparent_buffers(struct kemoview_psf *kemo_psf, struct kemoview_mesh *kemo_mesh,
                             struct view_element *view_s, struct kemoview_buffers *kemo_buffers)
{
    double axis_radius = 4.0;
    set_axis_to_buf(view_s, kemo_mesh->mesh_m->iflag_draw_axis,
                    kemo_mesh->mesh_m->dist_domains,
                    kemo_buffers->ncorner_axis, axis_radius,
                    kemo_buffers->axis_buf);

    int iflag_psf = sort_by_patch_distance_psfs(kemo_psf->psf_d, kemo_psf->psf_m,
                                            kemo_psf->psf_a, view_s);
    iflag_psf = iflag_psf + check_draw_psf(kemo_psf->psf_a);
    
    const_PSF_trans_objects_buffer(view_s, kemo_psf->psf_d, kemo_psf->psf_m, kemo_psf->psf_a,
                                   kemo_buffers->PSF_trns_buf, kemo_buffers->PSF_ttxur_buf);
    const_trans_mesh_buffer(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view_s,
                            kemo_buffers->mesh_trns_buf);
    return;
};
