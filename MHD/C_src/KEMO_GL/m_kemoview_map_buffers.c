/*
//  m_kemoview_map_buffers.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "m_kemoview_map_buffers.h"

struct MAP_buffers * init_MAP_buffers(void)
{
    struct MAP_buffers *MAP_bufs = (struct MAP_buffers *) malloc(sizeof(struct MAP_buffers));
    if(MAP_bufs == NULL){
        printf("malloc error in MAP_buffers \n");
        exit(0);
    };
    
    int n_point = 1024;
    MAP_bufs->MAP_solid_index_buf = init_gl_index_buffer(12, 3);
    MAP_bufs->MAP_solid_buf =       init_strided_buffer(n_point);
    MAP_bufs->MAP_isoline_buf =     init_strided_buffer(n_point);
    MAP_bufs->MAP_isotube_index_buf = init_gl_index_buffer(12, 3);

    MAP_bufs->MAP_coast_line_buf = init_strided_buffer(n_point);
    MAP_bufs->MAP_coast_tube_buf = init_strided_buffer(n_point);
    MAP_bufs->MAP_coast_index_buf = init_gl_index_buffer(12, 3);

    return MAP_bufs;
}

void dealloc_MAP_buffers(struct MAP_buffers *MAP_bufs)
{
    dealloc_gl_index_buffer(MAP_bufs->MAP_coast_index_buf);
    dealloc_strided_buffer(MAP_bufs->MAP_coast_tube_buf);
    dealloc_strided_buffer(MAP_bufs->MAP_coast_line_buf);

    dealloc_gl_index_buffer(MAP_bufs->MAP_isotube_index_buf);
    dealloc_strided_buffer(MAP_bufs->MAP_isoline_buf);
    dealloc_strided_buffer(MAP_bufs->MAP_solid_buf);
    dealloc_gl_index_buffer(MAP_bufs->MAP_solid_index_buf);
    
    free(MAP_bufs);
};

void const_map_buffers(int nthreads, struct kemoview_mul_psf *kemo_mul_psf,
                       struct mesh_menu_val *mesh_m, struct view_element *view_s,
                       struct gl_strided_buffer *PSF_node_buf, struct MAP_buffers *MAP_bufs)
{
    if(view_s->shading_mode == SMOOTH_SHADE){
        set_map_node_buffer(nthreads, kemo_mul_psf->psf_d, kemo_mul_psf->psf_n,
                            kemo_mul_psf->psf_a, PSF_node_buf);
        const_PSF_patch_index_buffer(nthreads,
                                     IZERO, kemo_mul_psf->psf_a->istack_solid_psf_patch,
                                     kemo_mul_psf->psf_d, kemo_mul_psf->psf_a,
                                     MAP_bufs->MAP_solid_index_buf);
        MAP_bufs->MAP_solid_buf->num_nod_buf = 0;
    }else{
        set_map_patch_buffer(nthreads,
                             IZERO, kemo_mul_psf->psf_a->istack_solid_psf_patch,
                             kemo_mul_psf->psf_d, kemo_mul_psf->psf_m, kemo_mul_psf->psf_a,
                             MAP_bufs->MAP_solid_buf);
        MAP_bufs->MAP_solid_index_buf->ntot_vertex = 0;
    }
    
    
    set_map_PSF_isolines_buffer(nthreads, view_s,
                                kemo_mul_psf->psf_d, kemo_mul_psf->psf_n,
                                kemo_mul_psf->psf_m, kemo_mul_psf->psf_a,
                                MAP_bufs->MAP_isoline_buf,
                                MAP_bufs->MAP_isotube_index_buf);
    
    set_map_coastline_line_buffer(mesh_m, MAP_bufs->MAP_coast_line_buf);
    if(view_s->iflag_coastline_tube){
        set_map_coastline_tube_buffer(mesh_m, view_s,
                                      MAP_bufs->MAP_coast_tube_buf,
                                      MAP_bufs->MAP_coast_index_buf);
        MAP_bufs->MAP_coast_line_buf->num_nod_buf = 0;
    }else{
        MAP_bufs->MAP_coast_index_buf->ntot_vertex = 0;
    }
    return;
}

