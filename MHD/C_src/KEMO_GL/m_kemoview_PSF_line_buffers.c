/*
//  m_kemoview_PSF_line_buffers.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "m_kemoview_PSF_line_buffers.h"

struct PSF_line_buffers * init_PSF_line_buffers(void)
{
    struct PSF_line_buffers *PSF_lines = (struct PSF_line_buffers *) malloc(sizeof(struct PSF_line_buffers));
    if(PSF_lines == NULL){
        printf("malloc error in PSF_line_buffers \n");
        exit(0);
    };
    
    long n_point = 1024;
    PSF_lines->PSF_isoline_buf = init_strided_buffer(n_point);
    PSF_lines->PSF_isotube_buf = init_strided_buffer(n_point);
    PSF_lines->PSF_isotube_index_buf = init_gl_index_buffer(12, 3);

    PSF_lines->PSF_arrow_buf =   init_strided_buffer(n_point);
    PSF_lines->PSF_arrow_index_buf =  init_gl_index_buffer(12, 3);

    PSF_lines->coast_line_buf =  init_strided_buffer(n_point);
    PSF_lines->coast_tube_buf =  init_strided_buffer(n_point);
    PSF_lines->coast_index_buf =  init_gl_index_buffer(12, 3);
    return PSF_lines;
}

void dealloc_PSF_line_buffers(struct PSF_line_buffers *PSF_lines)
{
    dealloc_gl_index_buffer(PSF_lines->PSF_isotube_index_buf);
    dealloc_strided_buffer(PSF_lines->coast_tube_buf);
    dealloc_strided_buffer(PSF_lines->coast_line_buf);
    
    dealloc_gl_index_buffer(PSF_lines->PSF_arrow_index_buf);
    dealloc_strided_buffer(PSF_lines->PSF_arrow_buf);
    
    dealloc_gl_index_buffer(PSF_lines->coast_index_buf);
    dealloc_strided_buffer(PSF_lines->PSF_isotube_buf);
    dealloc_strided_buffer(PSF_lines->PSF_isoline_buf);
    free(PSF_lines);
};

void const_PSF_isolines_buffer(const int nthreads, struct view_element *view_s,
                               struct psf_data **psf_s, struct psf_normals **psf_n,
                               struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                               struct mesh_menu_val *mesh_m, 
                               struct PSF_line_buffers *PSF_lines){
    const_PSF_isoline_buffer(nthreads, view_s, psf_s, psf_n, psf_m, psf_a, 
                             PSF_lines->PSF_isoline_buf);

    const_PSF_isotube_buffer(nthreads, view_s,
                             psf_s, psf_n, psf_m, psf_a,
                             PSF_lines->PSF_isotube_buf,
                             PSF_lines->PSF_isotube_index_buf);

    const_PSF_arrow_buffer(nthreads, view_s, psf_s, psf_n, psf_m, psf_a,
                           PSF_lines->PSF_arrow_buf,
                           PSF_lines->PSF_arrow_index_buf);
    
    
    set_coastline_line_buffer(mesh_m, PSF_lines->coast_line_buf);
    if(view_s->iflag_coastline_tube){
        set_coastline_tube_buffer(mesh_m, view_s,
                                  PSF_lines->coast_tube_buf,
                                  PSF_lines->coast_index_buf);
    }else{
        PSF_lines->coast_index_buf->ntot_vertex = 0;
    }
    return;
}
