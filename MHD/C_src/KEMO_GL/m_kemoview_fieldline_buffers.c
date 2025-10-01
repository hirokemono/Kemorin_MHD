/*
//  m_kemoview_fieldline_buffers.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "m_kemoview_fieldline_buffers.h"

#include "set_each_arrow_line_c.h"


struct FieldLine_buffers * init_FieldLine_buffers(void)
{
    struct FieldLine_buffers *Fline_bufs = (struct FieldLine_buffers *) malloc(sizeof(struct FieldLine_buffers));
    if(Fline_bufs == NULL){
        printf("malloc error in FieldLine_buffers \n");
        exit(0);
    };
    
    long n_point = 1024;
    Fline_bufs->FLINE_line_buf =       init_strided_buffer(n_point);
    Fline_bufs->FLINE_tube_buf =       init_strided_buffer(n_point);
    Fline_bufs->FLINE_tube_index_buf = init_gl_index_buffer(12, 3);
    Fline_bufs->FLINE_tube_index_buf->num_ele_buf = 0;
    Fline_bufs->FLINE_tube_index_buf->ntot_vertex = 0;    
    return Fline_bufs;
}

void dealloc_FieldLine_buffers(struct FieldLine_buffers *Fline_bufs)
{
    dealloc_strided_buffer(Fline_bufs->FLINE_line_buf);
    dealloc_strided_buffer(Fline_bufs->FLINE_tube_buf);
    dealloc_gl_index_buffer(Fline_bufs->FLINE_tube_index_buf);
    free(Fline_bufs);
};


void const_fieldlines_buffer(const int nthreads, struct view_element *view_s,
                             struct psf_data *fline_d,
                             struct fline_directions *fline_dir,
                             struct psf_menu_val *fline_m,
                             struct FieldLine_buffers *Fline_bufs){
    Fline_bufs->FLINE_line_buf->num_nod_buf =       0;
    Fline_bufs->FLINE_tube_buf->num_nod_buf =       0;
//    Fline_bufs->FLINE_tube_index_buf->ntot_vertex = 0;
    if(fline_m->iflag_draw_viz <= 0) return;
    
    long num_edge = count_fieldlines_to_buf(fline_d);
    set_buffer_address_4_patch(ITWO*num_edge, Fline_bufs->FLINE_line_buf);
    if(Fline_bufs->FLINE_line_buf->num_nod_buf>0){
        resize_strided_buffer(Fline_bufs->FLINE_line_buf);
        sel_fieldlines_to_buf_pthread(IZERO, nthreads,
                                      fline_d, fline_m,
                                      Fline_bufs->FLINE_line_buf);
    };

    double ref_width = 1.5;
    double tube_width;
    if(fline_m->viz_line_width <= 0.0){
        tube_width = ref_width * set_tube_radius_by_axis(view_s);
    }else{
        tube_width = fline_m->viz_line_width;
    };
    
    if(fline_m->viz_line_type == IFLAG_PIPE){
        long num_node = ITWO*(fline_m->ncorner_viz_line + 1) * num_edge;
        long num_patch = IFOUR * fline_m->ncorner_viz_line * num_edge;
        
        set_buffer_address_4_patch(num_node, Fline_bufs->FLINE_tube_buf);
        if(Fline_bufs->FLINE_tube_buf->num_nod_buf> 0){
            resize_strided_buffer(Fline_bufs->FLINE_tube_buf);
            resize_gl_index_buffer(num_patch, ITHREE, Fline_bufs->FLINE_tube_index_buf);

            num_patch = sel_fieldtubes_to_buf_pthread(IZERO, nthreads, tube_width,
                                                      fline_d, fline_dir, fline_m,
                                                      Fline_bufs->FLINE_tube_buf,
                                                      Fline_bufs->FLINE_tube_index_buf);
        };
    };

	return;
}
