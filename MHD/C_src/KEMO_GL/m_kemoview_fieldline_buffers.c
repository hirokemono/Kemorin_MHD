
/* m_kemoview_fieldline_buffers.c */

#include "m_kemoview_fieldline_buffers.h"


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
    return Fline_bufs;
}

void dealloc_FieldLine_buffers(struct FieldLine_buffers *Fline_bufs)
{
    dealloc_strided_buffer(Fline_bufs->FLINE_line_buf);
    dealloc_strided_buffer(Fline_bufs->FLINE_tube_buf);
    free(Fline_bufs);
};


void const_fieldlines_buffer(const int nthreads, struct view_element *view_s,
                             struct fline_data *fline_d, struct fline_menu_val *fline_m,
                             struct FieldLine_buffers *Fline_bufs){
    Fline_bufs->FLINE_line_buf->num_nod_buf = 0;
    if(fline_m->iflag_draw_fline <= 0) return;
        
    set_color_code_for_fieldlines(fline_d, fline_m);

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
    if(fline_m->fieldline_thick <= 0.0){
        tube_width = ref_width * set_tube_radius_by_axis(view_s);
    }else{
        tube_width = fline_m->fieldline_thick;
    };

    Fline_bufs->FLINE_tube_buf->num_nod_buf = 0;
    if(fline_m->fieldline_type == IFLAG_PIPE){
        long num_patch = ITHREE * (ITWO*fline_m->fieldline_ncorner) * num_edge;
        set_buffer_address_4_patch(num_patch, Fline_bufs->FLINE_tube_buf);
        if(Fline_bufs->FLINE_tube_buf->num_nod_buf> 0){
            resize_strided_buffer(Fline_bufs->FLINE_tube_buf);
            num_patch = sel_fieldtubes_to_buf_pthread(IZERO, nthreads, tube_width,
                                                      fline_d, fline_m,
                                                      Fline_bufs->FLINE_tube_buf);
        };
    };

	return;
}
