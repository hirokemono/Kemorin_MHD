
/* draw_fieldlines.c */

#include "draw_fieldlines.h"

void const_fieldlines_buffer(const int nthreads,
                             struct fline_data *fline_d,
                             struct fline_menu_val *fline_m,
                             struct gl_strided_buffer *FLINE_tube_buf,
                             struct gl_strided_buffer *FLINE_line_buf){
    FLINE_tube_buf->num_nod_buf = 0;
    FLINE_line_buf->num_nod_buf = 0;
    if(fline_m->iflag_draw_fline <= 0) return;
        
    set_color_code_for_fieldlines(fline_d, fline_m);

    if(fline_m->fieldline_type == IFLAG_PIPE){
        long num_patch = count_fieldtubes_to_buf(fline_m->ncorner, fline_d);
        
        set_buffer_address_4_patch(ITHREE*num_patch, FLINE_tube_buf);
        if(FLINE_tube_buf->num_nod_buf> 0){
            resize_strided_buffer(FLINE_tube_buf);
            num_patch = sel_fieldtubes_to_buf_pthread(IZERO, nthreads,
                                                      fline_d, fline_m,
                                                      FLINE_tube_buf);
        };
    };

    long num_edge = count_fieldlines_to_buf(fline_d);
    set_buffer_address_4_patch(ITWO*num_edge, FLINE_line_buf);
    if(FLINE_line_buf->num_nod_buf>0){
        resize_strided_buffer(FLINE_line_buf);
        sel_fieldlines_to_buf_pthread(IZERO, nthreads,
                                      fline_d, fline_m, FLINE_line_buf);
    };
	return;
}
