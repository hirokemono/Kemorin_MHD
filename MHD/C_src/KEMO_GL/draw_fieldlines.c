
/* draw_fieldlines.c */

#include "draw_fieldlines.h"

void const_fieldlines_buffer(struct psf_data *fline_s, struct fline_menu_val *fline_m,
                             struct gl_strided_buffer *FLINE_tube_buf,
                             struct gl_strided_buffer *FLINE_line_buf){
    int ncorner = ISIX;

    FLINE_tube_buf->num_nod_buf = 0;
    FLINE_line_buf->num_nod_buf = 0;
    if(fline_m->iflag_draw_fline <= 0) return;
        
    set_color_code_for_fieldlines(fline_s, fline_m);

    if(fline_m->fieldline_type == IFLAG_PIPE){
        int num_patch = count_fieldtubes_to_buf(ncorner, fline_s);
        set_buffer_address_4_patch(ITHREE*num_patch, FLINE_tube_buf);
        if(FLINE_tube_buf->num_nod_buf> 0){
            resize_strided_buffer(FLINE_tube_buf);
            set_fieldtubes_to_buf(ncorner, fline_s, fline_m, FLINE_tube_buf);
        };
    };

    int num_edge = count_fieldlines_to_buf(fline_s);
    set_buffer_address_4_patch(ITWO*num_edge, FLINE_line_buf);
    if(FLINE_line_buf->num_nod_buf>0){
        resize_strided_buffer(FLINE_line_buf);
        set_fieldlines_to_buf(fline_s, fline_m, FLINE_line_buf);
    };
	return;
}
