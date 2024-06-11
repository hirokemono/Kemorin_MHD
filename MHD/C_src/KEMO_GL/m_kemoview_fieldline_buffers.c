
/* m_kemoview_fieldline_buffers.c */

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
    return Fline_bufs;
}

void dealloc_FieldLine_buffers(struct FieldLine_buffers *Fline_bufs)
{
    dealloc_strided_buffer(Fline_bufs->FLINE_line_buf);
    dealloc_strided_buffer(Fline_bufs->FLINE_tube_buf);
    free(Fline_bufs);
};


void const_fieldlines_buffer(const int nthreads, struct view_element *view_s,
                             struct psf_data *fline_d,
                             struct fline_directions *fline_dir,
                             struct psf_menu_val *fline_m,
                             struct FieldLine_buffers *Fline_bufs){
    Fline_bufs->FLINE_line_buf->num_nod_buf = 0;
    if(fline_m->iflag_draw_viz <= 0) return;
        
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
    if(fline_m->viz_line_width <= 0.0){
        tube_width = ref_width * set_tube_radius_by_axis(view_s);
    }else{
        tube_width = fline_m->viz_line_width;
    };

    Fline_bufs->FLINE_tube_buf->num_nod_buf = 0;
    if(fline_m->viz_line_type == IFLAG_PIPE){
        long num_patch = ITHREE * (ITWO*fline_m->ncorner_viz_line) * num_edge;
        set_buffer_address_4_patch(num_patch, Fline_bufs->FLINE_tube_buf);
        if(Fline_bufs->FLINE_tube_buf->num_nod_buf> 0){
            resize_strided_buffer(Fline_bufs->FLINE_tube_buf);
            num_patch = sel_fieldtubes_to_buf_pthread(IZERO, nthreads, tube_width,
                                                      fline_d, fline_dir, fline_m,
                                                      Fline_bufs->FLINE_tube_buf);
        };
    };

	return;
}



long set_tracer_ico_to_buf(const long ist_tri, 
                                    long ist_nod, long ied_nod,
                                    struct psf_data *tracer_d, 
                                    struct psf_menu_val *tracer_m,
                                    struct gl_strided_buffer *Tracer_buf){
    long inum_tri = ist_tri;
    for(long inod = ist_nod; inod < ied_nod; inod++){
        inum_tri = set_icosahedron_strided_buffer(inum_tri, 
                                                  tracer_m->viz_line_width,
                                                  &tracer_d->xyzw_viz[4*inod],
                                                  &tracer_d->color_nod[4*inod],
                                                  Tracer_buf);
    };
    return inum_tri;
}

long set_tracer_arrow_to_buf(const long ist_tri, 
                             long ist_nod, long ied_nod,
                             struct psf_data *tracer_d, 
                             struct psf_menu_val *tracer_m,
                             struct gl_strided_buffer *Tracer_buf){
    double xyzw_line[8], dir_line[8];
    long inum_tri = ist_tri;
    for(long inod = ist_nod; inod < ied_nod; inod++){
        set_line_for_tracer_arrow((int) tracer_d->istack_comp[tracer_m->if_draw_viz],
                                  inod, tracer_d, tracer_m,
                                  xyzw_line, dir_line);

        inum_tri = set_cone_strided_buffer(inum_tri, tracer_m->ncorner_viz_line,
                                           tracer_m->viz_line_width,
                                           xyzw_line, dir_line,
                                           &tracer_d->color_nod[4*inod],
                                           Tracer_buf);
    };
    return inum_tri;
}

void const_tracer_buffer(const int nthreads, struct view_element *view_s,
                         struct psf_data *tracer_d,
                         struct psf_menu_val *tracer_m,
                         struct gl_strided_buffer *Tracer_buf){
    Tracer_buf->num_nod_buf = 0;
    if(tracer_m->iflag_draw_viz <= 0) return;
    long num_patch = ITHREE * num_icosahedron_patch() * tracer_d->nnod_viz;
    set_buffer_address_4_patch(num_patch, Tracer_buf);
    set_color_code_for_fieldlines(tracer_d, tracer_m);
    num_patch = set_tracer_ico_to_buf(IZERO, IZERO, tracer_d->nnod_viz,
                                      tracer_d, tracer_m, Tracer_buf);
    return;
}
