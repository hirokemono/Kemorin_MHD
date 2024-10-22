/*
//  m_kemoview_tracer_buffers.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "m_kemoview_tracer_buffers.h"

#include "set_each_arrow_line_c.h"


struct Tracer_buffers * init_Tracer_buffers(void)
{
    struct Tracer_buffers *Tracer_bufs
            = (struct Tracer_buffers *) malloc(sizeof(struct Tracer_buffers));
    if(Tracer_bufs == NULL){
        printf("malloc error in Tracer_buffers \n");
        exit(0);
    };
    
    long n_point = 1024;
    Tracer_bufs->Tracer_dot_buf =       init_strided_buffer(n_point);
    Tracer_bufs->Tracer_ico_buf =       init_strided_buffer(n_point);
    return Tracer_bufs;
}

void dealloc_Tracer_buffers(struct Tracer_buffers *Tracer_bufs)
{
    dealloc_strided_buffer(Tracer_bufs->Tracer_dot_buf);
    dealloc_strided_buffer(Tracer_bufs->Tracer_ico_buf);
    free(Tracer_bufs);
};





long set_tracer_ico_to_buf(const long ist_tri, 
                                    long ist_nod, long ied_nod,
                                    struct psf_data *tracer_d, 
                                    struct psf_menu_val *tracer_m,
                                    struct gl_strided_buffer *Tracer_ico_buf){
    long inum_tri = ist_tri;
    for(long inod = ist_nod; inod < ied_nod; inod++){
        inum_tri = set_icosahedron_strided_buffer(inum_tri, 
                                                  tracer_m->viz_line_width,
                                                  &tracer_d->xyzw_viz[4*inod],
                                                  &tracer_d->color_nod[4*inod],
                                                  Tracer_ico_buf);
    };
    return inum_tri;
}

long set_tracer_arrow_to_buf(const long ist_tri, 
                             long ist_nod, long ied_nod,
                             struct psf_data *tracer_d, 
                             struct psf_menu_val *tracer_m,
                             struct gl_strided_buffer *Tracer_ico_buf){
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
                                           Tracer_ico_buf);
    };
    return inum_tri;
}





void const_tracer_buffer(const int nthreads, struct view_element *view_s,
                         struct psf_data *tracer_d,
                         struct psf_menu_val *tracer_m,
                         struct Tracer_buffers *Tracer_bufs){
    Tracer_bufs->Tracer_ico_buf->num_nod_buf = 0;
    Tracer_bufs->Tracer_dot_buf->num_nod_buf = 0;
    if(tracer_m->iflag_draw_viz <= 0) return;
    
    set_color_code_for_fieldlines(tracer_d, tracer_m);
    
    long num_points = tracer_d->nnod_viz;
    set_buffer_address_4_patch(num_points, Tracer_bufs->Tracer_dot_buf);
    if(Tracer_bufs->Tracer_dot_buf->num_nod_buf>0){
        resize_strided_buffer(Tracer_bufs->Tracer_dot_buf);
        
        set_nodes_strided_buffer(IZERO, 
                                 tracer_d->nnod_viz, tracer_d->xyzw_viz,
                                 tracer_d->xyzw_viz, tracer_d->color_nod, 
                                 tracer_d->xyzw_viz, Tracer_bufs->Tracer_dot_buf);
    };
    
    double ref_width = 1.5;
    double tube_width;
    if(tracer_m->viz_line_width <= 0.0){
        tube_width = ref_width * set_tube_radius_by_axis(view_s);
    }else{
        tube_width = tracer_m->viz_line_width;
    };
    
    Tracer_bufs->Tracer_ico_buf->num_nod_buf = 0;
    long num_patch = ITHREE * num_icosahedron_patch() * tracer_d->nnod_viz;
    set_buffer_address_4_patch(num_patch, Tracer_bufs->Tracer_ico_buf);
    
    if(Tracer_bufs->Tracer_ico_buf->num_nod_buf> 0){
    resize_strided_buffer(Tracer_bufs->Tracer_ico_buf);
    num_patch = set_tracer_ico_to_buf(IZERO, IZERO, tracer_d->nnod_viz,
                                      tracer_d, tracer_m,
                                      Tracer_bufs->Tracer_ico_buf);
    };
	return;
}

