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
    Tracer_bufs->Tracer_dot_buf =    init_strided_buffer(n_point);
    Tracer_bufs->Tracer_ico_buf =    init_strided_buffer(n_point);
    Tracer_bufs->Tracer_index_buf =  init_gl_index_buffer(12, 3);

    return Tracer_bufs;
}

void dealloc_Tracer_buffers(struct Tracer_buffers *Tracer_bufs)
{
    dealloc_strided_buffer(Tracer_bufs->Tracer_dot_buf);
    dealloc_strided_buffer(Tracer_bufs->Tracer_ico_buf);

    dealloc_gl_index_buffer(Tracer_bufs->Tracer_index_buf);
    free(Tracer_bufs);
};




long set_tracer_ico_buffer(const long ist_ico,
                           long ist_nod, long ied_nod,
                           struct psf_data *tracer_d,
                           struct psf_menu_val *tracer_m,
                           struct gl_strided_buffer *Tracer_ico_buf,
                           struct gl_index_buffer *Tracer_index_buf){
    long inum_ico = ist_nod;
    long inod;
    for(inod = ist_nod; inod < ied_nod; inod++){
        inum_ico = set_icosahedron_node_index_buffer(inum_ico,
                                                     tracer_m->viz_line_width,
                                                     &tracer_d->xyzw_viz[4*inod],
                                                     &tracer_d->color_nod[4*inod],
                                                     Tracer_ico_buf, Tracer_index_buf);
    };
    return inum_ico;
}

long set_tracer_cone_buffer(const long ist_ico,
                            long ist_nod, long ied_nod,
                            struct psf_data *tracer_d,
                            struct psf_menu_val *tracer_m,
                            struct gl_strided_buffer *Tracer_ico_buf,
                            struct gl_index_buffer *Tracer_index_buf){
    double xyzw_line[8], dir_line[8], color_line[8];
    long inum_ico = ist_nod;
    int nd;
    long inod;
    for(inod = ist_nod; inod < ied_nod; inod++){
        set_line_for_tracer_arrow((int) tracer_d->istack_comp[tracer_m->if_draw_viz],
                                  inod, tracer_d, tracer_m,
                                  xyzw_line, dir_line);
        for(nd=0;nd<4;nd++){
            color_line[  nd] = tracer_d->color_nod[4*inod+nd];
            color_line[4+nd] = tracer_d->color_nod[4*inod+nd];
        }
        color_line[3] = 1.0;
        color_line[7] = 1.0;
            
        inum_ico = set_cone_node_index_buffer(inum_ico,
                                              tracer_m->ncorner_viz_line,
                                              tracer_m->viz_line_width,
                                              xyzw_line, dir_line, color_line,
                                              Tracer_ico_buf, Tracer_index_buf);
    };
    return inum_ico;
}

void const_tracer_buffer(const int nthreads, struct view_element *view_s,
                         struct psf_data *tracer_d,
                         struct psf_menu_val *tracer_m,
                         struct Tracer_buffers *Tracer_bufs){
    Tracer_bufs->Tracer_index_buf->ntot_vertex = 0;
    Tracer_bufs->Tracer_ico_buf->num_nod_buf = 0;
    Tracer_bufs->Tracer_dot_buf->num_nod_buf = 0;
    if(tracer_m->iflag_draw_viz <= 0) return;
    
    long num_points = tracer_d->nnod_viz;
    set_buffer_address_4_patch(num_points, Tracer_bufs->Tracer_dot_buf);
    if(Tracer_bufs->Tracer_dot_buf->num_nod_buf>0){
        resize_strided_buffer(Tracer_bufs->Tracer_dot_buf);
        
        set_nodes_strided_buffer(IZERO, 
                                 tracer_d->nnod_viz, tracer_d->xyzw_viz,
                                 tracer_d->xyzw_viz, tracer_d->color_nod, 
                                 tracer_d->xyzw_viz, Tracer_bufs->Tracer_dot_buf);
    };
    
    double ref_width = 8.0;
    double tube_width;
    if(tracer_m->viz_line_width <= 0.0){
        tube_width = ref_width * set_tube_radius_by_axis(view_s);
    }else{
        tube_width = tracer_m->viz_line_width;
    };

    long num_patch;
    Tracer_bufs->Tracer_ico_buf->num_nod_buf = 0;
    if(tracer_m->draw_psf_vect > 0){
        num_patch = (2 + tracer_m->ncorner_viz_line) * tracer_d->nnod_viz;
        set_buffer_address_4_patch(num_patch, Tracer_bufs->Tracer_ico_buf);
    }else{
        num_patch = 12 * tracer_d->nnod_viz;
        set_buffer_address_4_patch(num_patch, Tracer_bufs->Tracer_ico_buf);
    };

    if(Tracer_bufs->Tracer_ico_buf->num_nod_buf <= 0) return;
    resize_strided_buffer(Tracer_bufs->Tracer_ico_buf);

    if(tracer_m->draw_psf_vect > 0){
        resize_gl_index_buffer(((2*tracer_m->ncorner_viz_line) * tracer_d->nnod_viz), ITHREE,
                               Tracer_bufs->Tracer_index_buf);
        num_patch = set_tracer_cone_buffer(IZERO, IZERO, tracer_d->nnod_viz,
                                           tracer_d, tracer_m,
                                           Tracer_bufs->Tracer_ico_buf,
                                           Tracer_bufs->Tracer_index_buf);
    }else{
        resize_gl_index_buffer((20 * tracer_d->nnod_viz), ITHREE,
                               Tracer_bufs->Tracer_index_buf);
        num_patch = set_tracer_ico_buffer(IZERO, IZERO, tracer_d->nnod_viz,
                                          tracer_d, tracer_m,
                                          Tracer_bufs->Tracer_ico_buf,
                                          Tracer_bufs->Tracer_index_buf);
    };
	return;
}

