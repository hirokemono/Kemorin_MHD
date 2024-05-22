
/* draw_coastline.c */


#include  "draw_coastline.h"

long count_coastline_line_buffer(struct mesh_menu_val *mesh_m){
    long n_vertex = 0;
    if(mesh_m->iflag_draw_sph_grid > 0){
        n_vertex = n_vertex + (count_sph_med_flame() + count_sph_long_flame());
    }
    if(mesh_m->iflag_draw_coast > 0){
        n_vertex = n_vertex + get_nedge_coastline();
    };
    return n_vertex;
}

void set_coastline_line_buffer(struct mesh_menu_val *mesh_m,
                               struct gl_strided_buffer *coast_buf){
    long n_vertex = ITWO * count_coastline_line_buffer(mesh_m);
    set_buffer_address_4_patch(n_vertex, coast_buf);
    if(coast_buf->num_nod_buf == 0) return;
    resize_strided_buffer(coast_buf);

    long n_tube = 0;
    if(mesh_m->iflag_draw_sph_grid > 0){
        n_tube = set_sph_med_flame_line_to_buf(n_tube, IZERO, count_sph_med_flame(),
                                               count_sph_frame_line(),
                                               mesh_m->radius_coast, coast_buf);
        n_tube = set_sph_long_flame_line_to_buf(n_tube, IZERO, count_sph_long_flame(),
                                                count_sph_frame_line(),
                                                mesh_m->radius_coast, coast_buf);
    };
    if(mesh_m->iflag_draw_coast > 0){
        n_tube = set_coastline_line_buf(n_tube, IZERO, get_nedge_coastline(),
                                        mesh_m->radius_coast, coast_buf);
    };
    return;
};

void set_coastline_tube_buffer(struct mesh_menu_val *mesh_m, struct view_element *view_s,
                               struct gl_strided_buffer *coast_buf){
    long n_vertex = ITHREE * (ITWO*view_s->ncorner_tube) * count_coastline_line_buffer(mesh_m);
    set_buffer_address_4_patch(n_vertex, coast_buf);
    if(coast_buf->num_nod_buf == 0) return;
    resize_strided_buffer(coast_buf);

    double tube_radius;
    if(view_s->width_tube <= 0.0){
        tube_radius = set_tube_radius_by_axis(view_s);
    }else{
        tube_radius = view_s->width_tube;
    };
    
    long n_tube = 0;
    if(mesh_m->iflag_draw_sph_grid > 0){
        n_tube = set_sph_med_flame_tube_to_buf(n_tube, IZERO, count_sph_med_flame(),
                                               count_sph_frame_line(),
                                               view_s->ncorner_tube, tube_radius,
                                               mesh_m->radius_coast, coast_buf);
        n_tube = set_sph_long_flame_tube_to_buf(n_tube, IZERO, count_sph_long_flame(),
                                                count_sph_frame_line(),
                                                view_s->ncorner_tube, tube_radius,
                                                mesh_m->radius_coast, coast_buf);
    };
    if(mesh_m->iflag_draw_coast > 0){
        n_tube = set_coastline_tube_buf(n_tube, IZERO, get_nedge_coastline(),
                                        view_s->ncorner_tube, tube_radius,
                                        mesh_m->radius_coast, coast_buf);
     };
    return;
};

void set_map_coastline_line_buffer(struct mesh_menu_val *mesh_m,
                                   struct gl_strided_buffer *coast_buf){
    long n_vertex = count_coastline_line_buffer(mesh_m);
    if(mesh_m->iflag_draw_tangent_cyl > 0){
        n_vertex = n_vertex + ITWO * count_sph_long_flame();
    };
    n_vertex = ITWO * n_vertex;

    set_buffer_address_4_patch(n_vertex, coast_buf);
    if(coast_buf->num_nod_buf == 0) return;
    resize_strided_buffer(coast_buf);

    
    long n_tube = 0;
    if(mesh_m->iflag_draw_sph_grid != 0){
        n_tube = set_map_med_frame_line_to_buf(n_tube, IZERO, count_sph_med_flame(),
                                               count_sph_frame_line(), coast_buf);
        n_tube = set_long_map_flame_line_to_buf(n_tube, IZERO, count_sph_long_flame(),
                                                count_sph_frame_line(), coast_buf);
    };
    if(mesh_m->iflag_draw_coast != 0){
        n_tube = set_map_coastline_line_buf(n_tube, IZERO, get_nedge_coastline(),
                                            coast_buf);
    };
    if(mesh_m->iflag_draw_tangent_cyl != 0){
        n_tube = set_tangent_cylinder_line_to_buf(n_tube, IZERO,
                                                  (ITWO * count_sph_frame_line()),
                                                  count_sph_frame_line(),
                                                  mesh_m->radius_coast, mesh_m->r_ICB,
                                                  coast_buf);
    };
	return;
};

void set_map_coastline_tube_buffer(struct mesh_menu_val *mesh_m, struct view_element *view_s,
                                   struct gl_strided_buffer *coast_buf){
    long n_vertex =  count_coastline_line_buffer(mesh_m);
    if(mesh_m->iflag_draw_tangent_cyl > 0){
        n_vertex = n_vertex + ITWO * count_sph_long_flame();
    };
    n_vertex = ITHREE*(ITWO*view_s->ncorner_tube) * n_vertex;
    
    set_buffer_address_4_patch(n_vertex, coast_buf);
    if(coast_buf->num_nod_buf == 0) return;
    resize_strided_buffer(coast_buf);

    double tube_radius;
    if(view_s->width_tube <= 0.0){
        tube_radius = set_tube_radius_by_axis(view_s);
    }else{
        tube_radius = view_s->width_tube;
    };

    long n_tube = 0;
    if(mesh_m->iflag_draw_sph_grid != 0){
        n_tube = set_map_med_frame_tube_to_buf(n_tube, IZERO, count_sph_med_flame(),
                                               count_sph_frame_line(),
                                               view_s->ncorner_tube, tube_radius, coast_buf);
        n_tube = set_map_long_frame_tube_to_buf(n_tube, IZERO, count_sph_long_flame(),
                                                count_sph_frame_line(),
                                                view_s->ncorner_tube, tube_radius, coast_buf);
    };
    if(mesh_m->iflag_draw_coast != 0){
        n_tube = set_map_coastline_tube_buf(n_tube, IZERO, get_nedge_coastline(),
                                            view_s->ncorner_tube, tube_radius, coast_buf);
    };
    if(mesh_m->iflag_draw_tangent_cyl != 0){
        n_tube = set_tangent_cylinder_tube_to_buf(n_tube, IZERO,
                                                  (ITWO * count_sph_frame_line()),
                                                  count_sph_frame_line(),
                                                  view_s->ncorner_tube, (2.0*tube_radius),
                                                  mesh_m->radius_coast, mesh_m->r_ICB,
                                                  coast_buf);
    };
    return;
};
