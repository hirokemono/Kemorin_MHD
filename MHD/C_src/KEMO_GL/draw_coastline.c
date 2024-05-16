
/* draw_coastline.c */


#include  "draw_coastline.h"

void set_coastline_line_buffer(int ncorner, int tube_radius,
                               struct mesh_menu_val *mesh_m,
                               struct gl_strided_buffer *coast_buf){
    long n_vertex = 0;
    if(mesh_m->iflag_draw_sph_grid > 0){
        n_vertex = n_vertex + (count_sph_med_flame() + count_sph_long_flame());
    }
    if(mesh_m->iflag_draw_coast > 0){
        n_vertex = n_vertex + get_nedge_coastline();
    };
    
    if(mesh_m->iflag_draw_coast_tube != 0){
        n_vertex = ITHREE * (ITWO*ncorner) * n_vertex;
    }else{
        n_vertex = ITWO * n_vertex;
    };
    set_buffer_address_4_patch(n_vertex, coast_buf);
    if(coast_buf->num_nod_buf == 0) return;
    resize_strided_buffer(coast_buf);

    long n_tube = 0;
    if(mesh_m->iflag_draw_coast_tube == 0){
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
    }else{
        if(mesh_m->iflag_draw_sph_grid > 0){
            n_tube = set_sph_med_flame_tube_to_buf(n_tube, IZERO, count_sph_med_flame(),
                                                   count_sph_frame_line(),
                                                   ncorner, tube_radius,
                                                   mesh_m->radius_coast, coast_buf);
            n_tube = set_sph_long_flame_tube_to_buf(n_tube, IZERO, count_sph_long_flame(),
                                                    count_sph_frame_line(),
                                                    ncorner, tube_radius,
                                                    mesh_m->radius_coast, coast_buf);
        };
        if(mesh_m->iflag_draw_coast > 0){
            n_tube = set_coastline_tube_buf(n_tube, IZERO, get_nedge_coastline(),
                                            ncorner, tube_radius,
                                            mesh_m->radius_coast, coast_buf);
        };
     };
return;
};

void set_map_coastline_line_buffer(int ncorner, int tube_radius,
                                   struct mesh_menu_val *mesh_m,
                                   struct gl_strided_buffer *coast_buf){
    long n_vertex = 0;
    if(mesh_m->iflag_draw_sph_grid != 0){
        n_vertex = n_vertex + count_sph_med_flame() + count_sph_long_flame();
    }
    if(mesh_m->iflag_draw_coast != 0){
        n_vertex = n_vertex + get_nedge_coastline();
    }
    
    if(mesh_m->iflag_draw_coast_tube != 0){
        n_vertex = ITHREE * (ITWO*ncorner) * n_vertex;
    }else{
        n_vertex = ITWO * n_vertex;
    };
    set_buffer_address_4_patch(n_vertex, coast_buf);
    if(coast_buf->num_nod_buf == 0) return;
    resize_strided_buffer(coast_buf);

    
    long n_tube = 0;
    if(mesh_m->iflag_draw_coast_tube == 0){
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
    }else{
        if(mesh_m->iflag_draw_sph_grid != 0){
            n_tube = set_map_med_frame_tube_to_buf(n_tube, IZERO, count_sph_med_flame(),
                                                   count_sph_frame_line(),
                                                   ncorner, tube_radius, coast_buf);
            n_tube = set_map_long_frame_tube_to_buf(n_tube, IZERO, count_sph_long_flame(),
                                                    count_sph_frame_line(),
                                                    ncorner, tube_radius, coast_buf);
        };
        if(mesh_m->iflag_draw_coast != 0){
            n_tube = set_map_coastline_tube_buf(n_tube, IZERO, get_nedge_coastline(),
                                                ncorner, tube_radius, coast_buf);
        };
    };
	return;
};
