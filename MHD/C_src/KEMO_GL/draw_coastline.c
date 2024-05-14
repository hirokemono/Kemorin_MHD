
/* draw_coastline.c */


#include  "draw_coastline.h"

void set_sph_flame_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mflame_buf){
    if(mesh_m->iflag_draw_sph_grid > 0){
        long n_vertex = ITWO * count_sph_flame();
        set_buffer_address_4_patch(n_vertex, mflame_buf);
        resize_strided_buffer(mflame_buf);
        set_sph_flame_to_buf(mesh_m->radius_coast,
                             mflame_buf);
    }else{
        mflame_buf->num_nod_buf = 0;
    };
	return;
};

void set_coastline_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *coast_buf){
    if(mesh_m->iflag_draw_coast > 0){
        long n_vertex = ITWO * count_coastline_buf();
        set_buffer_address_4_patch(n_vertex, coast_buf);
        resize_strided_buffer(coast_buf);
        set_coastline_buf(mesh_m->radius_coast, coast_buf);
    }else{
        coast_buf->num_nod_buf = 0;
    };
	return;
};

void set_map_flame_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mflame_buf){
    if(mesh_m->iflag_draw_sph_grid != 0){
        long n_vertex = ITWO * count_sph_flame();
        set_buffer_address_4_patch(n_vertex, mflame_buf);
        resize_strided_buffer(mflame_buf);
        set_map_flame_to_buf(mflame_buf);
    } else {
        mflame_buf->num_nod_buf = 0;
    };
    return;
};

void set_map_coastline_buffer(struct mesh_menu_val *mesh_m,
                              struct gl_strided_buffer *coast_buf){
    if(mesh_m->iflag_draw_coast != 0){
        long n_vertex = ITWO * count_coastline_buf();
        set_buffer_address_4_patch(n_vertex, coast_buf);
        resize_strided_buffer(coast_buf);
        set_map_coastline_buf(coast_buf);
    } else {
        coast_buf->num_nod_buf = 0;
    };
	return;
};
