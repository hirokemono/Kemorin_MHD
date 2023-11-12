
/* draw_coastline.c */


#include  "draw_coastline.h"

void set_sph_flame_buffer(int iflag_draw_sph_grid, double radius,
                          struct gl_strided_buffer *mflame_buf){
    if(iflag_draw_sph_grid > 0){
        set_sph_flame_to_buf(radius, mflame_buf);
    }else{
        mflame_buf->num_nod_buf = 0;
    }
	return;
};

void set_coastline_buffer(int iflag_draw_coast, double radius,
                          struct gl_strided_buffer *coast_buf){
    if(iflag_draw_coast > 0){
        set_coastline_buf(radius, coast_buf);
    }else{
        coast_buf->num_nod_buf = 0;
    }
	return;
};

void set_map_flame_buffer(int iflag_draw_sph_grid,
                          struct gl_strided_buffer *mflame_buf){
    if(iflag_draw_sph_grid != 0){
        set_map_flame_to_buf(mflame_buf);
    } else {
        mflame_buf->num_nod_buf = 0;
    };
    return;
};

void set_map_coastline_buffer(int iflag_draw_coast,
                              struct gl_strided_buffer *coast_buf){
    if(iflag_draw_coast != 0){
        set_map_coastline_buf(coast_buf);
    } else {
        coast_buf->num_nod_buf = 0;
    };
	return;
};



void set_axis_VAO(struct mesh_menu_val *mesh_m, struct view_element *view_s,
			struct VAO_ids *mesh_VAO){
	int ncorner = ISIX;
	int icou_patch = 0;
	double radius = 4.0;
	
	mesh_VAO->npoint_draw = 0;
	if(mesh_m->iflag_draw_axis == 0) return;
	
	struct gl_strided_buffer *axis_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	int n_point = ITHREE * count_axis_to_buf(ncorner);
	
	set_buffer_address_4_patch(n_point, axis_buf);
	alloc_strided_buffer(axis_buf);
	
	icou_patch = set_axis_to_buf(view_s, mesh_m->dist_domains, ncorner, radius, axis_buf);
	
    mesh_VAO->npoint_draw = axis_buf->num_nod_buf;
	Const_VAO_4_Phong(mesh_VAO, axis_buf);
	
	free(axis_buf->v_buf);
	free(axis_buf);
	return;
};

void set_coastline_grid_VBO(struct mesh_menu_val *mesh_m, struct VAO_ids **grid_VAO){
    int n_vertex = ITWO * count_coastline_buf();
	struct gl_strided_buffer *coast_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    set_buffer_address_4_patch(n_vertex, coast_buf);
    alloc_strided_buffer(coast_buf);
    set_coastline_buffer(mesh_m->iflag_draw_coast,
                         mesh_m->radius_coast, coast_buf);

    n_vertex = ITWO * count_sph_flame();
    struct gl_strided_buffer *mflame_buf
            = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    set_buffer_address_4_patch(n_vertex, mflame_buf);
    alloc_strided_buffer(mflame_buf);
    set_sph_flame_buffer(mesh_m->iflag_draw_sph_grid,
                         mesh_m->radius_coast, mflame_buf);

    grid_VAO[0]->npoint_draw = coast_buf->num_nod_buf;
    if(grid_VAO[0]->npoint_draw > 0){
        Const_VAO_4_Simple(grid_VAO[0], coast_buf);
	};
	
    grid_VAO[1]->npoint_draw = mflame_buf->num_nod_buf;
	if(grid_VAO[1]->npoint_draw > 0){
        Const_VAO_4_Simple(grid_VAO[1], mflame_buf);
	};
    free(mflame_buf->v_buf);
    free(mflame_buf);
	free(coast_buf->v_buf);
	free(coast_buf);
	return;
};

void map_coastline_grid_VBO(struct gl_strided_buffer *coast_buf,
                            struct gl_strided_buffer *mflame_buf,
                            struct VAO_ids **grid_VAO){
    grid_VAO[0]->npoint_draw = coast_buf->num_nod_buf;
    if(grid_VAO[0]->npoint_draw > 0){
        Const_VAO_4_Simple(grid_VAO[0], coast_buf);
        free(coast_buf->v_buf);
    };
    
    grid_VAO[1]->npoint_draw = mflame_buf->num_nod_buf;
    if(grid_VAO[1]->npoint_draw > 0){
        Const_VAO_4_Simple(grid_VAO[1], mflame_buf);
        free(mflame_buf->v_buf);
	};
	return;
};
