
/* draw_coastline.c */


#include  "draw_coastline.h"

int set_sph_flame_buffer(int iflag_draw_sph_grid, double radius, int ist_buf,
                         struct gl_strided_buffer *mflame_buf){
    int ied_buf = ist_buf;
    if(iflag_draw_sph_grid > 0){
        ied_buf = set_sph_flame_to_buf(radius, ist_buf, mflame_buf);
    };
	return ied_buf;
};

int set_coastline_buffer(int iflag_draw_coast, double radius, int ist_buf,
                         struct gl_strided_buffer *coast_buf){
    int ied_buf = ist_buf;
    if(iflag_draw_coast > 0){
        ied_buf = set_coastline_buf(radius, ist_buf, coast_buf);
    }
	return ied_buf;
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



void set_axis_VAO(struct gl_strided_buffer *axis_buf, struct VAO_ids *mesh_VAO){
	mesh_VAO->npoint_draw = axis_buf->num_nod_buf;
	if(mesh_VAO->npoint_draw > 0) Const_VAO_4_Phong(mesh_VAO, axis_buf);
	
	return;
};

void set_coastline_grid_VBO(struct mesh_menu_val *mesh_m, struct VAO_ids **grid_VAO){
    int n_vertex = ITWO * count_coastline_buf();
	struct gl_strided_buffer *coast_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    set_buffer_address_4_patch(n_vertex, coast_buf);
    alloc_strided_buffer(coast_buf);
    int ied_buf;
    ied_buf = set_coastline_buffer(mesh_m->iflag_draw_coast, mesh_m->radius_coast,
                                    IZERO, coast_buf);
    coast_buf->num_nod_buf = ied_buf;

    n_vertex = ITWO * count_sph_flame();
    struct gl_strided_buffer *mflame_buf
            = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    set_buffer_address_4_patch(n_vertex, mflame_buf);
    alloc_strided_buffer(mflame_buf);
    ied_buf = set_sph_flame_buffer(mesh_m->iflag_draw_sph_grid, mesh_m->radius_coast,
                                   IZERO, mflame_buf);

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
