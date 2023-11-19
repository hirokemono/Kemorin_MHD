
/* draw_coastline.c */


#include  "draw_coastline.h"

void set_sph_flame_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mflame_buf){
    if(mesh_m->iflag_draw_sph_grid > 0){
        mflame_buf->num_nod_buf = ITWO * count_sph_flame();
        set_sph_flame_to_buf(mesh_m->radius_coast, mflame_buf);
    }else{
        mflame_buf->num_nod_buf = 0;
    };
	return;
};

void set_coastline_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *coast_buf){
    if(mesh_m->iflag_draw_coast > 0){
        coast_buf->num_nod_buf = ITWO * count_coastline_buf();
        set_coastline_buf(mesh_m->radius_coast, coast_buf);
    }else{
        coast_buf->num_nod_buf = 0;
    };
	return;
};

void set_map_flame_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mflame_buf){
    if(mesh_m->iflag_draw_sph_grid != 0){
        mflame_buf->num_nod_buf = ITWO * count_sph_flame();
        set_map_flame_to_buf(mflame_buf);
    } else {
        mflame_buf->num_nod_buf = 0;
    };
    return;
};

void set_map_coastline_buffer(struct mesh_menu_val *mesh_m,
                              struct gl_strided_buffer *coast_buf){
    if(mesh_m->iflag_draw_coast != 0){
        coast_buf->num_nod_buf = ITWO * count_coastline_buf();
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

void set_coastline_grid_VBO(struct gl_strided_buffer *coast_buf,
                            struct gl_strided_buffer *grid_buf,
                            struct VAO_ids **grid_VAO){
    grid_VAO[0]->npoint_draw = coast_buf->num_nod_buf;
    if(grid_VAO[0]->npoint_draw > 0){Const_VAO_4_Simple(grid_VAO[0], coast_buf);};
	
    grid_VAO[1]->npoint_draw = grid_buf->num_nod_buf;
	if(grid_VAO[1]->npoint_draw > 0){Const_VAO_4_Simple(grid_VAO[1], grid_buf);};
	return;
};

void map_coastline_grid_VBO(struct gl_strided_buffer *coast_buf,
                            struct gl_strided_buffer *mflame_buf,
                            struct VAO_ids **grid_VAO){
    grid_VAO[0]->npoint_draw = coast_buf->num_nod_buf;
    if(grid_VAO[0]->npoint_draw > 0){
        Const_VAO_4_Simple(grid_VAO[0], coast_buf);
    };
    
    grid_VAO[1]->npoint_draw = mflame_buf->num_nod_buf;
    if(grid_VAO[1]->npoint_draw > 0){
        Const_VAO_4_Simple(grid_VAO[1], mflame_buf);
	};
	return;
};
