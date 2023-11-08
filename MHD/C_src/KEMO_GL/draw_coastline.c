
/* draw_coastline.c */


#include  "draw_coastline.h"

static void set_sph_flame_VBO(double radius, struct VAO_ids *line_VAO,
                              struct gl_strided_buffer *line_buf){
	int n_point = ITWO * count_sph_flame();
	
	set_buffer_address_4_patch(n_point, line_buf);
	resize_strided_buffer(line_buf);
	set_sph_flame_to_buf(radius, line_buf);
	
    line_VAO->npoint_draw = line_buf->num_nod_buf;
	Const_VAO_4_Simple(line_VAO, line_buf);
	return;
};

static void set_coastline_VBO(double radius, struct VAO_ids *line_VAO,
                              struct gl_strided_buffer *line_buf){
	int icou;
    int n_point = ITWO * count_coastline_buf();
	
	set_buffer_address_4_patch(n_point, line_buf);
	resize_strided_buffer(line_buf);
	icou = set_coastline_buf(radius, line_buf);
	
    line_VAO->npoint_draw = line_buf->num_nod_buf;
	Const_VAO_4_Simple(line_VAO, line_buf);
	return;
};


void set_map_flame_buffer(int iflag_draw_sph_grid,
                          struct gl_strided_buffer *mflame_buf){
    if(iflag_draw_sph_grid != 0){
        int n_point = ITWO * count_sph_flame();
        set_buffer_address_4_patch(n_point, mflame_buf);
        alloc_strided_buffer(mflame_buf);
        set_map_flame_to_buf(mflame_buf);
    } else {
        mflame_buf->num_nod_buf = 0;
    };
    return;
};

void set_map_coastline_buffer(int iflag_draw_coast,
                              struct gl_strided_buffer *coast_buf){
    if(iflag_draw_coast != 0){
        int n_points = ITWO * count_coastline_buf();
        set_buffer_address_4_patch(n_points, coast_buf);
        alloc_strided_buffer(coast_buf);
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
	struct gl_strided_buffer *line_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, line_buf);
	alloc_strided_buffer(line_buf);
	
	if(mesh_m->iflag_draw_coast != 0){
		set_coastline_VBO(mesh_m->radius_coast, grid_VAO[0], line_buf);
	} else {
		grid_VAO[0]->npoint_draw = 0;
	};
	
	if(mesh_m->iflag_draw_sph_grid != 0){
		set_sph_flame_VBO(mesh_m->radius_coast, grid_VAO[1], line_buf);
	} else {
		grid_VAO[1]->npoint_draw = 0;
	};
	free(line_buf->v_buf);
	free(line_buf);
	return;
};

void map_coastline_grid_VBO(struct mesh_menu_val *mesh_m, struct VAO_ids **grid_VAO){
    struct gl_strided_buffer *coast_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    set_map_coastline_buffer(mesh_m->iflag_draw_coast, coast_buf);

    struct gl_strided_buffer *mflame_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_map_flame_buffer(mesh_m->iflag_draw_sph_grid, mflame_buf);
    
    
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
    free(coast_buf);
    free(mflame_buf);
	return;
};
