
/* draw_coastline.c */


#include <OpenGL/gl3.h>
#include  "draw_coastline.h"

void set_sph_flame_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	line_VAO->npoint_draw = ITWO * count_sph_flame();
	
	set_buffer_address_4_patch(line_VAO->npoint_draw, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	set_sph_flame_to_buf(radius, line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	Const_VAO_4_Phong(line_VAO, line_buf);
	glBindVertexArray(0);
	
	return;
};

void set_map_flame_VBO(struct VAO_ids *line_VAO, 
			struct gl_strided_buffer *line_buf){
	line_VAO->npoint_draw = ITWO * count_sph_flame();
	
	set_buffer_address_4_patch(line_VAO->npoint_draw, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	set_map_flame_to_buf(line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	Const_VAO_4_Simple(line_VAO, line_buf);
	glBindVertexArray(0);
	return;
};


void set_coastline_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int icou;
	line_VAO->npoint_draw = ITWO * count_coastline_buf();
	
	set_buffer_address_4_patch(line_VAO->npoint_draw, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	icou = set_coastline_buf(radius, line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	Const_VAO_4_Phong(line_VAO, line_buf);
	glBindVertexArray(0);
	return;
};

void set_map_coastline_VBO(struct VAO_ids *line_VAO, 
			struct gl_strided_buffer *line_buf){
	int icou;
	line_VAO->npoint_draw = ITWO * count_coastline_buf();
	
	set_buffer_address_4_patch(line_VAO->npoint_draw, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	icou = set_map_coastline_buf(line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	Const_VAO_4_Simple(line_VAO, line_buf);
	glBindVertexArray(0);
	
	return;
};



void draw_axis_VAO(struct view_element *view_s, GLfloat dist, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *strided_buf){	
	int ncorner = ISIX;
	float radius = 3.0;
	
	int num_patch = count_axis_to_buf(ncorner);
	
	set_buffer_address_4_patch(ITHREE*num_patch, strided_buf);
	alloc_strided_buffer(strided_buf->num_nod_buf, strided_buf->ncomp_buf, strided_buf);
	
	set_axis_to_buf(view_s, dist, ncorner, radius, strided_buf);
	
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glGenVertexArrays(1, &mesh_VAO->id_VAO);
	glBindVertexArray(mesh_VAO->id_VAO);
	Const_VAO_4_Phong(mesh_VAO, strided_buf);
	glBindVertexArray(0);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(mesh_VAO);
	
	free(strided_buf->v_buf);
	
	return;
}



void draw_coastline_VBO(struct mesh_menu_val *mesh_m, struct view_element *view_s, 
			struct VAO_ids **grid_VAO, struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *line_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, line_buf);
	alloc_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	
	if(mesh_m->iflag_draw_coast != 0){
		glGenVertexArrays(1, &grid_VAO[0]->id_VAO);
		set_coastline_VBO(mesh_m->radius_coast, view_s, grid_VAO[0], kemo_shaders, line_buf);
	} else {
		grid_VAO[0]->npoint_draw = 0;
	};
	
	if(mesh_m->iflag_draw_sph_grid != 0){
		glGenVertexArrays(1, &grid_VAO[1]->id_VAO);
		set_sph_flame_VBO(mesh_m->radius_coast, view_s, grid_VAO[1], kemo_shaders, line_buf);
	} else {
		grid_VAO[1]->npoint_draw = 0;
	};
	
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	if(grid_VAO[0]->npoint_draw > 0){
		glBindVertexArray(grid_VAO[0]->id_VAO);
		glDrawArrays(GL_LINES, IZERO, grid_VAO[0]->npoint_draw);
	};
	
	if(grid_VAO[1]->npoint_draw > 0){
		glBindVertexArray(grid_VAO[1]->id_VAO);
		glDrawArrays(GL_LINES, IZERO, grid_VAO[1]->npoint_draw);
	};
	free(line_buf->v_buf);
	free(line_buf);
	
//	Destroy_Phong_VAO(grid_VAO[0]);
//	Destroy_Phong_VAO(grid_VAO[1]);
	return;
};

