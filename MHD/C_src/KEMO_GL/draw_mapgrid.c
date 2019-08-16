
/* draw_mapgrid.c */


#include  "draw_mapgrid.h"


#define NUM_T   6
#define NUM_P   6
#define N_CURVE 180

double theta_p_grid[N_CURVE+1];
double phi_p_grid[NUM_P+1];
double theta_t_grid[NUM_T+1];
double phi_t_grid[N_CURVE+1];

void init_mapgrid_position(){
	int i, j;
	double pi;
	
	pi = TWO * acos(ZERO);
	
	for(i=0; i<N_CURVE+1; i++){theta_p_grid[i] = (double)i * pi / (double)N_CURVE;}
	for(j=0; j<NUM_P+1; j++) {phi_p_grid[j] = (double)j * TWO * pi / (double)NUM_P;}
	
	for(j=0; j<NUM_T-1; j++){theta_t_grid[j] = (double)(j+1) * pi / (double)NUM_T;}
	for(i=0; i<N_CURVE+1; i++){phi_t_grid[i] = (double)i * TWO * pi / (double)N_CURVE;}
	return;
}

static int count_sph_flame(){
	int num_edge = (NUM_P+1) * N_CURVE + (NUM_T-1) * N_CURVE;
	return num_edge;
}

static void set_sph_flame_to_buf(double radius, struct gl_strided_buffer *strided_buf){
	int i, j, k, nd, inum, ierr;
	double f_color[4];
	double t_mid, p_mid;
	
	set_black_color_c(f_color);
	
	inum = 0;
	for(j=0; j<NUM_P+1; j++){
		for(i=0; i<N_CURVE; i++){
			t_mid = 0.5 * (theta_p_grid[i] + theta_p_grid[i+1]);
			set_node_stride_VBO((ITWO*inum  ), strided_buf);
			strided_buf->x_draw[0] = (GLfloat) (radius * sin(theta_p_grid[i]) * cos(phi_p_grid[j]));
			strided_buf->x_draw[1] = (GLfloat) (radius * sin(theta_p_grid[i]) * sin(phi_p_grid[j]));
			strided_buf->x_draw[2] = (GLfloat) (radius * cos(theta_p_grid[i]));
			strided_buf->n_draw[0] = (GLfloat) (1.0 *    sin(theta_p_grid[i]) * cos(phi_p_grid[j]));
			strided_buf->n_draw[1] = (GLfloat) (1.0 *    sin(theta_p_grid[i]) * sin(phi_p_grid[j]));
			strided_buf->n_draw[2] = (GLfloat) (1.0 *    cos(theta_p_grid[i]));
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			
			set_node_stride_VBO((ITWO*inum+1), strided_buf);
			strided_buf->x_draw[0] = (GLfloat) (radius * sin(t_mid) * cos(phi_p_grid[j]));
			strided_buf->x_draw[1] = (GLfloat) (radius * sin(t_mid) * sin(phi_p_grid[j]));
			strided_buf->x_draw[2] = (GLfloat) (radius * cos(t_mid));
			strided_buf->n_draw[0] = (GLfloat) (1.0 *    sin(t_mid) * cos(phi_p_grid[j]));
			strided_buf->n_draw[1] = (GLfloat) (1.0 *    sin(t_mid) * sin(phi_p_grid[j]));
			strided_buf->n_draw[2] = (GLfloat) (1.0 *    cos(t_mid));
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			inum = inum + 1;
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		for(i=0; i<N_CURVE; i++){
			p_mid = 0.5 * (phi_t_grid[i] + phi_t_grid[i+1]);
			set_node_stride_VBO((ITWO*inum  ), strided_buf);
			strided_buf->x_draw[0] = (GLfloat) (radius * sin(theta_t_grid[j]) * cos(phi_t_grid[i]));
			strided_buf->x_draw[1] = (GLfloat) (radius * sin(theta_t_grid[j]) * sin(phi_t_grid[i]));
			strided_buf->x_draw[2] = (GLfloat) (radius * cos(theta_t_grid[j]));
			strided_buf->n_draw[0] = (GLfloat) (1.0 *    sin(theta_t_grid[j]) * cos(phi_t_grid[i]));
			strided_buf->n_draw[1] = (GLfloat) (1.0 *    sin(theta_t_grid[j]) * sin(phi_t_grid[i]));
			strided_buf->n_draw[2] = (GLfloat) (1.0 *    cos(theta_t_grid[j]));
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			
			set_node_stride_VBO((ITWO*inum+1), strided_buf);
			strided_buf->x_draw[0] = (GLfloat) (radius * sin(theta_t_grid[j]) * cos(p_mid));
			strided_buf->x_draw[1] = (GLfloat) (radius * sin(theta_t_grid[j]) * sin(p_mid));
			strided_buf->x_draw[2] = (GLfloat) (radius * cos(theta_t_grid[j]));
			strided_buf->n_draw[0] = (GLfloat) (1.0 *    sin(theta_t_grid[j]) * cos(p_mid));
			strided_buf->n_draw[1] = (GLfloat) (1.0 *    sin(theta_t_grid[j]) * sin(p_mid));
			strided_buf->n_draw[2] = (GLfloat) (1.0 *    cos(theta_t_grid[j]));
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			inum = inum + 1;
		}
	}
	return;
}

static void set_map_flame_to_buf(struct gl_strided_buffer *strided_buf){
	int i, j, nd, inum, ierr;
	double rtp_flame[6], d_map_flame[4], f_color[4];
	
	set_black_color_c(f_color);
	
	inum = 0;
	rtp_flame[0] = ONE;
	rtp_flame[3] = ONE;
	for(j=0; j<NUM_P+1; j++){
		rtp_flame[2] = phi_p_grid[j];
		rtp_flame[5] = phi_p_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[1] = theta_p_grid[i];
			rtp_flame[4] = theta_p_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			
			set_node_stride_VBO((ITWO*inum  ), strided_buf);
			strided_buf->x_draw[0] = d_map_flame[0];
			strided_buf->x_draw[1] = d_map_flame[1];
			strided_buf->x_draw[2] = 0.002;
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			
			set_node_stride_VBO((ITWO*inum+1), strided_buf);
			strided_buf->x_draw[0] = 0.5 * (d_map_flame[0] + d_map_flame[2]);
			strided_buf->x_draw[1] = 0.5 * (d_map_flame[1] + d_map_flame[3]);
			strided_buf->x_draw[2] = 0.002;
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			inum = inum + 1;
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		rtp_flame[1] = theta_t_grid[j] ;
		rtp_flame[4] = theta_t_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[2] = phi_t_grid[i];
			rtp_flame[5] = phi_t_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			
			set_node_stride_VBO((ITWO*inum  ), strided_buf);
			strided_buf->x_draw[0] = d_map_flame[0];
			strided_buf->x_draw[1] = d_map_flame[1];
			strided_buf->x_draw[2] = 0.002;
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			
			set_node_stride_VBO((ITWO*inum+1), strided_buf);
			strided_buf->x_draw[0] = 0.5 * (d_map_flame[0] + d_map_flame[2]);
			strided_buf->x_draw[1] = 0.5 * (d_map_flame[1] + d_map_flame[3]);
			strided_buf->x_draw[2] = 0.002;
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];}
			inum = inum + 1;
		}
	}
	return;
}


void draw_sph_flame_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int nedge_flame = count_sph_flame();
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	
	int id_numLight = glGetUniformLocation(kemo_shaders->phong->programId, "num_lights");
	int id_lightPosition = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[0].position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
	
	int num_light = 1;
	GLfloat  lightposition[4] = {5.0, 5.0, -5.0,1.0};
	GLfloat white1[4] = {0.3, 0.3, 0.3, 1.0};
	GLfloat white2[4] = {0.8, 0.8, 0.8, 1.0};
	GLfloat white3[4] = {1.0, 1.0, 1.0, 1.0};
	GLfloat shine = 20.0;
	
	glUniform1i(id_numLight, num_light);
	glUniform4fv(id_lightPosition, 1, lightposition);
	
	glUniform4fv(id_MaterialAmbient, 1, white2);
	glUniform4fv(id_MaterialDiffuse, 1, white1);
	glUniform4fv(id_MaterialSpecular, 1, white3);
	glUniform1f(id_MaterialShiness, shine);
	
	set_buffer_address_4_patch(ITWO*nedge_flame, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	set_sph_flame_to_buf(radius, line_buf);
	
	glGenVertexArrays(1, &line_VAO->id_VAO);
	glBindVertexArray(line_VAO->id_VAO);
	
	glGenBuffers(1, &line_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * line_buf->num_nod_buf*line_buf->ncomp_buf,
				 line_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, line_buf->istride,
						  (GLvoid*) (line_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, line_buf->istride, 
						  (GLvoid*) (line_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, line_buf->istride, 
						  (GLvoid*) (line_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(line_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_flame));
	
	DestroyVBO(line_VAO);
	
	return;
};

void draw_map_flame_VBO(const GLdouble *orthogonal, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int nedge_flame = count_sph_flame();
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	set_buffer_address_4_patch(ITWO*nedge_flame, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	set_map_flame_to_buf(line_buf);
	
	glGenVertexArrays(1, &line_VAO->id_VAO);
	glBindVertexArray(line_VAO->id_VAO);
	
	glGenBuffers(1, &line_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * line_buf->num_nod_buf*line_buf->ncomp_buf,
				 line_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, line_buf->istride,
						  (GLvoid*) (line_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, line_buf->istride, 
						  (GLvoid*) (line_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	glBindVertexArray(line_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_flame));
	
	DestroyVBO(line_VAO);
	
	return;
};
