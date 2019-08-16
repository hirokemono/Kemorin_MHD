
/* draw_axis_c.c */

#include "draw_axis_c.h"
#include <OpenGL/gl3.h>

static const GLfloat red[4] =   {RED_R, RED_G, RED_B, RED_A};
static const GLfloat blue[4] =  {BLUE_R, BLUE_G, BLUE_B, BLUE_A};
static const GLfloat green[4] = {L_GREEN_R, L_GREEN_G, L_GREEN_B, L_GREEN_A};

static struct view_element arrow_view;

static float set_ratio_4_axislabel(struct view_element *view_s, 
                                   int end_screen[2], int zero_screen[2], GLfloat l_axis){
	GLfloat ratio;
	GLfloat ratio_tmp[2];
	
	if ( end_screen[0] < view_s->nx_window/10 ) { 
		ratio_tmp[0] = (GLfloat) (view_s->nx_window/10 - zero_screen[0])
        / (GLfloat) (end_screen[0] - zero_screen[0]);
	} else if  ( end_screen[0] > view_s->nx_window*9/10 ) {
		ratio_tmp[0] = (GLfloat) (zero_screen[0]-view_s->nx_window*9/10)
        / (GLfloat) (zero_screen[0] - end_screen[0]);
	} else {
		ratio_tmp[0] = ONE;
	};
	
	if ( end_screen[1] < view_s->ny_window/10 ) { 
		ratio_tmp[1] = (GLfloat) (view_s->ny_window/10 - zero_screen[1])
        / (GLfloat) (end_screen[1] - zero_screen[1]);
	} else if  ( end_screen[1] > view_s->ny_window*9/10 ) {
		ratio_tmp[1] = (GLfloat) (zero_screen[1]-view_s->ny_window*9/10)
        / (GLfloat) (zero_screen[1] - end_screen[1]);
	} else {
		ratio_tmp[1] = ONE;
	};
	
	if (ratio_tmp[0] < ratio_tmp[1]) { ratio = ratio_tmp[0]; }
	else { ratio = ratio_tmp[1]; };
	
	ratio = ratio * l_axis;
	return ratio;
}

static void set_vertexs_for_axis(struct view_element *view_s, GLfloat dist,
								 GLfloat x_arrowx[6], GLfloat x_arrowy[6], GLfloat x_arrowz[6], GLfloat w_ratio[3],
								 GLfloat x_charax[12], GLfloat x_charay[18], GLfloat x_charaz[18], float *radius){
	GLfloat l_axis[3], axis_org[3];
	GLfloat label_ratio[3], min_l_ratio;
	
	GLfloat x_text_x1, x_text_x2, x_text_z1, x_text_z2;
	GLfloat y_text_y1, y_text_y2, y_text_z1, y_text_z2;
	GLfloat y_text_y3, y_text_z3;
	GLfloat z_text_z1, z_text_z2, z_text_x1, z_text_x2;
	
	GLfloat xx_axis[3];
	GLfloat x_label[2], y_label[2], z_label[2];
	
	int zero_screen[2], end_screen[2];
	
	set_axis_positions(view_s, (GLfloat) dist, l_axis, axis_org);
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(zero_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, view_s);
	
	xx_axis[0] = axis_org[0] + l_axis[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, view_s);
	label_ratio[0] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[0]);
	x_label[0] = end_screen[0] / label_ratio[0];
	x_label[1] = end_screen[1] / label_ratio[0];
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1] + l_axis[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, view_s);
	label_ratio[1] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[1]);
	y_label[0] = end_screen[0] / label_ratio[0];
	y_label[1] = end_screen[1] / label_ratio[0];
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2] + l_axis[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, view_s);
	label_ratio[2] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[2]);
	z_label[0] = end_screen[0] / label_ratio[0];
	z_label[1] = end_screen[1] / label_ratio[0];
	
	min_l_ratio = label_ratio[0];
	if(label_ratio[1] <= min_l_ratio) min_l_ratio = label_ratio[1];
	if(label_ratio[2] <= min_l_ratio) min_l_ratio = label_ratio[2];
	
	*radius = *radius * min_l_ratio / (float) view_s->ny_window;

	/*
	 printf("x_label %e, %e \n",x_label[0],x_label[1]);
	 printf("y_label %e, %e \n",y_label[0],y_label[1]);
	 printf("z_label %e, %e \n",z_label[0],z_label[1]);
	 */
	
	x_text_x1 = axis_org[0] + label_ratio[0] * ( 1.06 * (1.0 + dist));
	x_text_x2 = axis_org[0] + label_ratio[0];
	x_text_z1 = axis_org[2] - label_ratio[0] * ( 0.05 * (1.0 + dist));
	x_text_z2 = axis_org[2] - label_ratio[0] * ( 0.15 * (1.0 + dist));
	
	y_text_y1 = axis_org[1] + label_ratio[1];
	y_text_y2 = axis_org[1] + label_ratio[1] * ( 1.03 * (1.0 + dist));
	y_text_y3 = axis_org[1] + label_ratio[1] * ( 1.06 * (1.0 + dist));
	y_text_z1 = axis_org[2] - label_ratio[1] * ( 0.05 * (1.0 + dist));
	y_text_z2 = axis_org[2] - label_ratio[1] * ( 0.10 * (1.0 + dist));
	y_text_z3 = axis_org[2] - label_ratio[1] * ( 0.15 * (1.0 + dist));
	
	z_text_z1 = axis_org[2] + label_ratio[2] * ( 1.07 * (1.0 + dist));
	z_text_z2 = axis_org[2] + label_ratio[2];
	z_text_x1 = axis_org[0] - label_ratio[2] * ( 0.07 * (1.0 + dist));
	z_text_x2 = axis_org[0] - label_ratio[2] * ( 0.13 * (1.0 + dist));
	
    /* Generate vertex array*/
	x_arrowx[ 0] = axis_org[0]; x_arrowx[ 3] = axis_org[0] + label_ratio[0];
	x_arrowx[ 1] = axis_org[1]; x_arrowx[ 4] = axis_org[1];
	x_arrowx[ 2] = axis_org[2]; x_arrowx[ 5] = axis_org[2];
	
	x_arrowy[ 0] = axis_org[0]; x_arrowy[ 3] = axis_org[0];
	x_arrowy[ 1] = axis_org[1]; x_arrowy[ 4] = axis_org[1] + label_ratio[1];
	x_arrowy[ 2] = axis_org[2]; x_arrowy[ 5] = axis_org[2];
	
	x_arrowz[ 0] = axis_org[0]; x_arrowz[ 3] = axis_org[0];
	x_arrowz[ 1] = axis_org[1]; x_arrowz[ 4] = axis_org[1];
	x_arrowz[ 2] = axis_org[2]; x_arrowz[ 5] = axis_org[2] + label_ratio[2];
	
	w_ratio[0] = 0.004 * min_l_ratio / label_ratio[0];
	w_ratio[1] = 0.004 * min_l_ratio / label_ratio[1];
	w_ratio[2] = 0.004 * min_l_ratio / label_ratio[2];
	
	
	x_charax[ 0] = x_text_x1; x_charax[ 3] = x_text_x2;
	x_charax[ 6] = x_text_x2; x_charax[ 9] = x_text_x1;
	
	x_charax[ 1] = axis_org[1]; x_charax[ 4] = axis_org[1];
	x_charax[ 7] = axis_org[1]; x_charax[10] = axis_org[1];
	
	x_charax[ 2] = x_text_z1; x_charax[ 5] = x_text_z2;
	x_charax[ 8] = x_text_z1; x_charax[11] = x_text_z2;
	
	x_charay[ 0] = axis_org[0]; x_charay[ 3] = axis_org[0];
	x_charay[ 6] = axis_org[0]; x_charay[ 9] = axis_org[0];
	x_charay[12] = axis_org[0]; x_charay[15] = axis_org[0];
	
	x_charay[ 1] = y_text_y1; x_charay[ 4] = y_text_y2;
	x_charay[ 7] = y_text_y3; x_charay[10] = y_text_y2;
	x_charay[13] = y_text_y2; x_charay[16] = y_text_y2;
	
	x_charay[ 2] = y_text_z1; x_charay[ 5] = y_text_z2;
	x_charay[ 8] = y_text_z1; x_charay[11] = y_text_z2;
	x_charay[14] = y_text_z2; x_charay[17] = y_text_z3;
	
	
	x_charaz[ 0] = z_text_x1; x_charaz[ 3] = z_text_x2;
	x_charaz[ 6] = z_text_x2; x_charaz[ 9] = z_text_x1;
	x_charaz[12] = z_text_x1; x_charaz[15] = z_text_x2;
	
	x_charaz[ 1] = axis_org[1]; x_charaz[ 4] = axis_org[1];
	x_charaz[ 7] = axis_org[1]; x_charaz[10] = axis_org[1];
	x_charaz[13] = axis_org[1]; x_charaz[16] = axis_org[1];
	
	x_charaz[ 2] = z_text_z1; x_charaz[ 5] = z_text_z1;
	x_charaz[ 8] = z_text_z1; x_charaz[11] = z_text_z2;
	x_charaz[14] = z_text_z2; x_charaz[17] = z_text_z2;
	
	/*
	 printf("l_axis %e %e %e \n",l_axis[0],l_axis[1],l_axis[2]);
	 printf("label_ratio %e %e %e \n",label_ratio[0],label_ratio[1],label_ratio[2]);
	 printf("x_text_x1 %e %e %e %e \n",x_text_x1,x_text_x2,x_text_z1,x_text_z2);
	 printf("y_text_y1 %e %e %e %e %e %e \n",y_text_y1,y_text_y2,y_text_y3,
	 y_text_z1,y_text_z2,y_text_z3);
	 printf("z_text_z1 %e %e %e %e \n",z_text_z1,z_text_z2,z_text_x1,z_text_x2);
	 */
	/*  Draw */
	return;
}

static int count_axis_VBO(int ncorner){
	int npatch_wall = 0;
    /*draw x axis */
	npatch_wall = npatch_wall + 2*ncorner;
    /*draw y axis */
	npatch_wall = npatch_wall + 2*ncorner;
    /*draw z axis */
	npatch_wall = npatch_wall + 2*ncorner;
	
	
	/*draw 'X' */
	npatch_wall = npatch_wall + 4*ncorner;
	/*draw 'Y' */
	npatch_wall = npatch_wall + 6*ncorner;
	/*draw 'Z' */
	npatch_wall = npatch_wall + 6*ncorner;
    
	return npatch_wall;
}

static void set_axis_VBO(int ncorner, float radius, 
			GLfloat x_arrowx[6], GLfloat x_arrowy[6], GLfloat x_arrowz[6],
			GLfloat x_charax[12], GLfloat x_charay[18], GLfloat x_charaz[18],
			struct gl_strided_buffer *strided_buf){
	GLfloat dir_line[6];
	GLfloat color_2p[8];
	int npatch_wall;
    int icou = 0;
	int k, nd;
    /*draw x axis */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_arrowx[3+nd] - x_arrowx[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  red[nd];};
	}
	/*
	printf("x_arrowx1 %f %f %f \n", x_arrowx[0], x_arrowx[1], x_arrowx[2]);
	printf("x_arrowx2 %f %f %f \n", x_arrowx[3], x_arrowx[4], x_arrowx[5]);
	printf("dir_line1 %f %f %f \n", dir_line[0], dir_line[1], dir_line[2]);
	*/
	npatch_wall = set_tube_strided_buffer(ncorner, radius, x_arrowx, 
				dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	
    /*draw y axis */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_arrowy[3+nd] - x_arrowy[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, x_arrowy, 
				dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	
    /*draw z axis */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_arrowz[3+nd] - x_arrowz[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, x_arrowz,
				dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	
	
	/*draw 'X' */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charax[3+nd] - x_charax[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  red[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charax[0],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charax[9+nd] - x_charax[6+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  red[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charax[6],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	
	/*draw 'Y' */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charay[3+nd] - x_charay[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charay[0],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charay[9+nd] - x_charay[6+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charay[6],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charay[15+nd] - x_charay[12+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charay[12],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	
	/*draw 'Z' */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charaz[3+nd] - x_charaz[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charaz[0],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charaz[9+nd] - x_charaz[6+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charaz[6],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[3*k+nd] =  x_charaz[15+nd] - x_charaz[12+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	npatch_wall = set_tube_strided_buffer(ncorner, radius, &x_charaz[12],
					dir_line, color_2p, icou, strided_buf);
	icou = icou + 3 * npatch_wall;
    
	return;
}

void draw_axis_VAO(struct view_element *view_s, GLfloat dist, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *strided_buf){	
	int ncorner = ISIX;
	float radius = 3.0;
	
	GLfloat x_arrowx[6], x_arrowy[6], x_arrowz[6];
	GLfloat w_ratio[3];
	GLfloat x_charax[12], x_charay[18], x_charaz[18];
	
	int num_patch = count_axis_VBO(ncorner);
	
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
	
	set_buffer_address_4_patch(ITHREE*num_patch, strided_buf);
	alloc_strided_buffer(strided_buf->num_nod_buf, strided_buf->ncomp_buf, strided_buf);
	
	set_vertexs_for_axis(view_s, dist, x_arrowx, x_arrowy, x_arrowz, 
						 w_ratio, x_charax, x_charay, x_charaz, &radius);
	set_axis_VBO(ncorner, radius, 
				x_arrowx, x_arrowy, x_arrowz, x_charax, x_charay, x_charaz,
				strided_buf);
	
	
	glGenVertexArrays(1, &mesh_VAO->id_VAO);
	glBindVertexArray(mesh_VAO->id_VAO);
	
	glGenBuffers(1, &mesh_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, mesh_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, mesh_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(mesh_VAO);
	free(strided_buf->v_buf);
	
	return;
}

