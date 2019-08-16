
/* draw_fieldlines.c */

#include <OpenGL/gl3.h>
#include "draw_fieldlines.h"

/*
static const GLfloat black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
*/


void draw_fieldtubes_c(struct psf_data *fline_s, struct fline_menu_val *fline_m,
					   struct buffer_for_gl *gl_buf) {
	int num_wall, inum_buf;
	int inod, iele, i, k, nd;
	int ncorner = ISIX;
	float xyz[9*2*ncorner], nor[9*2*ncorner], col[12*2*ncorner];
	float x_line[6], dir_line[6], color_line[8];
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
	
	inum_buf = 0;
	for (iele = 0; iele < fline_s->nele_viz; iele++) {
		for (k = 0; k < 2; k++) {
			inod = fline_s->ie_viz[iele][k] - 1;
			for (nd=0; nd<3; nd++) {
				x_line[3*k+nd] = (float) fline_s->xx_viz[inod][nd];
				dir_line[3*k+nd] = (float) fline_s->dir_nod[inod][nd];
			};
			for (nd=0; nd<4; nd++) {color_line[4*k+nd] = (float) fline_s->color_nod[inod][nd];};
		};
		
		num_wall = set_tube_vertex(ncorner, fline_m->fieldline_thick, x_line, dir_line, color_line,
								   xyz, nor, col);
		
		for (i=0; i<3*num_wall; i++) {
			for(nd=0;nd<3;nd++){gl_buf->xyz[3*inum_buf+i][nd] =  xyz[3*i+nd];};
			for(nd=0;nd<3;nd++){gl_buf->norm[3*inum_buf+i][nd] = nor[3*i+nd];};
			for(nd=0;nd<4;nd++){gl_buf->rgba[3*inum_buf+i][nd] = col[4*i+nd];};
		};
		inum_buf = inum_buf + num_wall;

		if(inum_buf >= (NSIZE_GL_BUFFER - num_wall) ){
			glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));
			inum_buf = 0;
		};
 
	};
	if(inum_buf > 0){glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));};
	
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	return;
}

void draw_fieldlines_c(struct psf_data *fline_s, struct fline_menu_val *fline_m,
					   struct buffer_for_gl *gl_buf) {
	int inod, iele, k;
	int num, icou, inum;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	icou = 0;
	while(icou<fline_s->nele_viz){
		if(icou+NSIZE_GL_BUFFER <= fline_s->nele_viz) {num = NSIZE_GL_BUFFER;}
		else {num = fline_s->nele_viz-icou;};

		for(inum=0; inum<num; inum++){
			iele = inum + icou;
			for(k=0;k<ITWO;k++){
				inod =fline_s->ie_viz[iele][k] - 1;
				gl_buf->xyz[ITWO*inum+k][0] = fline_s->xx_viz[inod][0];
				gl_buf->xyz[ITWO*inum+k][1] = fline_s->xx_viz[inod][1];
				gl_buf->xyz[ITWO*inum+k][2] = fline_s->xx_viz[inod][2];
				/*
				 for(nd=0;nd=3;nd++){
				 gl_buf->norm[ITWO*inum+k][nd] = fline_s->norm_nod[inod][nd];
				 };
				 */
				gl_buf->rgba[ITWO*inum+k][0] = fline_s->color_nod[inod][0];
				gl_buf->rgba[ITWO*inum+k][1] = fline_s->color_nod[inod][1];
				gl_buf->rgba[ITWO*inum+k][2] = fline_s->color_nod[inod][2];
				gl_buf->rgba[ITWO*inum+k][3] = fline_s->color_nod[inod][3];
			};
		};
		glDrawArrays(GL_LINES, IZERO, (ITWO*num));
		icou = icou + NSIZE_GL_BUFFER;
	};
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	
	return;
}

static int count_fieldtubes_to_buf(int ncorner, struct psf_data *fline_s){
	int num_patch = 2 * fline_s->nele_viz * ncorner; 
	return num_patch;
};
static int count_fieldlines_to_buf(struct psf_data *fline_s){
	int num_edge = fline_s->nele_viz;
	return num_edge;
}

void set_fieldtubes_to_buf(int ncorner, struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct gl_strided_buffer *strided_buf) {
	int num_wall, inum_patch;
	int inod, iele, k, nd;
	float xyz[9*2*ncorner], nor[9*2*ncorner], col[12*2*ncorner];
	float x_line[6], dir_line[6], color_line[8];
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	inum_patch = 0;
	for (iele = 0; iele < fline_s->nele_viz; iele++) {
		for (k = 0; k < 2; k++) {
			inod = fline_s->ie_viz[iele][k] - 1;
			for (nd=0; nd<3; nd++) {
				x_line[3*k+nd] = (float) fline_s->xx_viz[inod][nd];
				dir_line[3*k+nd] = (float) fline_s->dir_nod[inod][nd];
			};
			for (nd=0; nd<4; nd++) {color_line[4*k+nd] = (float) fline_s->color_nod[inod][nd];};
		};
		
		num_wall = set_tube_vertex(ncorner, fline_m->fieldline_thick, x_line, dir_line, color_line,
								   xyz, nor, col);
		
		for (k=0; k<3*num_wall; k++) {
			set_node_stride_VBO((ITHREE*inum_patch+k), strided_buf);
			for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] =  xyz[3*k+nd];};
			for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = nor[3*k+nd];};
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = col[4*k+nd];};
		};
		inum_patch = inum_patch + num_wall; 
	};
	return;
};


static void set_fieldlines_to_buf(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct gl_strided_buffer *strided_buf) {
	int inod, iele, k, nd;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	for(iele=0; iele<fline_s->nele_viz; iele++){
		for(k=0;k<ITWO;k++){
			inod =fline_s->ie_viz[iele][k] - 1;
			set_node_stride_VBO((ITWO*iele+k), strided_buf);
			for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = fline_s->xx_viz[inod][nd];};
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = fline_s->color_nod[inod][nd];};
		};
	};
	
	return;
}


void draw_fieldtubes_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct view_element *view_s, 
			struct VAO_ids *fline_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *fline_buf){
	int ncorner = ISIX;
	
	int num_patch = count_fieldtubes_to_buf(ncorner, fline_s);
	if(num_patch <= 0) return;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	
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
	
	set_buffer_address_4_patch(ITHREE*num_patch, fline_buf);
	resize_strided_buffer(fline_buf->num_nod_buf, fline_buf->ncomp_buf, fline_buf);
	
	set_fieldtubes_to_buf(ncorner, fline_s, fline_m, fline_buf);
	
	glGenVertexArrays(1, &fline_VAO->id_VAO);
	glBindVertexArray(fline_VAO->id_VAO);
	
	glGenBuffers(1, &fline_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, fline_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * fline_buf->num_nod_buf*fline_buf->ncomp_buf,
				 fline_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, fline_buf->istride,
						  (GLvoid*) (fline_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, fline_buf->istride, 
						  (GLvoid*) (fline_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, fline_buf->istride, 
						  (GLvoid*) (fline_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(fline_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, fline_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(fline_VAO);
	
	return;
}

void draw_fieldlines_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct view_element *view_s, 
			struct VAO_ids *fline_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *fline_buf){
	int inod, iele, k;
	int num, icou, inum;
	
	int num_edge = count_fieldlines_to_buf(fline_s);
	if(num_edge <= 0) return;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	glUseProgram(kemo_shaders->test->programId);
	transfer_matrix_to_shader(kemo_shaders->test, view_s);
	
	set_buffer_address_4_patch(ITWO*num_edge, fline_buf);
	resize_strided_buffer(fline_buf->num_nod_buf, fline_buf->ncomp_buf, fline_buf);
	
	set_fieldlines_to_buf(fline_s, fline_m, fline_buf);
	
	
	glGenVertexArrays(1, &fline_VAO->id_VAO);
	glBindVertexArray(fline_VAO->id_VAO);
	
	glGenBuffers(1, &fline_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, fline_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * fline_buf->num_nod_buf*fline_buf->ncomp_buf,
				 fline_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, fline_buf->istride,
						  (GLvoid*) (fline_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, fline_buf->istride, 
						  (GLvoid*) (fline_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	glBindVertexArray(fline_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, fline_VAO->id_vertex);
	glDrawArrays(GL_LINES, IZERO, (ITWO*num_edge));
	
	DestroyVBO(fline_VAO);
	
	return;
}
