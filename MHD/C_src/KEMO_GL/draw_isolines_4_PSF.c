/*
// draw_isolines_4_PSF.c
*/

#include <OpenGL/gl3.h>
#include "draw_isolines_4_PSF.h"

static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};


double cal_isoline_value(int j, struct psf_menu_val *psf_m){
	double v_line, range_min, range_max;
	range_min = psf_m->cmap_psf->color_data[0];
	range_max = psf_m->cmap_psf->color_data[psf_m->cmap_psf->n_color_point-1];
	
	v_line = range_min + (range_max - range_min)
			* ((double) j) / ((double) psf_m->n_isoline-1);
	
	return v_line;
}

void find_start_positive_lines(struct psf_menu_val *psf_m){
    int j;
    double pre_value, current_value, range_min, range_max;
    range_min = psf_m->cmap_psf->color_data[0];
    range_max = psf_m->cmap_psf->color_data[psf_m->cmap_psf->n_color_point-1];
    
    if(range_min >= ZERO) psf_m->ist_positive_line = 1;
    else if(range_max <= ZERO){
        psf_m->ist_positive_line = psf_m->n_isoline + 1;
    } else {
        psf_m->ist_positive_line = 1;
        current_value = range_min;
        for (j = 1; j < psf_m->n_isoline; j++){
            pre_value = current_value;
            current_value = cal_isoline_value(j, psf_m);
            if( (current_value*pre_value) <= ZERO){
                psf_m->ist_positive_line = j + 1;
                return;
            };
        };
    };
    
    return;
}

static int draw_isoline_on_triangle(int ist, double width, double v_line, int icomp, double *f_color,
									 struct psf_data *psf_s, struct buffer_for_gl *gl_buf){
	
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	
	int inum;
	int idraw;
	int inod, iele, k, nd, k1;
	
	inum = ist;
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		/*  find isoline */
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xx_tri, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(k1=0;k1<6;k1++){
				for(nd=0;nd<3;nd++) gl_buf->xyz[6*inum+k1][nd] =  x_ribbon[3*k1+nd];
				for(nd=0;nd<4;nd++) gl_buf->rgba[6*inum+k1][nd] = f_color[nd];
			};
			inum = inum + 1;
		};
		
		if(2*inum>=NSIZE_GL_BUFFER){
			glDrawArrays(GL_TRIANGLES, IZERO, (6*inum));
			inum = 0;
		}
	};

	return inum;
};

static void draw_zeroline_4_psf(struct psf_data *psf_s, struct psf_menu_val *psf_m,
								struct buffer_for_gl *gl_buf){
	int inum = 0;
	
	inum = draw_isoline_on_triangle(inum, 0.005, ZERO, psf_m->icomp_draw_psf, black,
							 psf_s, gl_buf);
	if(inum > 0) glDrawArrays(GL_TRIANGLES, IZERO, (6*inum));
	return;
}

static void draw_isolines_4_psf(int ist, int ied, struct psf_data *psf_s,
                                struct psf_menu_val *psf_m,
								struct buffer_for_gl *gl_buf){
	int inum = 0;
	int j, nd;
	double v_line;
	double f_color[4];
	
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};

	for (j = (ist-1); j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);

		if (psf_m->isoline_color == RAINBOW_LINE){
			set_rainbow_color_code(psf_m->cmap_psf, v_line, f_color);
		};
		
		inum = draw_isoline_on_triangle(inum, 0.002, v_line, psf_m->icomp_draw_psf, f_color,
								 psf_s, gl_buf);
	};
	if(inum > 0) glDrawArrays(GL_TRIANGLES, IZERO, (6*inum));
	
	return;
}

void draw_PSF_isoline(struct psf_data *psf_s, struct psf_menu_val *psf_m,
					  struct buffer_for_gl *gl_buf, int iflag_retina){
    int ierr;
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	glLineWidth(HALF * ((float) iflag_retina+IONE));
	
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			draw_isolines_4_psf(IONE, psf_m->ist_positive_line,
						psf_s, psf_m, gl_buf);
		};
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            draw_isolines_4_psf(psf_m->ist_positive_line,
                                psf_m->n_isoline, psf_s, psf_m, gl_buf);
        };
    };
	if(psf_m->draw_psf_zero  != 0){
        draw_zeroline_4_psf(psf_s, psf_m, gl_buf);
    };
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	return;
}


static int count_isoline_to_buf(double width, double v_line, int icomp, struct psf_data *psf_s){
	
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	
	int idraw;
	int inod, iele, k;
	int num_quad = 0;
	
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		/*  find isoline */
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xx_tri, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){num_quad = num_quad + 1;};
	};
	
	return num_quad;
};

static int set_isoline_to_buf(int ist, double width, double v_line, int icomp, double *f_color,
									 struct psf_data *psf_s, struct gl_strided_buffer *strided_buf){
	
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	
	int inum;
	int idraw;
	int inod, iele, k, nd, k1;
	
	inum = ist;
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		/*  find isoline */
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xx_tri, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(k1=0;k1<6;k1++){
				set_node_stride_VBO((6*inum+k1), strided_buf);
				for(nd=0;nd<3;nd++) strided_buf->x_draw[nd] = x_ribbon[3*k1+nd];
				for(nd=0;nd<4;nd++) strided_buf->c_draw[nd] = f_color[nd];
			};
			inum = inum + 1;
		};
	};

	return inum;
};

static void draw_zeroline_VBO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int inum = 0;
	int num_patch = 2 * count_isoline_to_buf(0.005, ZERO, psf_m->icomp_draw_psf, psf_s);
	if(num_patch <= 0) return;
	
	glUseProgram(kemo_shaders->test->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	
	int id_numLight = glGetUniformLocation(kemo_shaders->test->programId, "num_lights");
	int id_lightPosition = glGetUniformLocation(kemo_shaders->test->programId, "LightSource[0].position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.shininess");
	
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
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum = set_isoline_to_buf(inum, 0.005, ZERO, psf_m->icomp_draw_psf, black,
							 psf_s, psf_buf);
	
	
	glGenVertexArrays(1, &psf_VAO->id_VAO);
	glBindVertexArray(psf_VAO->id_VAO);
	
	glGenBuffers(1, &psf_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * psf_buf->num_nod_buf*psf_buf->ncomp_buf,
				 psf_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, psf_buf->istride,
						  (GLvoid*) (psf_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(psf_VAO);
	return;
}

static void draw_isolines_VBO(int ist, int ied, struct psf_data *psf_s,
			struct psf_menu_val *psf_m, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = 0;
	int inum = 0;
	int j, nd;
	double v_line;
	double f_color[4];
	
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		num_patch = num_patch 
				+ 2 * count_isoline_to_buf(0.002, v_line, psf_m->icomp_draw_psf, psf_s);
	};
	if(num_patch <= 0) return;
	
	glUseProgram(kemo_shaders->test->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	
	int id_numLight = glGetUniformLocation(kemo_shaders->test->programId, "num_lights");
	int id_lightPosition = glGetUniformLocation(kemo_shaders->test->programId, "LightSource[0].position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->test->programId, "frontMaterial.shininess");
	
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
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		
		if (psf_m->isoline_color == RAINBOW_LINE){
			set_rainbow_color_code(psf_m->cmap_psf, v_line, f_color);
		};
		
		inum = set_isoline_to_buf(inum, 0.002, v_line, psf_m->icomp_draw_psf, f_color,
								 psf_s, psf_buf);
	};
	
	glGenVertexArrays(1, &psf_VAO->id_VAO);
	glBindVertexArray(psf_VAO->id_VAO);
	
	glGenBuffers(1, &psf_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * psf_buf->num_nod_buf*psf_buf->ncomp_buf,
				 psf_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, psf_buf->istride,
						  (GLvoid*) (psf_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(psf_VAO);
	
	return;
}

void draw_PSF_isoline_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
    int ierr;
	
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			draw_isolines_VBO(IZERO, psf_m->ist_positive_line,
						psf_s, psf_m, view_s, psf_VAO, kemo_shaders, psf_buf);
		};
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            draw_isolines_VBO(psf_m->ist_positive_line, psf_m->n_isoline,
						psf_s, psf_m, view_s, psf_VAO, kemo_shaders, psf_buf);
        };
    };
	if(psf_m->draw_psf_zero  != 0){
        draw_zeroline_VBO(psf_s, psf_m, view_s, psf_VAO, kemo_shaders, psf_buf);
    };
	
	return;
}
