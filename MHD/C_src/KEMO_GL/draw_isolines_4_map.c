/*
 *  draw_isolines_4_map.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/16.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include <OpenGL/gl3.h>
#include "draw_isolines_4_map.h"

static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};

static int count_map_isoline_to_buf(double width, double v_line, int icomp,
									 struct psf_data *psf_s){
	int num_quad;
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	double xyz_map[9];
	
	int idraw;
	int inod, iele, k;
	
	num_quad = 0;
	for(iele = 0; iele < psf_s->nele_viz; iele++) {
		for(k = 0; k < 3; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		projection_patch_4_map(xx_tri, xyz_map);
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xyz_map, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){num_quad = num_quad + 1;};
	};
	return num_quad;
};

static int set_map_isoline_to_buf(int ist, double width, double v_line, int icomp,
									 double *f_color, struct psf_data *psf_s,
									 struct gl_strided_buffer *strided_buf){
	int inum;
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	double xyz_map[9];
	
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
		
		projection_patch_4_map(xx_tri, xyz_map);
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xyz_map, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(k1=0;k1<6;k1++){
				set_node_stride_VBO((6*inum+k1), strided_buf);
				strided_buf->x_draw[0] =  x_ribbon[3*k1  ];
				strided_buf->x_draw[1] =  x_ribbon[3*k1+1];
				strided_buf->x_draw[2] =  0.0001;
				for(nd=0;nd<4;nd++) strided_buf->c_draw[nd] = f_color[nd];
			};
			inum = inum + 1;
		};
	};
	return inum;
};

static void draw_map_zeroline_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int inum = 0;
	int num_patch = 2 * count_map_isoline_to_buf(0.005, ZERO, psf_m->icomp_draw_psf, psf_s);
	if(num_patch <= 0) return;
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
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
	
	inum = set_map_isoline_to_buf(IZERO, 0.005, ZERO, psf_m->icomp_draw_psf, black,
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

static void draw_map_isolines_VAO(int ist, int ied, struct psf_data *psf_s,
			struct psf_menu_val *psf_m, const GLdouble *orthogonal, 
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
	
	for (j = (ist-1); j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		num_patch = num_patch 
				+ 2 * count_map_isoline_to_buf(0.002, v_line, psf_m->icomp_draw_psf, psf_s);
	};
	if(num_patch <= 0) return;
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
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
	
	
	for (j = (ist-1); j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		
		if (psf_m->isoline_color == RAINBOW_LINE){	
			set_rainbow_color_code(psf_m->cmap_psf, v_line, f_color);
		};
		inum = set_map_isoline_to_buf(inum, 0.002, v_line, psf_m->icomp_draw_psf, f_color, 
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

void draw_map_PSF_isolines_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			int iflag_retina, const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	if(psf_m->draw_psf_grid  != 0){
        find_start_positive_lines(psf_m);
        if(psf_m->ist_positive_line > 1){
            draw_map_isolines_VAO(IZERO, psf_m->ist_positive_line,
                                psf_s, psf_m, orthogonal, psf_VAO, kemo_shaders, psf_buf);
        };
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            draw_map_isolines_VAO(psf_m->ist_positive_line, psf_m->n_isoline, 
                                psf_s, psf_m, orthogonal, psf_VAO, kemo_shaders, psf_buf);
        };
    };
	if(psf_m->draw_psf_zero  != 0){
        draw_map_zeroline_VAO(psf_s, psf_m, orthogonal, psf_VAO, kemo_shaders, psf_buf);
    };
	return;
}

