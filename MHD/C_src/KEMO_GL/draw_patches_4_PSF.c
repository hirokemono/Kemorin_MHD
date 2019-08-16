
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

static GLfloat white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};


static void set_psf_nodes_to_map(int ist_psf, int num_buf, struct psf_data **psf_s, 
                                 struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, ipsf, k;
	double xx_tri[9], xyz_map[9];
	
    for(inum=0; inum<num_buf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
        iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s[ipsf]->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s[ipsf]->xx_viz[inod][2];
		};
		projection_patch_4_map(xx_tri, xyz_map);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			gl_buf->xy[ITHREE*inum+k][0] = xyz_map[ITHREE*k  ];
			gl_buf->xy[ITHREE*inum+k][1] = xyz_map[ITHREE*k+1];
		};
	};
	return;
}

static void set_psf_colors_to_map(int ist_psf, int num_buf, struct psf_data **psf_s,
                                  struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
    int inum, iele, inod, ipsf, k;
    
    for(inum=0; inum<num_buf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
        iele = psf_a->iele_viz_far[inum+ist_psf]-1;
        for (k = 0; k < ITHREE; k++) {
            inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
            gl_buf->rgba[ITHREE*inum+k][0] = psf_s[ipsf]->color_nod[inod][0];
            gl_buf->rgba[ITHREE*inum+k][1] = psf_s[ipsf]->color_nod[inod][1];
            gl_buf->rgba[ITHREE*inum+k][2] = psf_s[ipsf]->color_nod[inod][2];
            gl_buf->rgba[ITHREE*inum+k][3] = psf_s[ipsf]->color_nod[inod][3];
        };
    };
    return;
}



void draw_patches_4_map(int shading_mode, int ist_psf, int ied_psf,
                        struct psf_data **psf_s, struct kemo_array_control *psf_a,
						struct buffer_for_gl *gl_buf) {
	int icou, num;
		
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITWO, GL_FLOAT, IZERO, gl_buf->xy);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	icou = ist_psf;
	while(icou < ied_psf){
		if( (icou+NSIZE_GL_BUFFER) <= ied_psf) {num = NSIZE_GL_BUFFER;}
		else								   {num = ied_psf-icou;};
		
		set_psf_nodes_to_map(icou, num, psf_s, psf_a, gl_buf);
		set_psf_colors_to_map(icou, num, psf_s, psf_a, gl_buf);
		
		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num));
		icou = icou + NSIZE_GL_BUFFER;
	};

	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	return;	
}

static void set_texture(struct psf_menu_val *psf_m){
    static const GLfloat blend[] = {1.0,1.0,1.0,1.0};
	/* Preference for resiging texture */
	glBindTexture(GL_TEXTURE_2D , psf_m->texture_name[0]);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glTexImage2D(
		GL_TEXTURE_2D , 0 , GL_RGBA , psf_m->texture_width , psf_m->texture_height ,
		0 , GL_RGBA , GL_UNSIGNED_BYTE , psf_m->texture_rgba);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);	
	glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, blend);
	
	return;
};


static void set_psf_nodes_to_buf(int ist_psf, int num_buf, int shading_mode, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m, 
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
	int inum, iele, inod, k, nd, ipsf;
	
	for(inum=0; inum<num_buf; inum++){
		ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
		iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			
			set_node_stride_VBO((ITHREE*inum+k), strided_buf);
			for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = psf_s[ipsf]->xx_viz[inod][nd];};
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = psf_s[ipsf]->color_nod[inod][nd];};
			if (shading_mode == SMOOTH_SHADE){
                for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = psf_s[ipsf]->norm_nod[inod][nd];};
			} else {
				for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = psf_s[ipsf]->norm_ele[iele][nd];};
			};
			if(psf_m[ipsf]->polygon_mode_psf == REVERSE_POLYGON){
				for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = -strided_buf->n_draw[nd];};
			};
		};
    };
    return;
}

static void set_psf_textures_to_buf(int ist_psf, int num_buf, struct psf_data **psf_s,
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
	int inum, iele, inod, k, ipsf;
	int iflag;
	double xx_tri[9], rtp_patch[9];
	
	for(inum=0; inum<num_buf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
        iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s[ipsf]->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s[ipsf]->xx_viz[inod][2];
		};
		iflag = latitude_longitude_on_map(xx_tri, rtp_patch);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			set_node_stride_VBO((ITHREE*inum+k), strided_buf);
			strided_buf->x_txur[0] =  rtp_patch[ITHREE*k+2] * ARCPI * HALF;
			strided_buf->x_txur[1] = -rtp_patch[ITHREE*k+1] * ARCPI;
		};
	};
	return;
}

static void set_psf_map_to_straided_buf(int ist_psf, int num_buf, struct psf_data **psf_s, 
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
	int inum, iele, inod, ipsf, nd, k;
	double xx_tri[9], xyz_map[9];
	
	for(inum=0; inum<num_buf; inum++){
		ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
		iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s[ipsf]->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s[ipsf]->xx_viz[inod][2];
		};
		projection_patch_4_map(xx_tri, xyz_map);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			set_node_stride_VBO((ITHREE*inum+k), strided_buf);
			strided_buf->x_draw[0] = xyz_map[ITHREE*k  ];
			strided_buf->x_draw[1] = xyz_map[ITHREE*k+1];
			strided_buf->x_draw[2] = 0.0;
			
			strided_buf->n_draw[0] = 0.0;
			strided_buf->n_draw[1] = 0.0;
			strided_buf->n_draw[2] = 1.0;
			
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = psf_s[ipsf]->color_nod[inod][nd];};
        };
    };
    return;
}

void draw_PSF_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = ied_psf - ist_psf;
	
	if(num_patch <= 0) return;
	
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
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	
	set_psf_nodes_to_buf(ist_psf, num_patch, shading_mode, 
								   psf_s, psf_m, psf_a, psf_buf);
	
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

void draw_PSF_texture_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct shader_ids *phong_texure, 
			struct gl_strided_buffer *psf_buf){
	int i;
	int num_patch = ied_psf - ist_psf;
	
	if(num_patch <= 0) return;
	
	glUseProgram(phong_texure->programId);
	transfer_matrix_to_shader(phong_texure, view_s);
	
	int id_numLight = glGetUniformLocation(phong_texure->programId, "num_lights");
	int id_lightPosition = glGetUniformLocation(phong_texure->programId, "LightSource[0].position");
	
	int id_MaterialAmbient = glGetUniformLocation(phong_texure->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(phong_texure->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(phong_texure->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(phong_texure->programId, "frontMaterial.shininess");
	
	int id_textureImage = glGetUniformLocation(phong_texure->programId, "image");
	
	int num_light = 1;
	GLfloat  lightposition[4] = {5.0, 5.0, -5.0,1.0};
	GLfloat white1[4] = {0.9, 0.9, 0.9, 1.0};
	GLfloat white2[4] = {0.7, 0.7, 0.7, 1.0};
	GLfloat white3[4] = {0.4, 0.4, 0.4, 1.0};
	GLfloat shine = 20.0;
	
	glUniform1i(id_numLight, num_light);
	glUniform4fv(id_lightPosition, 1, lightposition);
	
	glUniform4fv(id_MaterialAmbient, 1, white2);
	glUniform4fv(id_MaterialDiffuse, 1, white1);
	glUniform4fv(id_MaterialSpecular, 1, white3);
	glUniform1f(id_MaterialShiness, shine);
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	
	set_psf_nodes_to_buf(ist_psf, num_patch, shading_mode, 
				psf_s, psf_m, psf_a, psf_buf);
	set_psf_textures_to_buf(ist_psf, num_patch, 
				psf_s, psf_a, psf_buf);
	
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
	glVertexAttribPointer(4, 2, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_tex * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	glEnableVertexAttribArray(4);
	
	glActiveTexture(GL_TEXTURE_2D); 
	i = psf_a->ipsf_viz_far[ist_psf]-1;
	set_texture(psf_m[i]);
	glUniform1i(id_textureImage, 0);
	
	glBindVertexArray(0);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(psf_VAO);
	
	return;	
}


