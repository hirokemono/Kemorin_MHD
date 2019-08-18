
/* draw_arrows_4_PSF.c */

#include <OpenGL/gl3.h>
#include "draw_arrows_4_PSF.h"

static GLfloat arrow_c[4] = {0.8, 0.7, 0.6, 1.0};

static int count_psf_arrows_to_buf(int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int inod;
	
	int inum_buf = 0;
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
			if(psf_s->norm_nod[inod][0] != 0.0
						|| psf_s->norm_nod[inod][1] !=0.0
						|| psf_s->norm_nod[inod][2] !=0.0){
				inum_buf = inum_buf + ncorner;
			};
		};
	};
	
	return inum_buf;
}

static void set_psf_arrows_to_buf(int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct gl_strided_buffer *strided_buf) {
	float x_line[6], dir_line[6], color_line[8];
	float xyz[18*ncorner], nor[18*ncorner], col[24*ncorner];
	GLdouble dcolor[4];
	int num_wall, inum_buf;
	
	double v_tmp[3], v_xyz[3], x_rtp[3], d_mag;
    
	int inod, i, k, nd;
	
	int icomp = psf_s->istack_comp[psf_m->if_draw_psf];
	float radius = (float) psf_m->vector_thick;
	double ascale = ONE / psf_m->scale_vect;
	
	inum_buf = 0;
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
            if(psf_s->norm_nod[inod][0] != 0.0
               || psf_s->norm_nod[inod][1] !=0.0
               || psf_s->norm_nod[inod][2] !=0.0){
                for (k=0; k<3; k++) v_tmp[k] = psf_s->d_nod[inod][icomp+k];
			
                if(psf_s->id_coord[psf_m->if_draw_psf]==1){
                    position_2_sph_c(IONE, psf_s->xx_viz[inod], x_rtp);
                    sph_vector_to_xyz_vect(x_rtp[1], x_rtp[2], v_tmp, v_xyz);
                } else if(psf_s->id_coord[psf_m->if_draw_psf]==2){
                    position_2_sph_c(IONE, psf_s->xx_viz[inod], x_rtp);
                    cyl_vector_to_xyz_vect(x_rtp[2], v_tmp, v_xyz);
                } else {
                    for (k=0; k<3; k++) v_xyz[k] = v_tmp[k];
                };
			
				if(psf_m->ivect_tangential==TANGENTIAL_COMPONENT){
					for (k=0; k<3; k++) {
						v_xyz[k] = v_xyz[k] - psf_s->norm_nod[inod][k]
								* (  v_xyz[0]*psf_s->norm_nod[inod][0]
									+ v_xyz[1]*psf_s->norm_nod[inod][1]
									+ v_xyz[2]*psf_s->norm_nod[inod][2]);
					};
				};
				
				d_mag = sqrt(v_xyz[0]*v_xyz[0]+v_xyz[1]*v_xyz[1]+v_xyz[2]*v_xyz[2]);
				if(psf_m->vector_patch_color == RAINBOW_SURFACE){
					set_rainbow_color_code(psf_m->cmap_psf, d_mag, dcolor);
				} else {
					for(nd=0;nd<4;nd++){dcolor[nd] = arrow_c[nd];};
				}
				
				for (k=0; k<3; k++){
					x_line[k  ] = psf_s->xx_viz[inod][k];
					x_line[k+3] = psf_s->xx_viz[inod][k] + v_xyz[k]*ascale;
					dir_line[k  ] =  v_xyz[k];
					dir_line[k+3] =  v_xyz[k];
				};
				for (k=0; k<4; k++){
					color_line[k  ] =  dcolor[k];
					color_line[k+4] =  dcolor[k];
				};
				
				num_wall = set_cone_vertex(ncorner, radius, x_line, dir_line, color_line,
							xyz, nor, col);
				
				for (i=0; i<3*num_wall; i++) {
					set_node_stride_VBO((ITHREE*inum_buf+i), strided_buf);
					for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xyz[3*i+nd];};
					for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = nor[3*i+nd];};
					for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = col[4*i+nd];};
				};
				inum_buf = inum_buf + num_wall;
			};
		};
	};
	
	return;	
}


void draw_PSF_arrow_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int ncorner = 20;
	int inum_buf;
	
	int num_patch = count_psf_arrows_to_buf(ncorner, psf_s, psf_m);
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
	
	set_psf_arrows_to_buf(ncorner, psf_s, psf_m, psf_buf);
	
	
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
