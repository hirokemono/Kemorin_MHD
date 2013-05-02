
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

static GLfloat white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};

static void set_psf_nodes_to_buf(int ist_ele, int num_buf,
								 struct psf_data *psf_s, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, k;
	
	for(inum=0; inum<num_buf; inum++){
		iele = psf_s->iele_viz_far[inum+ist_ele]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			gl_buf->xyz[ITHREE*inum+k][0] = psf_s->xx_viz[inod][0];
			gl_buf->xyz[ITHREE*inum+k][1] = psf_s->xx_viz[inod][1];
			gl_buf->xyz[ITHREE*inum+k][2] = psf_s->xx_viz[inod][2];
		};
	};
	return;
}

static void set_psf_colors_to_buf(int ist_ele, int num_buf,
								  struct psf_data *psf_s, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, k;
	
	for(inum=0; inum<num_buf; inum++){
		iele = psf_s->iele_viz_far[inum+ist_ele]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			gl_buf->rgba[ITHREE*inum+k][0] = psf_s->color_nod[inod][0];
			gl_buf->rgba[ITHREE*inum+k][1] = psf_s->color_nod[inod][1];
			gl_buf->rgba[ITHREE*inum+k][2] = psf_s->color_nod[inod][2];
			gl_buf->rgba[ITHREE*inum+k][3] = psf_s->color_nod[inod][3];
		};
	};
	return;
}

static void set_psf_textures_to_buf(int ist_ele, int num_buf,
									struct psf_data *psf_s, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, k;
	int iflag;
	double xx_tri[9], rtp_patch[9];
	
	for(inum=0; inum<num_buf; inum++){
		iele = psf_s->iele_viz_far[inum+ist_ele]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		iflag = latitude_longitude_on_map(xx_tri, rtp_patch);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			gl_buf->xy[ITHREE*inum+k][0] =  rtp_patch[ITHREE*k+2] * ARCPI * HALF;
			gl_buf->xy[ITHREE*inum+k][1] = -rtp_patch[ITHREE*k+1] * ARCPI;
		};
	};
	return;
}

static void set_psf_nodes_to_map(int ist_ele, int num_buf,
									struct psf_data *psf_s, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, k;
	double xx_tri[9], xyz_map[9];
	
	for(inum=0; inum<num_buf; inum++){
		iele = inum + ist_ele;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		projection_patch_4_map(xx_tri, xyz_map);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			gl_buf->xy[ITHREE*inum+k][0] = xyz_map[ITHREE*k  ];
			gl_buf->xy[ITHREE*inum+k][1] = xyz_map[ITHREE*k+1];
		};
	};
	return;
}

static void set_psf_normals_to_buf(int ist_ele, int num_buf, int shading_mode, int polygon_mode, 
								   struct psf_data *psf_s, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, k;
	
	for(inum=0; inum<num_buf; inum++){
		iele = psf_s->iele_viz_far[inum+ist_ele]-1;
		
		if (shading_mode == SMOOTH_SHADE){
			for (k = 0; k < ITHREE; k++) {
				inod = psf_s->ie_viz[iele][k] - 1;
				gl_buf->norm[ITHREE*inum+k][0] = psf_s->norm_nod[inod][0];
				gl_buf->norm[ITHREE*inum+k][1] = psf_s->norm_nod[inod][1];
				gl_buf->norm[ITHREE*inum+k][2] = psf_s->norm_nod[inod][2];
			};
		} else {
			for (k = 0; k < ITHREE; k++) {
				gl_buf->norm[ITHREE*inum+k][0] = psf_s->norm_ele[iele][0];
				gl_buf->norm[ITHREE*inum+k][1] = psf_s->norm_ele[iele][1];
				gl_buf->norm[ITHREE*inum+k][2] = psf_s->norm_ele[iele][2];
			};
		};
		
		if(polygon_mode == REVERSE_POLYGON){
			for (k = 0; k < ITHREE; k++) {
				gl_buf->norm[ITHREE*inum+k][0] = -gl_buf->norm[ITHREE*inum+k][0];
				gl_buf->norm[ITHREE*inum+k][1] = -gl_buf->norm[ITHREE*inum+k][1];
				gl_buf->norm[ITHREE*inum+k][2] = -gl_buf->norm[ITHREE*inum+k][2];
			};
		};
	};
	return;
}


void draw_patch_4_PSF(int shading_mode, struct psf_data *psf_s, struct psf_menu_val *psf_m,
					  struct buffer_for_gl *gl_buf){
	int icou, num;
	
	set_color_code_for_psf(psf_s, psf_m);

	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
	
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	icou = 0;
	while(icou < psf_s->nele_viz){
		if( (icou+NSIZE_GL_BUFFER) <= psf_s->nele_viz) {num = NSIZE_GL_BUFFER;}
		else										   {num = psf_s->nele_viz-icou;};

		set_psf_nodes_to_buf(icou, num, psf_s, gl_buf);
		set_psf_normals_to_buf(icou, num, shading_mode, psf_m->polygon_mode_psf, 
							   psf_s, gl_buf);
		set_psf_colors_to_buf(icou, num, psf_s, gl_buf);

		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num));
		icou = icou + NSIZE_GL_BUFFER;
	};

	glEnable(GL_CULL_FACE);
	
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	
	return;	
}

void draw_patches_4_map(int shading_mode, struct psf_data *psf_s,
						struct psf_menu_val *psf_m, struct buffer_for_gl *gl_buf) {
	int icou, num;
	
	set_color_code_for_psf(psf_s, psf_m);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITWO, GL_FLOAT, IZERO, gl_buf->xy);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	icou = 0;
	while(icou < psf_s->nele_viz){
		if( (icou+NSIZE_GL_BUFFER) <= psf_s->nele_viz) {num = NSIZE_GL_BUFFER;}
		else										   {num = psf_s->nele_viz-icou;};
		
		set_psf_nodes_to_map(icou, num, psf_s, gl_buf);
		set_psf_colors_to_buf(icou, num, psf_s, gl_buf);
		
		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num));
		icou = icou + NSIZE_GL_BUFFER;
	};

	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);

	return;	
}

void draw_arrow_4_PSF(struct psf_data *psf_s, struct psf_menu_val *psf_m) {
	GLfloat arrow_c[4] =   {0.8, 0.7, 0.6, 1.0};
	double v_tmp[3], v_xyz[3], d_tri[3], x_rtp[3], d_mag;
	int inod, k;
	int icomp = psf_s->istack_comp[psf_m->if_draw_psf];
	int iflag_coord = psf_s->id_coord[psf_m->if_draw_psf];
	double ascale = ONE / psf_m->scale_vect;
	
	glPushMatrix();
	glLoadIdentity();
	glPopMatrix();
	
	glShadeModel(GL_SMOOTH);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	if(psf_m->vector_patch_color != RAINBOW_SURFACE){
		glColor4fv(arrow_c);
	};
	
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
			for (k=0; k<3; k++) v_tmp[k] = psf_s->d_nod[inod][icomp+k];
			
			if(iflag_coord==1){
				position_2_sph_c(IONE, psf_s->xx_viz[inod], x_rtp);
				sph_vector_to_xyz_vect(x_rtp[1], x_rtp[2], v_tmp, v_xyz);
			} else if(iflag_coord==2){
				position_2_sph_c(IONE, psf_s->xx_viz[inod], x_rtp);
				cyl_vector_to_xyz_vect(x_rtp[2], v_tmp, v_xyz);
			} else {
				for (k=0; k<3; k++) v_xyz[k] = v_tmp[k];
			};
			
			for (k=0; k<3; k++) {
				d_tri[k] = psf_s->xx_viz[inod][k] + v_xyz[k]*ascale;
			};
			
			d_mag = sqrt(v_xyz[0]*v_xyz[0]+v_xyz[1]*v_xyz[1]+v_xyz[2]*v_xyz[2]);
			if(psf_m->vector_patch_color == RAINBOW_SURFACE){
				set_rainbow_PSF_c(d_mag, psf_m->cmap_psf);
			}
			
			glDrawArrowf((GLfloat) psf_s->xx_viz[inod][0],
						 (GLfloat) psf_s->xx_viz[inod][1],
						 (GLfloat) psf_s->xx_viz[inod][2],
						 (GLfloat) d_tri[0], (GLfloat) d_tri[1], (GLfloat) d_tri[2],
					     (d_mag*ascale*0.01) );
		}
	};
	
	return;	
}

void set_texture(struct psf_menu_val *psf_m){
	/* Preference for resiging texture */
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glTexImage2D(
		GL_TEXTURE_2D , 0 , GL_RGBA , psf_m->texture_width , psf_m->texture_height ,
		0 , GL_RGBA , GL_UNSIGNED_BYTE , psf_m->texture_rgba);
	
	glBindTexture(GL_TEXTURE_2D , psf_m->texture_name[0]);
	return;
};

void draw_texure_4_PSF(int shading_mode, struct psf_data *psf_s,
					   struct psf_menu_val *psf_m, struct buffer_for_gl *gl_buf){
	int icou, num;
	
	set_texture(psf_m);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
	glTexCoordPointer(ITWO, GL_FLOAT, IZERO, gl_buf->xy);
	glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
	
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glColor4fv(white);
	
	icou = 0;
	while(icou < psf_s->nele_viz){
		if( (icou+NSIZE_GL_BUFFER) <= psf_s->nele_viz) {num = NSIZE_GL_BUFFER;}
		else										   {num = psf_s->nele_viz-icou;};
		
		set_psf_nodes_to_buf(icou, num, psf_s, gl_buf);
		set_psf_normals_to_buf(icou, num, shading_mode, psf_m->polygon_mode_psf, 
							   psf_s, gl_buf);
		set_psf_textures_to_buf(icou, num, psf_s, gl_buf);
		
		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num));
		icou = icou + NSIZE_GL_BUFFER;
	};
	glDisable(GL_TEXTURE_2D);
	glEnable(GL_CULL_FACE);

	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);

	return;	
}
