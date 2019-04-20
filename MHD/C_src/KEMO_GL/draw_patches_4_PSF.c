
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

static GLfloat white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};

static void set_psf_nodes_to_buf(int ist_psf, int num_buf, struct psf_data **psf_s, 
								 struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, k, ipsf;
	
	for(inum=0; inum<num_buf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
		iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			gl_buf->xyz[ITHREE*inum+k][0] = psf_s[ipsf]->xx_viz[inod][0];
			gl_buf->xyz[ITHREE*inum+k][1] = psf_s[ipsf]->xx_viz[inod][1];
			gl_buf->xyz[ITHREE*inum+k][2] = psf_s[ipsf]->xx_viz[inod][2];
		};
	};
	return;
}

static void set_psf_colors_to_buf(int ist_psf, int num_buf, struct psf_data **psf_s, 
								  struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
	int inum, iele, inod, k, ipsf;
	
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

static void set_psf_normals_to_buf(int ist_psf, int num_buf, int shading_mode, 
                                   struct psf_data **psf_s, struct psf_menu_val **psf_m, 
                                   struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
    int inum, iele, inod, k, ipsf;
    
    for(inum=0; inum<num_buf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
        iele = psf_a->iele_viz_far[inum+ist_psf]-1;
        
        if (shading_mode == SMOOTH_SHADE){
            for (k = 0; k < ITHREE; k++) {
                inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
                gl_buf->norm[ITHREE*inum+k][0] = psf_s[ipsf]->norm_nod[inod][0];
                gl_buf->norm[ITHREE*inum+k][1] = psf_s[ipsf]->norm_nod[inod][1];
                gl_buf->norm[ITHREE*inum+k][2] = psf_s[ipsf]->norm_nod[inod][2];
            };
        } else {
            for (k = 0; k < ITHREE; k++) {
                gl_buf->norm[ITHREE*inum+k][0] = psf_s[ipsf]->norm_ele[iele][0];
                gl_buf->norm[ITHREE*inum+k][1] = psf_s[ipsf]->norm_ele[iele][1];
                gl_buf->norm[ITHREE*inum+k][2] = psf_s[ipsf]->norm_ele[iele][2];
            };
        };
        
        if(psf_m[ipsf]->polygon_mode_psf == REVERSE_POLYGON){
            for (k = 0; k < ITHREE; k++) {
                gl_buf->norm[ITHREE*inum+k][0] = -gl_buf->norm[ITHREE*inum+k][0];
                gl_buf->norm[ITHREE*inum+k][1] = -gl_buf->norm[ITHREE*inum+k][1];
                gl_buf->norm[ITHREE*inum+k][2] = -gl_buf->norm[ITHREE*inum+k][2];
            };
        };
    };
    return;
}


static void set_psf_textures_to_buf(int ist_psf, int num_buf, struct psf_data **psf_s,
									struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
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
			gl_buf->xy[ITHREE*inum+k][0] =  rtp_patch[ITHREE*k+2] * ARCPI * HALF;
			gl_buf->xy[ITHREE*inum+k][1] = -rtp_patch[ITHREE*k+1] * ARCPI;
		};
	};
	return;
}

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



void draw_patch_4_PSF(int shading_mode, int ist_psf, int ied_psf, 
                      struct psf_data **psf_s, struct psf_menu_val **psf_m,
					  struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
	int icou, num;

    if(ied_psf-ist_psf <= 0) return;

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
	
	icou = ist_psf;
	while(icou < ied_psf){
		if( (icou+NSIZE_GL_BUFFER) <= ied_psf) {num = NSIZE_GL_BUFFER;}
		else                                   {num = ied_psf-icou;};

		set_psf_nodes_to_buf(icou, num, psf_s, psf_a, gl_buf);
		set_psf_normals_to_buf(icou, num, shading_mode, 
							   psf_s, psf_m, psf_a, gl_buf);
		set_psf_colors_to_buf(icou, num, psf_s, psf_a, gl_buf);

		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num));
		icou = icou + NSIZE_GL_BUFFER;
	};

	glEnable(GL_CULL_FACE);
	
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	
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
	
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
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

void draw_arrow_4_PSF(struct psf_data *psf_s, struct psf_menu_val *psf_m, struct buffer_for_gl *gl_buf) {
    int ncorner = 20;
    float x_line[6], dir_line[6], color_line[8];
    float xyz[18*ncorner], nor[18*ncorner], col[24*ncorner];
    GLdouble dcolor[4];
    int num_wall, inum_buf;

	GLfloat arrow_c[4] =   {0.8, 0.7, 0.6, 1.0};
	double v_tmp[3], v_xyz[3], x_rtp[3], d_mag;
    
	int inod, i, k, nd;
	int icomp = psf_s->istack_comp[psf_m->if_draw_psf];
	int iflag_coord = psf_s->id_coord[psf_m->if_draw_psf];
	int iflag_tangential = psf_m->ivect_tangential;
    float radius = (float) psf_m->vector_thick;
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
	
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
    glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
    glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
    
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glShadeModel(GL_SMOOTH);
    glDisable(GL_CULL_FACE);

    
    inum_buf = 0;
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
            if(psf_s->norm_nod[inod][0] != 0.0
               || psf_s->norm_nod[inod][1] !=0.0
               || psf_s->norm_nod[inod][2] !=0.0){
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
			
                if(iflag_tangential==TANGENTIAL_COMPONENT){
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
                    for(nd=0;nd<4;nd++){dcolor[nd] = white[nd];}
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
		};
	};
    if(inum_buf > 0){glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));};

    glEnable(GL_CULL_FACE);
    
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
	
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

void draw_texure_4_PSF(int shading_mode, int ist_psf, int ied_psf, 
                       struct psf_data **psf_s, struct psf_menu_val **psf_m,
                       struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf){
	int i, icou, num;
    
    if(ied_psf-ist_psf <= 0) return;
    
    i = psf_a->ipsf_viz_far[ist_psf]-1;
    set_texture(psf_m[i]);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, gl_buf->xyz);
    glColorPointer(IFOUR, GL_FLOAT, IZERO, gl_buf->rgba);
	glTexCoordPointer(ITWO, GL_FLOAT, IZERO, gl_buf->xy);
	glNormalPointer(GL_FLOAT, IZERO, gl_buf->norm);
	
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	icou = ist_psf;
	while(icou < ied_psf){
		if( (icou+NSIZE_GL_BUFFER) <= ied_psf) {num = NSIZE_GL_BUFFER;}
		else								   {num = ied_psf-icou;};
		
		set_psf_nodes_to_buf(icou, num, psf_s, psf_a, gl_buf);
		set_psf_normals_to_buf(icou, num, shading_mode, 
							   psf_s, psf_m, psf_a, gl_buf);
        set_psf_colors_to_buf(icou, num, psf_s, psf_a, gl_buf);
		set_psf_textures_to_buf(icou, num, psf_s, psf_a, gl_buf);
		
		glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num));
		icou = icou + NSIZE_GL_BUFFER;
	};
	glDisable(GL_TEXTURE_2D);
	glEnable(GL_CULL_FACE);

	glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);

	return;	
}
