/*
// draw_colorbar_gl.c
*/

#include <OpenGL/gl3.h>
#include "draw_colorbar_gl.h"

static int ibase_8x12;
static int ibase_12x16;
static int ibase_16x24;
static int ibase_20x32;

static const GLfloat black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};


static void ysGlPlotBitmap2d_retina(int iflag_retina,
                                    GLfloat x_plot, GLfloat y_plot,
                                    const char *label){
    if(iflag_retina == IONE){
        ysGlPlotBitmap2d(ibase_16x24, (x_plot-HALF), (y_plot-HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot+HALF), (y_plot-HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot+HALF), (y_plot+HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot-HALF), (y_plot+HALF), (GLubyte *)label);

        ysGlPlotBitmap2d(ibase_16x24, (x_plot-HALF), (y_plot    ), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot+HALF), (y_plot    ), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot    ), (y_plot-HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot    ), (y_plot+HALF), (GLubyte *)label);

        ysGlPlotBitmap2d(ibase_16x24, (x_plot    ), (y_plot    ), (GLubyte *)label);
    } else {
        ysGlPlotBitmap2d(ibase_8x12, (x_plot    ), (y_plot    ), (GLubyte *)label);
    }
	
	
    return;
}

void init_colorbar_fonts(){
	
	/*printf("Base %d \n",ibase_8x12);*/
	ibase_8x12 = glGenLists(0x100);
	YsGlUseFontBitmap8x12(ibase_8x12);
	/*printf("Base %d \n",ibase_12x16);*/
	ibase_12x16 = glGenLists(0x100);
	YsGlUseFontBitmap12x16(ibase_12x16);
	
	/*printf("Base %d \n",ibase_16x24);*/
	ibase_16x24 = glGenLists(0x100);
	YsGlUseFontBitmap16x24(ibase_16x24);
	/*printf("Base %d \n",ibase_20x32);*/
	ibase_20x32 = glGenLists(0x100);
	YsGlUseFontBitmap20x32(ibase_20x32);
	return;
}


struct cbar_work{
	int iflag_zero;
	
	GLfloat xwin;
	GLfloat ywin;
	
	GLfloat xbar_min;
	GLfloat xbar_max;
	GLfloat xbar_mid;
	
	GLfloat ybar_min;
	GLfloat ybar_max;
	GLfloat ydelta;
	GLfloat yline_zero;
	
	int num_quad;
	double psf_min;
	double psf_max;
};

void set_one_quad_to_buf(int i_quad, 
			GLfloat x1[3], GLfloat x2[3], GLfloat x3[3], GLfloat x4[3], 
			GLfloat c1[4], GLfloat c2[4], GLfloat c3[4], GLfloat c4[4], 
			struct gl_strided_buffer *strided_buf){
	int nd;
	
	set_node_stride_VBO(6*i_quad, strided_buf);
	for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = x1[nd];}
	for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = c1[nd];};
	
	set_node_stride_VBO(6*i_quad+1, strided_buf);
	for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = x2[nd];}
	for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = c2[nd];};
		
	set_node_stride_VBO(6*i_quad+2, strided_buf);
	for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = x3[nd];}
	for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = c3[nd];};
		
	set_node_stride_VBO(6*i_quad+3, strided_buf);
	for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = x3[nd];}
	for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = c3[nd];};
		
	set_node_stride_VBO(6*i_quad+4, strided_buf);
	for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = x4[nd];}
	for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = c4[nd];};
	
	set_node_stride_VBO(6*i_quad+5, strided_buf);
	for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = x1[nd];};
	for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = c1[nd];};
	return;
};

void solid_colorbar_box_to_buf(struct colormap_params *cmap_s, 
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	GLfloat y1;
	GLfloat x1[3], x2[3], x3[3], x4[3];
	GLfloat c1[4], c2[4], c3[4], c4[4];
	double psf_value;
	double f_color[4], l_color[4];
	int i, nd;
	
	set_rainbow_color_code(cmap_s, cbar_wk->psf_min, f_color);
	f_color[3] = ONE;
	for(nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	
	for(i=0;i<cbar_wk->num_quad;i++){
		y1 = cbar_wk->ybar_min + cbar_wk->ydelta * (GLfloat) i;
		psf_value = cbar_wk->psf_min + (cbar_wk->psf_max - cbar_wk->psf_min)
									* (double)(i+1) / (double)cbar_wk->num_quad;
		set_rainbow_color_code(cmap_s, psf_value, f_color);
		f_color[3] = ONE;
		
		x1[0] = cbar_wk->xbar_min;
		x1[1] = y1;
		x1[2] = 0.001;
		for(nd=0;nd<4;nd++) {c1[nd] = l_color[nd];};
		
		x2[0] = cbar_wk->xbar_mid;
		x2[1] = y1;
		x2[2] = 0.001;
		for(nd=0;nd<4;nd++) {c2[nd] = l_color[nd];};
		
		x3[0] = cbar_wk->xbar_mid;
		x3[1] = y1 + cbar_wk->ydelta;
		x3[2] = 0.001;
		for(nd=0;nd<4;nd++) {c3[nd] = f_color[nd];};
		
		x4[0] = cbar_wk->xbar_min;
		x4[1] = y1 + cbar_wk->ydelta;
		x4[2] = 0.001;
		for(nd=0;nd<4;nd++) {c4[nd] = f_color[nd];};
		
		set_one_quad_to_buf(i, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
		
		for(nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	};
	return;
};

void fade_colorbar_box_to_buf(int ist, struct colormap_params *cmap_s, GLfloat *bg_color, 
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	GLfloat y1;
	GLfloat x1[3], x2[3], x3[3], x4[3];
	GLfloat c1[4], c2[4], c3[4], c4[4];
	
	double psf_value;
	double f_color[4], l_color[4];
	int i, nd;
	
	
	set_rainbow_color_code(cmap_s, cbar_wk->psf_min, f_color);
	for (nd=0; nd<3; nd++) {
		f_color[nd] = f_color[nd] * f_color[3]
				+ bg_color[nd] * (ONE - f_color[3]);
	};
	f_color[3] = ONE;
	for (nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	
	for(i=0;i<cbar_wk->num_quad;i++){
		y1 = cbar_wk->ybar_min + cbar_wk->ydelta * (GLfloat)i;
		
		psf_value = cbar_wk->psf_min + (cbar_wk->psf_max - cbar_wk->psf_min)
									* (double)(i+1) / (double)cbar_wk->num_quad;
		set_rainbow_color_code(cmap_s, psf_value, f_color);
	
		for (nd=0; nd<3; nd++) {
			f_color[nd] = f_color[nd] * f_color[3]
					+ bg_color[nd] * (ONE - f_color[3]);
		};
		f_color[3] = ONE;
		
		x1[0] = cbar_wk->xbar_mid;
		x1[1] = y1;
		x1[2] = 0.001;
		for(nd=0;nd<4;nd++) {c1[nd] = l_color[nd];};
		
		x2[0] = cbar_wk->xbar_max;
		x2[1] = y1;
		x2[2] = 0.001;
		for(nd=0;nd<4;nd++) {c2[nd] = l_color[nd];};
		
		x3[0] = cbar_wk->xbar_max;
		x3[1] = y1 + cbar_wk->ydelta;
		x3[2] = 0.001;
		for(nd=0;nd<4;nd++) {c3[nd] = f_color[nd];};
		
		x4[0] = cbar_wk->xbar_mid;
		x4[1] = y1 + cbar_wk->ydelta;
		x4[2] = 0.001;
		for(nd=0;nd<4;nd++) {c4[nd] = f_color[nd];};
		
		set_one_quad_to_buf(ist+i, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
		
		for (nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	};
	return;
};

void colorbar_frame_to_buf(int iflag_retina, GLfloat *text_color,
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	GLfloat x1[3], x2[3], x3[3], x4[3];
	GLfloat c1[4], c2[4], c3[4], c4[4];
	int nd;
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x1[2] = 0.001;
	for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
	
	x2[0] = cbar_wk->xbar_min;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x2[2] = 0.001;
	for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
	
	x3[0] = cbar_wk->xbar_min;
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	x3[2] = 0.001;
	for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	x4[2] = 0.001;
	for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_max;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x1[2] = 0.001;
	for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
	
	x2[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x2[2] = 0.001;
	for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
	
	x3[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	x3[2] = 0.001;
	for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
	
	x4[0] = cbar_wk->xbar_max;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	x4[2] = 0.001;
	for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
	set_one_quad_to_buf(1, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x1[2] = 0.001;
	for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x2[2] = 0.001;
	for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_min;
	x3[2] = 0.001;
	for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_min;
	x4[2] = 0.001;
	for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
	set_one_quad_to_buf(2, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_max;
	x1[2] = 0.001;
	for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_max;
	x2[2] = 0.001;
	for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	x3[2] = 0.001;
	for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	x4[2] = 0.001;
	for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
	set_one_quad_to_buf(3, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	if(cbar_wk->iflag_zero == 1){
		x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x1[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x1[2] = 0.001;
		for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
		
		x2[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x2[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x2[2] = 0.001;
		for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
		
		x3[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x3[1] = cbar_wk->yline_zero;
		x3[2] = 0.001;
		for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
		
		x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x4[1] = cbar_wk->yline_zero;
		x4[2] = 0.001;
		for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
		set_one_quad_to_buf(4, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	};
	return;
};

void set_colorbar_position(int iflag_retina, GLint nx_win, GLint ny_win,
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk){
	cbar_wk->num_quad = 64;
	
	cbar_wk->xwin = (GLfloat)nx_win;
	cbar_wk->ywin = (GLfloat)ny_win;
	
    if( cbar_wk->xwin >= 640*(iflag_retina+1) ){
        cbar_wk->xbar_max = cbar_wk->xwin * 0.875;
    } else {
        cbar_wk->xbar_max = cbar_wk->xwin - (iflag_retina+1) * 80;
    }
	cbar_wk->xbar_min = cbar_wk->xbar_max - 0.025 * cbar_wk->xwin;
	cbar_wk->xbar_mid = (cbar_wk->xbar_min + cbar_wk->xbar_max) * 0.5;
	cbar_wk->ybar_min = 0.05 * cbar_wk->ywin;
	cbar_wk->ybar_max = 0.25 * cbar_wk->ywin;
	cbar_wk->ydelta =  (cbar_wk->ybar_max - cbar_wk->ybar_min)
			/ ((GLfloat)64);
	
	cbar_wk->iflag_zero = 0;
	
	cbar_wk->psf_min = cmap_s->color_data[IZERO];
	cbar_wk->psf_max = cmap_s->color_data[cmap_s->n_color_point-1];
	if( (cbar_wk->psf_min*cbar_wk->psf_max) < ZERO ) cbar_wk->iflag_zero = 1;
	
	cbar_wk->yline_zero = cbar_wk->ybar_min 
		+ (cbar_wk->ybar_max - cbar_wk->ybar_min) * (-cbar_wk->psf_min) 
		/ (cbar_wk->psf_max - cbar_wk->psf_min);
	
	return;
}

void draw_colorbar_VAO(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4], struct colormap_params *cmap_s,
			struct VAO_ids *cbar_VAO, struct kemoview_shaders *kemo_shaders,
			struct gl_strided_buffer *cbar_buf){
	int inum, nd;
	int num_patch, num_edge;
	
	GLdouble orthogonal[16];
	GLfloat xy_buf[384][2];
	GLfloat rgba_buf[384][4];
	char minlabel[20], maxlabel[20], zerolabel[20];
	
	struct cbar_work *cbar_wk = (struct cbar_work *) malloc(sizeof(struct cbar_work));
	set_colorbar_position(iflag_retina, nx_win, ny_win, cmap_s, cbar_wk);
	
	sprintf(minlabel, "% 3.2E",cbar_wk->psf_min);
	sprintf(maxlabel, "% 3.2E",cbar_wk->psf_max);
	sprintf(zerolabel,"% 3.2E",ZERO);
	
	sprintf(colorbar_text, "% 3.2E% 3.2E% 3.2E", cbar_wk->psf_min, cbar_wk->psf_max, ZERO);
	
	orthogonal_glmat_c(0.0, cbar_wk->xwin, 0.0, cbar_wk->ywin, -1.0, 1.0, orthogonal);
	
	if(cmap_s->min_opacity < 1.0) {
		glEnable(GL_BLEND);
		glEnable(GL_TRUE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glEnable(GL_MULTISAMPLE);
	}
	
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	num_patch = 4 * cbar_wk->num_quad;
	set_buffer_address_4_patch(ITHREE*num_patch, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	solid_colorbar_box_to_buf(cmap_s, cbar_wk, cbar_buf);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	fade_colorbar_box_to_buf(cbar_wk->num_quad, cmap_s, bg_color, cbar_wk, cbar_buf);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	
	glGenVertexArrays(1, &cbar_VAO->id_VAO);
	glBindVertexArray(cbar_VAO->id_VAO);
	
	glGenBuffers(1, &cbar_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * cbar_buf->num_nod_buf*cbar_buf->ncomp_buf,
				 cbar_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, cbar_buf->istride,
						  (GLvoid*) (cbar_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, cbar_buf->istride, 
						  (GLvoid*) (cbar_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	if(cmap_s->min_opacity < 1.0) {
		glEnable(GL_BLEND);
		glEnable(GL_TRUE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glEnable(GL_MULTISAMPLE);
	}
	
	glBindVertexArray(cbar_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(cbar_VAO);
	
	num_patch = 2*(cbar_wk->iflag_zero + IFOUR);
	set_buffer_address_4_patch(ITHREE*num_patch, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	colorbar_frame_to_buf(iflag_retina, text_color, cbar_wk, cbar_buf);
	
	glGenVertexArrays(1, &cbar_VAO->id_VAO);
	glBindVertexArray(cbar_VAO->id_VAO);
	
	glGenBuffers(1, &cbar_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * cbar_buf->num_nod_buf*cbar_buf->ncomp_buf,
				 cbar_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, cbar_buf->istride,
						  (GLvoid*) (cbar_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, cbar_buf->istride, 
						  (GLvoid*) (cbar_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	glBindVertexArray(cbar_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(cbar_VAO);
	
	if(cmap_s->min_opacity < 1.0) {
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	}
	
	unsigned char minBmp[8*8*12];
	unsigned char maxBmp[8*8*12];
	unsigned char zeroBmp[8*8*12];
	YsGlWriteStringToSingleBitBitmap(minlabel, minBmp, 8*8, 12, 0, 0, 
				YsFont12x16, 8, 12);
	YsGlWriteStringToSingleBitBitmap(maxlabel, maxBmp, 8*8, 12, 0, 0, 
				YsFont12x16, 8, 12);
	if(cbar_wk->iflag_zero == 1){
	YsGlWriteStringToSingleBitBitmap(zerolabel, zeroBmp, 8*8, 12, 0, 0, 
				YsFont12x16, 8, 12);
	};
	return;
	
	
	
	
	
	glColor4fv(text_color);
	ysGlPlotBitmap2d_retina(iflag_retina,
		(cbar_wk->xbar_max+3.0), (cbar_wk->ybar_min-6.0), minlabel);
	ysGlPlotBitmap2d_retina(iflag_retina,
				(cbar_wk->xbar_max+3.0), (cbar_wk->ybar_max-6.0), maxlabel);
	
	
	if(cbar_wk->iflag_zero == 1){
		ysGlPlotBitmap2d_retina(iflag_retina, (cbar_wk->xbar_max+3.0),
                                (cbar_wk->yline_zero-6.0), zerolabel);
	};
	
	free(cbar_wk);
	glColor4fv(black);
    glEnable(GL_LIGHTING);
	return;
}

