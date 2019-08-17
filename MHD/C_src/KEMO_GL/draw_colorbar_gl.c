/*
// draw_colorbar_gl.c
*/

#include <OpenGL/gl3.h>
#include "draw_colorbar_gl.h"

#define IHIGHT_TXT 20
#define IWIDTH_TXT 140
#define NPIXEL_TXT 2800

static int ibase_8x12;
static int ibase_12x16;
static int ibase_16x24;
static int ibase_20x32;

static const GLfloat black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};

static unsigned char numBMP[12*IHIGHT_TXT*IWIDTH_TXT];
static unsigned char testBMP[9*IHIGHT_TXT*IWIDTH_TXT];


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

void set_one_texture_to_buf(int i_quad, 
			GLfloat t1[2], GLfloat t2[2], GLfloat t3[2], GLfloat t4[2], 
			struct gl_strided_buffer *strided_buf){
	int nd;
	
	set_node_stride_VBO(6*i_quad, strided_buf);
	for(nd=0;nd<2;nd++) {strided_buf->x_txur[nd] = t1[nd];}
	
	set_node_stride_VBO(6*i_quad+1, strided_buf);
	for(nd=0;nd<2;nd++) {strided_buf->x_txur[nd] = t2[nd];}
		
	set_node_stride_VBO(6*i_quad+2, strided_buf);
	for(nd=0;nd<2;nd++) {strided_buf->x_txur[nd] = t3[nd];}
		
	set_node_stride_VBO(6*i_quad+3, strided_buf);
	for(nd=0;nd<2;nd++) {strided_buf->x_txur[nd] = t3[nd];}
		
	set_node_stride_VBO(6*i_quad+4, strided_buf);
	for(nd=0;nd<2;nd++) {strided_buf->x_txur[nd] = t4[nd];}
	
	set_node_stride_VBO(6*i_quad+5, strided_buf);
	for(nd=0;nd<2;nd++) {strided_buf->x_txur[nd] = t1[nd];};
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
	
	x1[0] = cbar_wk->xbar_min;
	x2[0] = cbar_wk->xbar_mid;
	x3[0] = cbar_wk->xbar_mid;
	x4[0] = cbar_wk->xbar_min;
	x1[2] = 0.001;
	x2[2] = 0.001;
	x3[2] = 0.001;
	x4[2] = 0.001;
	
	for(i=0;i<cbar_wk->num_quad;i++){
		y1 = cbar_wk->ybar_min + cbar_wk->ydelta * (GLfloat) i;
		psf_value = cbar_wk->psf_min + (cbar_wk->psf_max - cbar_wk->psf_min)
									* (double)(i+1) / (double)cbar_wk->num_quad;
		set_rainbow_color_code(cmap_s, psf_value, f_color);
		f_color[3] = ONE;
		
		x1[1] = y1;
		for(nd=0;nd<4;nd++) {c1[nd] = l_color[nd];};
		
		x2[1] = y1;
		for(nd=0;nd<4;nd++) {c2[nd] = l_color[nd];};
		
		x3[1] = y1 + cbar_wk->ydelta;
		for(nd=0;nd<4;nd++) {c3[nd] = f_color[nd];};
		
		x4[1] = y1 + cbar_wk->ydelta;
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
	
	x1[0] = cbar_wk->xbar_mid;
	x2[0] = cbar_wk->xbar_max;
	x3[0] = cbar_wk->xbar_max;
	x4[0] = cbar_wk->xbar_mid;
	x1[2] = 0.001;
	x2[2] = 0.001;
	x3[2] = 0.001;
	x4[2] = 0.001;
	
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
		
		x1[1] = y1;
		for(nd=0;nd<4;nd++) {c1[nd] = l_color[nd];};
		
		x2[1] = y1;
		for(nd=0;nd<4;nd++) {c2[nd] = l_color[nd];};
		
		x3[1] = y1 + cbar_wk->ydelta;
		for(nd=0;nd<4;nd++) {c3[nd] = f_color[nd];};
		
		x4[1] = y1 + cbar_wk->ydelta;
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
	
	x1[2] = 0.001;
	x2[2] = 0.001;
	x3[2] = 0.001;
	x4[2] = 0.001;
	
	for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x2[0] = cbar_wk->xbar_min;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x3[0] = cbar_wk->xbar_min;
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_max;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x2[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x3[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	
	x4[0] = cbar_wk->xbar_max;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	set_one_quad_to_buf(1, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_min;
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_min;
	set_one_quad_to_buf(2, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_max;
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_max;
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	set_one_quad_to_buf(3, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	if(cbar_wk->iflag_zero == 1){
		x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x1[1] = cbar_wk->yline_zero - iflag_retina - 1;
		
		x2[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x2[1] = cbar_wk->yline_zero - iflag_retina - 1;
		
		x3[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x3[1] = cbar_wk->yline_zero;
		
		x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x4[1] = cbar_wk->yline_zero;
		set_one_quad_to_buf(4, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	};
	return;
};

void colorbar_mbox_to_buf(int iflag_retina, GLfloat *text_color,
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	GLfloat x1[3], x2[3], x3[3], x4[3];
	GLfloat c1[4], c2[4], c3[4], c4[4];
	GLfloat t1[2], t2[2], t3[2], t4[2];
	int nd;
	
	x1[0] = cbar_wk->xbar_max + 8.0*(iflag_retina + 1);
	x2[0] = cbar_wk->xbar_max + 88.0*(iflag_retina + 1);
	x3[0] = cbar_wk->xbar_max + 88.0*(iflag_retina + 1);
	x4[0] = cbar_wk->xbar_max + 8.0*(iflag_retina + 1);
	x1[2] = 0.001;
	x2[2] = 0.001;
	x3[2] = 0.001;
	x4[2] = 0.001;
	for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
	
	t1[0] = 0.0;
	t2[0] = 1.0;
	t3[0] = 1.0;
	t4[0] = 0.0;
	
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x3[1] = cbar_wk->ybar_min + 12.0 * (iflag_retina + 1);
	x4[1] = cbar_wk->ybar_min + 12.0 * (iflag_retina + 1);
	t1[1] = 0.0;
	t2[1] = 0.0;
	t3[1] = 1.0 / 3.0;
	t4[1] = 1.0 / 3.0;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	set_one_texture_to_buf(0, t1, t2, t3, t4, strided_buf);
	
	
	x1[1] = cbar_wk->ybar_max - iflag_retina - 1;
	x2[1] = cbar_wk->ybar_max - iflag_retina - 1;
	x3[1] = cbar_wk->ybar_max + 12.0 * (iflag_retina + 1);
	x4[1] = cbar_wk->ybar_max + 12.0 * (iflag_retina + 1);
	t1[1] = 1.0 / 3.0;
	t2[1] = 1.0 / 3.0;
	t3[1] = 2.0 / 3.0;
	t4[1] = 2.0 / 3.0;
	set_one_quad_to_buf(1, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	set_one_texture_to_buf(1, t1, t2, t3, t4, strided_buf);
	
	if(cbar_wk->iflag_zero == 1){
		x1[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x2[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x3[1] = cbar_wk->yline_zero + 12.0 * (iflag_retina + 1);
		x4[1] = cbar_wk->yline_zero + 12.0 * (iflag_retina + 1);
		t1[1] = 2.0 / 3.0;
		t2[1] = 2.0 / 3.0;
		t3[1] = 1.0;
		t4[1] = 1.0;
		set_one_quad_to_buf(2, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
		set_one_texture_to_buf(2, t1, t2, t3, t4, strided_buf);
	};
	return;
};

void draw_colorbar_VAO(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4], struct colormap_params *cmap_s,
			struct VAO_ids *cbar_VAO, struct kemoview_shaders *kemo_shaders,
			struct gl_strided_buffer *cbar_buf){
	int num_patch;
	
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
	
	glEnable(GL_BLEND);
	glEnable(GL_TRUE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glEnable(GL_MULTISAMPLE);
	
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
	
	int i;
	for(i=0;i<12*IWIDTH_TXT*IHIGHT_TXT;i++){
		numBMP[i] =  0;
	};
	for(i=0;i<9*IWIDTH_TXT*IHIGHT_TXT;i++){
		testBMP[i] =  0;
	};

	YsGlWriteStringToRGBA8Bitmap(minlabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 0, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(minlabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 2, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(minlabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 1, 0, YsFont12x16, 14, 16);

	YsGlWriteStringToRGBA8Bitmap(minlabel, 255, 255, 255, 255, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 1, YsFont12x16, 14, 16);

	YsGlWriteStringToRGBA8Bitmap(maxlabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, IHIGHT_TXT, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(maxlabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 2+IHIGHT_TXT, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(maxlabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 1, IHIGHT_TXT, YsFont12x16, 14, 16);
	
	YsGlWriteStringToRGBA8Bitmap(maxlabel, 255, 255, 255, 255, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT, 
								 0, 1+IHIGHT_TXT, YsFont12x16, 14, 16);

	if(cbar_wk->iflag_zero == 1){
		YsGlWriteStringToRGBA8Bitmap(zerolabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 0, 2*IHIGHT_TXT, YsFont12x16, 14, 16);
		YsGlWriteStringToRGBA8Bitmap(zerolabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 0, 2+2*IHIGHT_TXT, YsFont12x16, 14, 16);
		YsGlWriteStringToRGBA8Bitmap(zerolabel, 127, 127, 127, 127, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 1, 2*IHIGHT_TXT, YsFont12x16, 14, 16);

		YsGlWriteStringToRGBA8Bitmap(zerolabel, 255, 255, 255, 255, numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 0, 1+2*IHIGHT_TXT, YsFont12x16, 14, 16);
	};
	for(i=0;i<3*IWIDTH_TXT*IHIGHT_TXT;i++){
		testBMP[3*i  ] = (unsigned char) (0.8 * (float) ((int) numBMP[4*i  ]));
		testBMP[3*i+1] = (unsigned char) (0.2 * (float) ((int) numBMP[4*i+1]));
		testBMP[3*i+2] = (unsigned char) (0.4 * (float) ((int) numBMP[4*i+2]));
	};
	for(i=0;i<3*IWIDTH_TXT*IHIGHT_TXT;i++){
		numBMP[4*i  ] = (unsigned char) (0.8 * (float) ((int) numBMP[4*i  ]));
		numBMP[4*i+1] = (unsigned char) (0.2 * (float) ((int) numBMP[4*i+1]));
		numBMP[4*i+2] = (unsigned char) (0.4 * (float) ((int) numBMP[4*i+2]));
		numBMP[4*i+3] = (unsigned char) (0.8 * (float) ((int) numBMP[4*i+3]));
	};
	
//	pixout_BMP_c("/Users/matsui/Desktop/aho", IWIDTH_TXT, 3*IHIGHT_TXT, testBMP);
	
	glUseProgram(kemo_shaders->simple_texure->programId);
	map_matrix_to_shader(kemo_shaders->simple_texure, orthogonal);
	
	int id_textureImage = glGetUniformLocation(kemo_shaders->simple_texure->programId, "image");
	
	GLuint id_texture[1];
	static const GLfloat blend[] = {1.0,1.0,1.0,1.0};
	/* Preference for resiging texture */
	glBindTexture(GL_TEXTURE_2D , id_texture[0]);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glTexImage2D(GL_TEXTURE_2D , 0 , GL_RGBA , IWIDTH_TXT, 3*IHIGHT_TXT,
				 0 , GL_RGBA , GL_UNSIGNED_BYTE , numBMP);
	//	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
	//	glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, blend);

	
	
	num_patch = 2*(cbar_wk->iflag_zero + ITWO);
	set_buffer_address_4_patch(ITHREE*num_patch, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	colorbar_mbox_to_buf(iflag_retina, text_color, cbar_wk, cbar_buf);
	
	glGenVertexArrays(1, &cbar_VAO->id_VAO);
	glBindVertexArray(cbar_VAO->id_VAO);
	
	glGenBuffers(1, &cbar_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * cbar_buf->num_nod_buf*cbar_buf->ncomp_buf,
				 cbar_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, cbar_buf->istride,
						  (GLvoid*) (cbar_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, cbar_buf->istride, 
						  (GLvoid*) (cbar_buf->ist_tex * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);

	glBindVertexArray(cbar_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	
	glUniform1i(id_textureImage, id_texture[0]);
	glDrawArrays(GL_TRIANGLES, 0, 12);
	
	if(cbar_wk->iflag_zero == 1){
		glDrawArrays(GL_TRIANGLES, 12, 6);
	};
	
	DestroyVBO(cbar_VAO);
	
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);
	
	free(cbar_wk);
	return;
}

