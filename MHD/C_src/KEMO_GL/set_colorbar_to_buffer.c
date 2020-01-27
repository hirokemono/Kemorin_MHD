/*
// set_colorbar_to_buffer.c
*/

#include "set_colorbar_to_buffer.h"


static void set_one_quad_to_buf(int i_quad, 
			float x1[3], float x2[3], float x3[3], float x4[3], 
			float c1[4], float c2[4], float c3[4], float c4[4], 
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

static void set_one_texture_to_buf(int i_quad, 
			float t1[2], float t2[2], float t3[2], float t4[2], 
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

int solid_colorbar_box_to_buf(int ist_quad, struct colormap_params *cmap_s, 
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	int inum_quad = ist_quad;
	float y1;
	float x1[3], x2[3], x3[3], x4[3];
	float c1[4], c2[4], c3[4], c4[4];
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
		y1 = cbar_wk->ybar_min + cbar_wk->ydelta * (float) i;
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
	inum_quad = ist_quad + cbar_wk->num_quad;
	return inum_quad;
};

int fade_colorbar_box_to_buf(int ist_quad, struct colormap_params *cmap_s, float *bg_color, 
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	int inum_quad = ist_quad;
	float y1;
	float x1[3], x2[3], x3[3], x4[3];
	float c1[4], c2[4], c3[4], c4[4];
	
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
		y1 = cbar_wk->ybar_min + cbar_wk->ydelta * (float)i;
		
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
		
		set_one_quad_to_buf(ist_quad+i, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
		
		for (nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	};
	inum_quad = ist_quad + cbar_wk->num_quad;
	return inum_quad;
};

int colorbar_frame_to_buf(int ist_quad, int iflag_retina, float *text_color,
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	float x1[3], x2[3], x3[3], x4[3];
	float c1[4], c2[4], c3[4], c4[4];
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
	set_one_quad_to_buf(ist_quad, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_max;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x2[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x3[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	
	x4[0] = cbar_wk->xbar_max;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	set_one_quad_to_buf(ist_quad+1, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_min;
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_min;
	set_one_quad_to_buf(ist_quad+2, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_max;
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_max;
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	set_one_quad_to_buf(ist_quad+3, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	
	if(cbar_wk->iflag_zero == 1){
		x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x1[1] = cbar_wk->yline_zero - iflag_retina - 1;
		
		x2[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x2[1] = cbar_wk->yline_zero - iflag_retina - 1;
		
		x3[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x3[1] = cbar_wk->yline_zero;
		
		x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x4[1] = cbar_wk->yline_zero;
		set_one_quad_to_buf(ist_quad+4, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	};
	return (ist_quad+4);
};

void colorbar_mbox_to_buf(int iflag_retina, float *text_color,
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf){
	float x1[3], x2[3], x3[3], x4[3];
	float c1[4], c2[4], c3[4], c4[4];
	float t1[2], t2[2], t3[2], t4[2];
	int nd;
	
	x1[0] = cbar_wk->xbar_max + 8.0*(iflag_retina + 1);
	x2[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
	x3[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
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
	x3[1] = cbar_wk->ybar_min + 16.0 * (iflag_retina + 1);
	x4[1] = cbar_wk->ybar_min + 16.0 * (iflag_retina + 1);
	t1[1] = 0.0;
	t2[1] = 0.0;
	t3[1] = 1.0 / 3.0;
	t4[1] = 1.0 / 3.0;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	set_one_texture_to_buf(0, t1, t2, t3, t4, strided_buf);
	
	
	x1[1] = cbar_wk->ybar_max - iflag_retina - 1;
	x2[1] = cbar_wk->ybar_max - iflag_retina - 1;
	x3[1] = cbar_wk->ybar_max + 16.0 * (iflag_retina + 1);
	x4[1] = cbar_wk->ybar_max + 16.0 * (iflag_retina + 1);
	t1[1] = 1.0 / 3.0;
	t2[1] = 1.0 / 3.0;
	t3[1] = 2.0 / 3.0;
	t4[1] = 2.0 / 3.0;
	set_one_quad_to_buf(1, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	set_one_texture_to_buf(1, t1, t2, t3, t4, strided_buf);
	
	if(cbar_wk->iflag_zero == 1){
		x1[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x2[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x3[1] = cbar_wk->yline_zero + 16.0 * (iflag_retina + 1);
		x4[1] = cbar_wk->yline_zero + 16.0 * (iflag_retina + 1);
		t1[1] = 2.0 / 3.0;
		t2[1] = 2.0 / 3.0;
		t3[1] = 1.0;
		t4[1] = 1.0;
		set_one_quad_to_buf(2, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
		set_one_texture_to_buf(2, t1, t2, t3, t4, strided_buf);
	};
	return;
};

void message_mbox_to_buf(int iflag_retina, float *text_color,
			struct msg_work *msg_wk, struct gl_strided_buffer *strided_buf){
	float x1[3], x2[3], x3[3], x4[3];
	float c1[4], c2[4], c3[4], c4[4];
	float t1[2], t2[2], t3[2], t4[2];
	int nd;
	
	x1[0] = msg_wk->xbar_max;
	x2[0] = msg_wk->xbar_max + 320.0*(iflag_retina + 1);
	x3[0] = msg_wk->xbar_max + 320.0*(iflag_retina + 1);
	x4[0] = msg_wk->xbar_max;
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
	
	x1[1] = msg_wk->ybar_min - iflag_retina - 1;
	x2[1] = msg_wk->ybar_min - iflag_retina - 1;
	x3[1] = msg_wk->ybar_min + 24.0 * (iflag_retina + 1);
	x4[1] = msg_wk->ybar_min + 24.0 * (iflag_retina + 1);
	t1[1] = 0.0;
	t2[1] = 0.0;
	t3[1] = 1.0;
	t4[1] = 1.0;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4, strided_buf);
	set_one_texture_to_buf(0, t1, t2, t3, t4, strided_buf);
	
	return;
};

