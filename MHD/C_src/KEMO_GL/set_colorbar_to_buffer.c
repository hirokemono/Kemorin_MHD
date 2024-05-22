/*
// set_colorbar_to_buffer.c
*/

#include "set_colorbar_to_buffer.h"


static long set_one_quad_to_buf(long i_quad,
                                float x1[4], float x2[4], float x3[4], float x4[4],
                                float c1[4], float c2[4], float c3[4], float c4[4],
                                struct gl_strided_buffer *strided_buf){
    double xyzw_tri[12];
    double color_tri[12];
    double norm_tri[12] = {0.0, 0.0, 1.0, 1.0,
                           0.0, 0.0, 1.0, 1.0,
                           0.0, 0.0, 1.0, 1.0};
	int nd;
	
    for(nd=0;nd<4;nd++){
        xyzw_tri[  nd] =  x1[nd];
        xyzw_tri[4+nd] =  x2[nd];
        xyzw_tri[8+nd] =  x3[nd];
        color_tri[  nd] = c1[nd];
        color_tri[4+nd] = c2[nd];
        color_tri[8+nd] = c3[nd];
    }
    long ipatch_in = 2*i_quad;
    ipatch_in = set_patch_strided_buffer(ipatch_in, xyzw_tri,
                                         norm_tri, color_tri,
                                         strided_buf);
		
    for(nd=0;nd<4;nd++){
        xyzw_tri[  nd] =  x3[nd];
        xyzw_tri[4+nd] =  x4[nd];
        xyzw_tri[8+nd] =  x1[nd];
        color_tri[  nd] = c3[nd];
        color_tri[4+nd] = c4[nd];
        color_tri[8+nd] = c1[nd];
    }
    ipatch_in = set_patch_strided_buffer(ipatch_in, xyzw_tri,
                                         norm_tri, color_tri,
                                         strided_buf);
	return ipatch_in;
};

static long set_one_texture_to_buf(const long i_quad, 
                                   float t1[2], float t2[2], float t3[2], float t4[2],
                                   struct gl_strided_buffer *strided_buf){
    double xy_txur[12];
	int nd;
	
    long inum_patch = 2 * i_quad;
    for(nd=0;nd<2;nd++){
        xy_txur[   nd] = t1[nd];
        xy_txur[ 2+nd] = t2[nd];
        xy_txur[ 4+nd] = t3[nd];
        xy_txur[ 6+nd] = t3[nd];
        xy_txur[ 8+nd] = t4[nd];
        xy_txur[10+nd] = t1[nd];
    }
    inum_patch = set_patch_textur_to_buf(inum_patch, &xy_txur[0], strided_buf);
    inum_patch = set_patch_textur_to_buf(inum_patch, &xy_txur[6], strided_buf);
	return inum_patch;
};

long solid_colorbar_box_to_buf(const long ist_quad,
                               struct colormap_params *cmap_s, 
                               struct cbar_work *cbar_wk,
                               struct gl_strided_buffer *strided_buf){
	long inum_quad = ist_quad;
	float y1;
	float x1[4], x2[4], x3[4], x4[4];
	float c1[4], c2[4], c3[4], c4[4];
	double psf_value;
	double f_color[4], l_color[4];
	long i, nd;
	
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                           cbar_wk->psf_min, f_color);
    
	f_color[3] = ONE;
	for(nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	
    x1[3] = 1.0;
    x2[3] = 1.0;
    x3[3] = 1.0;
    x4[3] = 1.0;

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
		set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                               psf_value, f_color);

        f_color[3] = ONE;
		
		x1[1] = y1;
		for(nd=0;nd<4;nd++) {c1[nd] = l_color[nd];};
		
		x2[1] = y1;
		for(nd=0;nd<4;nd++) {c2[nd] = l_color[nd];};
		
		x3[1] = y1 + cbar_wk->ydelta;
		for(nd=0;nd<4;nd++) {c3[nd] = f_color[nd];};
		
		x4[1] = y1 + cbar_wk->ydelta;
		for(nd=0;nd<4;nd++) {c4[nd] = f_color[nd];};
		
		set_one_quad_to_buf(i, x1, x2, x3, x4, c1, c2, c3, c4,
                            strided_buf);
		
		for(nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	};
	inum_quad = ist_quad + cbar_wk->num_quad;
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	return inum_quad;
};

long fade_colorbar_box_to_buf(const long ist_quad, 
                              struct colormap_params *cmap_s, float *bg_color, 
                              struct cbar_work *cbar_wk, 
                              struct gl_strided_buffer *strided_buf){
	long inum_quad = ist_quad;
	float y1;
	float x1[4], x2[4], x3[4], x4[4];
	float c1[4], c2[4], c3[4], c4[4];
	
	double psf_value;
	double f_color[4], l_color[4];
	long i, nd;
	
	
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                           cbar_wk->psf_min, f_color);

    for (nd=0; nd<3; nd++) {
		f_color[nd] = f_color[nd] * f_color[3]
				+ bg_color[nd] * (ONE - f_color[3]);
	};
	f_color[3] = ONE;
	for (nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	
    x1[3] = 1.0;
    x2[3] = 1.0;
    x3[3] = 1.0;
    x4[3] = 1.0;

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
		set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                               psf_value, f_color);
	
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
		
		set_one_quad_to_buf(ist_quad+i, x1, x2, x3, x4, c1, c2, c3, c4,
                            strided_buf);
		
		for (nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	};
	inum_quad = ist_quad + cbar_wk->num_quad;

    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	return inum_quad;
};

long colorbar_frame_to_buf(const long ist_quad, int iflag_retina, 
                           float *text_color, struct cbar_work *cbar_wk,
                           struct gl_strided_buffer *strided_buf){
	float x1[4], x2[4], x3[4], x4[4];
	float c1[4], c2[4], c3[4], c4[4];
	int nd;
	
	x1[2] = 0.001;
	x2[2] = 0.001;
	x3[2] = 0.001;
	x4[2] = 0.001;
    x1[3] = 1.0;
    x2[3] = 1.0;
    x3[3] = 1.0;
    x4[3] = 1.0;
	
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
	set_one_quad_to_buf(ist_quad, x1, x2, x3, x4, c1, c2, c3, c4,
                        strided_buf);
	
	
	x1[0] = cbar_wk->xbar_max;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x2[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x3[0] = cbar_wk->xbar_max + iflag_retina + 1;
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	
	x4[0] = cbar_wk->xbar_max;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	set_one_quad_to_buf(ist_quad+1, x1, x2, x3, x4, c1, c2, c3, c4,
                        strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_min;
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_min;
	set_one_quad_to_buf(ist_quad+2, x1, x2, x3, x4, c1, c2, c3, c4,
                        strided_buf);
	
	
	x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x1[1] = cbar_wk->ybar_max;
	
	x2[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x2[1] = cbar_wk->ybar_max;
	
	x3[0] = cbar_wk->xbar_max + 3.0*(iflag_retina + 1);
	x3[1] = cbar_wk->ybar_max + iflag_retina + 1;
	
	x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
	x4[1] = cbar_wk->ybar_max + iflag_retina + 1;
	set_one_quad_to_buf(ist_quad+3, x1, x2, x3, x4, c1, c2, c3, c4,
                        strided_buf);
	
	if(cbar_wk->iflag_zero == 1){
		x1[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x1[1] = cbar_wk->yline_zero - iflag_retina - 1;
		
		x2[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x2[1] = cbar_wk->yline_zero - iflag_retina - 1;
		
		x3[0] = cbar_wk->xbar_max + 6.0*(iflag_retina + 1);
		x3[1] = cbar_wk->yline_zero;
		
		x4[0] = cbar_wk->xbar_min - iflag_retina - 1;
		x4[1] = cbar_wk->yline_zero;
		set_one_quad_to_buf(ist_quad+4, x1, x2, x3, x4, c1, c2, c3, c4,
                            strided_buf);
	};
	return (ist_quad+4);
};

void colorbar_mbox_to_buf(int iflag_retina, float *text_color,
                          struct cbar_work *cbar_wk,
                          struct gl_strided_buffer *min_vertex,
                          struct gl_strided_buffer *max_vertex,
                          struct gl_strided_buffer *zero_vertex){
	float x1[4], x2[4], x3[4], x4[4];
	float c1[4], c2[4], c3[4], c4[4];
	float t1[2], t2[2], t3[2], t4[2];
	int nd;
	
    x1[3] = 1.0;
    x2[3] = 1.0;
    x3[3] = 1.0;
    x4[3] = 1.0;

    x1[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
	x2[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
	x3[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
	x4[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
	x1[2] = 0.001;
	x2[2] = 0.001;
	x3[2] = 0.001;
	x4[2] = 0.001;
	for(nd=0;nd<4;nd++) {c1[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c2[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c3[nd] = text_color[nd];};
	for(nd=0;nd<4;nd++) {c4[nd] = text_color[nd];};
	
	x1[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
	x2[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
	x3[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
	x4[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
	x1[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x2[1] = cbar_wk->ybar_min - iflag_retina - 1;
	x3[1] = cbar_wk->ybar_min + 16.0 * (iflag_retina + 1);
	x4[1] = cbar_wk->ybar_min + 16.0 * (iflag_retina + 1);
	t1[0] = 0.0;
	t2[0] = 1.0;
	t3[0] = 1.0;
	t4[0] = 0.0;
	t1[1] = 0.0;
	t2[1] = 0.0;
	t3[1] = 1.0;
	t4[1] = 1.0;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4,
                        min_vertex);
	set_one_texture_to_buf(0, t1, t2, t3, t4,
                           min_vertex);
	
	
	x1[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
	x2[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
	x3[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
	x4[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
	x1[1] = cbar_wk->ybar_max - iflag_retina - 1;
	x2[1] = cbar_wk->ybar_max - iflag_retina - 1;
	x3[1] = cbar_wk->ybar_max + 16.0 * (iflag_retina + 1);
	x4[1] = cbar_wk->ybar_max + 16.0 * (iflag_retina + 1);
	t1[0] = 0.0;
	t2[0] = 1.0;
	t3[0] = 1.0;
	t4[0] = 0.0;
	t1[1] = 0.0;
	t2[1] = 0.0;
	t3[1] = 1.0;
	t4[1] = 1.0;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4,
                        max_vertex);
	set_one_texture_to_buf(0, t1, t2, t3, t4,
                           max_vertex);
	
	if(cbar_wk->iflag_zero == 1){
        x1[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
        x2[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
        x3[0] = cbar_wk->xbar_max + 104.0*(iflag_retina + 1);
        x4[0] = cbar_wk->xbar_max +   8.0*(iflag_retina + 1);
		x1[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x2[1] = cbar_wk->yline_zero - iflag_retina - 1;
		x3[1] = cbar_wk->yline_zero + 16.0 * (iflag_retina + 1);
		x4[1] = cbar_wk->yline_zero + 16.0 * (iflag_retina + 1);
        t1[0] = 0.0;
        t2[0] = 1.0;
        t3[0] = 1.0;
        t4[0] = 0.0;
		t1[1] = 0.0;
		t2[1] = 0.0;
		t3[1] = 1.0;
		t4[1] = 1.0;
		set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4,
                            zero_vertex);
		set_one_texture_to_buf(0, t1, t2, t3, t4,
                               zero_vertex);
	};
	return;
};

void time_mbox_to_buf(int iflag_retina, float *text_color,
                      float xwin, float ywin,
					  struct gl_strided_buffer *strided_buf){
	float x1[4], x2[4], x3[4], x4[4];
	float c1[4], c2[4], c3[4], c4[4];
	float t1[2], t2[2], t3[2], t4[2];
	int nd;
	
    x1[3] = 1.0;
    x2[3] = 1.0;
    x3[3] = 1.0;
    x4[3] = 1.0;
	x1[0] = xwin - 188.0*(iflag_retina + 1);
	x2[0] = xwin -   8.0*(iflag_retina + 1);
	x3[0] = xwin -   8.0*(iflag_retina + 1);
	x4[0] = xwin - 188.0*(iflag_retina + 1);
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
	
	x1[1] = ywin - 32.0 * (iflag_retina + 1);
	x2[1] = ywin - 32.0 * (iflag_retina + 1);
	x3[1] = ywin - 16.0 * (iflag_retina + 1);
	x4[1] = ywin - 16.0 * (iflag_retina + 1);
	t1[1] = 0.0;
	t2[1] = 0.0;
	t3[1] = 1.0;
	t4[1] = 1.0;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c2, c3, c4,
                        strided_buf);
	set_one_texture_to_buf(0, t1, t2, t3, t4,
                           strided_buf);
	return;
};

void message_mbox_to_buf(const int iflag_retina, const float text_opacity,
                         const float xbar_max, const float ybar_min,
                         struct gl_strided_buffer *strided_buf){
	float c1[4];
	float x1[4], x2[4], x3[4], x4[4];
	float t1[2], t2[2], t3[2], t4[2];
	int nd;
	
	x1[0] = xbar_max;
	x2[0] = xbar_max + 320.0*(iflag_retina + 1);
	x3[0] = xbar_max + 320.0*(iflag_retina + 1);
	x4[0] = xbar_max;
    x1[1] = ybar_min - iflag_retina - 1;
    x2[1] = ybar_min - iflag_retina - 1;
    x3[1] = ybar_min + 24.0 * (iflag_retina + 1);
    x4[1] = ybar_min + 24.0 * (iflag_retina + 1);
	x1[2] = 0.001;
	x2[2] = 0.001;
	x3[2] = 0.001;
	x4[2] = 0.001;
    x1[3] = 1.0;
    x2[3] = 1.0;
    x3[3] = 1.0;
    x4[3] = 1.0;
	
	c1[3] = text_opacity;
	for(nd=0;nd<4;nd++) {c1[nd] = 0.0;};
	
	t1[0] = 0.0;
	t2[0] = 1.0;
	t3[0] = 1.0;
	t4[0] = 0.0;
	
	t1[1] = 0.0;
	t2[1] = 0.0;
	t3[1] = 1.0;
	t4[1] = 1.0;
	set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c1, c1, c1,
                        strided_buf);
	set_one_texture_to_buf(0, t1, t2, t3, t4,
                           strided_buf);
	return;
};


void screen_mbox_to_buf(const int npix_x, const int npix_y,
                        struct gl_strided_buffer *strided_buf){
    float c1[4];
    float x1[4], x2[4], x3[4], x4[4];
    float t1[2], t2[2], t3[2], t4[2];
    int nd;
    
    x1[0] = 0.0;
    x2[0] = (float) npix_x;
    x3[0] = (float) npix_x;
    x4[0] = 0.0;
    x1[1] = 0.0;
    x2[1] = 0.0;
    x3[1] = (float) npix_y;
    x4[1] = (float) npix_y;
    x1[2] = 0.0;
    x2[2] = 0.0;
    x3[2] = 0.0;
    x4[2] = 0.0;
    x1[3] = 1.0;
    x2[3] = 1.0;
    x3[3] = 1.0;
    x4[3] = 1.0;
    
    c1[3] = 1.0;
    for(nd=0;nd<4;nd++) {c1[nd] = 0.0;};
    
    t1[0] = 0.0;
    t2[0] = 1.0;
    t3[0] = 1.0;
    t4[0] = 0.0;
    
    t1[1] = 0.0;
    t2[1] = 0.0;
    t3[1] = 1.0;
    t4[1] = 1.0;
    set_one_quad_to_buf(0, x1, x2, x3, x4, c1, c1, c1, c1,
                        strided_buf);
    set_one_texture_to_buf(0, t1, t2, t3, t4,
                           strided_buf);
    return;
};

