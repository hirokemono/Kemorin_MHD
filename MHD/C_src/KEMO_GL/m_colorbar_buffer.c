/*
// m_colorbar_buffer.c
*/

#include "m_colorbar_buffer.h"

void check_line_text_bitmap(struct gl_texure_image *image){
    int i;
    unsigned char *testBMP = (unsigned char *) calloc((9 * image->texure_npix), sizeof(unsigned char));
    if(testBMP == NULL){
        printf("malloc error for testBMP\n");
        exit(0);
    };
    for(i=0;i<image->texure_npix;i++){
        testBMP[3*i  ] = (unsigned char) (0.8 * (float) ((int) image->texure_rgba[4*i  ]));
        testBMP[3*i+1] = (unsigned char) (0.2 * (float) ((int) image->texure_rgba[4*i+1]));
        testBMP[3*i+2] = (unsigned char) (0.4 * (float) ((int) image->texure_rgba[4*i+2]));
    };
    pixout_BMP_c("/Users/matsui/Desktop/linetext", image->nipxel_xy[0], image->nipxel_xy[1], testBMP);
    free(testBMP);
    return;
};

static void set_line_text16_image(int icolor_txt, int icolor_mid, char *texts,
                                  struct gl_texure_image *image){
    YsGlWriteStringToRGBA8Bitmap(texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 0, 0, YsFont12x16, 14, 16);
    YsGlWriteStringToRGBA8Bitmap(texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 0, 2, YsFont12x16, 14, 16);
    YsGlWriteStringToRGBA8Bitmap(texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 1, 0, YsFont12x16, 14, 16);

    YsGlWriteStringToRGBA8Bitmap(texts, icolor_txt, icolor_txt, icolor_txt, icolor_txt,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 0, 1, YsFont12x16, 14, 16);
    return;
};

static void set_line_text24_image(int icolor_txt, int icolor_mid, char *texts,
                                  struct gl_texure_image *image){
    YsGlWriteStringToRGBA8Bitmap(texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 0, 4, YsFont16x24, 20, 24);
    YsGlWriteStringToRGBA8Bitmap(texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 0, 6, YsFont16x24, 20, 24);
    YsGlWriteStringToRGBA8Bitmap(texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 1, 4, YsFont16x24, 20, 24);

    YsGlWriteStringToRGBA8Bitmap(texts, icolor_txt, icolor_txt, icolor_txt, icolor_txt,
                                 image->texure_rgba, image->nipxel_xy[0], image->nipxel_xy[1],
                                 0, 5, YsFont16x24, 20, 24);
    return;
};

static void set_line_msgbox_image(int icolor_txt, int icolor_mid,
                                  struct gl_texure_image *image){
    int i;
    /* Draw box in the texture */
    for(i=0;i<2*image->nipxel_xy[0];i++){
        image->texure_rgba[4*i  ] = icolor_txt;
        image->texure_rgba[4*i+1] = icolor_txt;
        image->texure_rgba[4*i+2] = icolor_txt;
        image->texure_rgba[4*i+3] = icolor_txt;
        image->texure_rgba[4*(image->texure_npix-i-1)  ] = icolor_mid;
        image->texure_rgba[4*(image->texure_npix-i-1)+1] = icolor_mid;
        image->texure_rgba[4*(image->texure_npix-i-1)+2] = icolor_mid;
        image->texure_rgba[4*(image->texure_npix-i-1)+3] = icolor_mid;
    };
    for(i=0;i<image->nipxel_xy[1];i++){
        image->texure_rgba[4*image->nipxel_xy[0]*i  ] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*i+1] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*i+2] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*i+3] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*i+4] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*i+5] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*i+6] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*i+7] = icolor_txt;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-4] = icolor_mid;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-3] = icolor_mid;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-2] = icolor_mid;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-1] = icolor_mid;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-8] = icolor_mid;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-7] = icolor_mid;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-6] = icolor_mid;
        image->texure_rgba[4*image->nipxel_xy[0]*(i+1)-5] = icolor_mid;
    };
    return;
};

void set_line_text_color(const float text_color3[3], struct gl_texure_image *image){
    int i;
    for(i=0;i<image->texure_npix;i++){
        image->texure_rgba[4*i  ]
            = (unsigned char) (text_color3[0] * (float) ((int) image->texure_rgba[4*i  ]));
        image->texure_rgba[4*i+1]
            = (unsigned char) (text_color3[1] * (float) ((int) image->texure_rgba[4*i+1]));
        image->texure_rgba[4*i+2]
            = (unsigned char) (text_color3[2] * (float) ((int) image->texure_rgba[4*i+2]));
    };
    return;
};

static void set_line_text_opacity(const float text_opacity, struct gl_texure_image *image){
    int i;
    for(i=0;i<image->texure_npix;i++){
        image->texure_rgba[4*i+3] = (unsigned char) ((float) 255 * text_opacity);
    };
    return;
};


void set_colorbar_position(int iflag_retina, int nx_win, int ny_win,
						   struct colormap_params *cmap_s, struct cbar_work *cbar_wk){
	int num;
	double d1, v1, d2, v2;

	cbar_wk->num_quad = 64;
	
    if(nx_win >= 640*(iflag_retina+1) ){
        cbar_wk->xbar_max = ((float) nx_win) * 0.875;
    } else {
        cbar_wk->xbar_max = ((float) nx_win) - (iflag_retina+1) * 80;
    }
	cbar_wk->xbar_min = cbar_wk->xbar_max - 0.025 * ((float) nx_win);
	cbar_wk->xbar_mid = (cbar_wk->xbar_min + cbar_wk->xbar_max) * 0.5;
	cbar_wk->ybar_min = 0.05 * ((float) ny_win);
	cbar_wk->ybar_max = 0.25 * ((float) ny_win);
	cbar_wk->ydelta =  (cbar_wk->ybar_max - cbar_wk->ybar_min) / ((float) cbar_wk->num_quad);
	
	cbar_wk->iflag_zero = 0;
	
	num = count_real2_clist(cmap_s->colormap);
	set_from_real2_clist_at_index(0,     cmap_s->colormap, &d1, &v1);
	set_from_real2_clist_at_index(num-1, cmap_s->colormap, &d2, &v2);

	cbar_wk->psf_min = d1;
	cbar_wk->psf_max = d2;
	if( (cbar_wk->psf_min*cbar_wk->psf_max) < ZERO ) cbar_wk->iflag_zero = 1;
	
	cbar_wk->yline_zero = cbar_wk->ybar_min 
		+ (cbar_wk->ybar_max - cbar_wk->ybar_min) * (-cbar_wk->psf_min) 
		/ (cbar_wk->psf_max - cbar_wk->psf_min);
	
	return;
}

void set_colorbar_text_image(float text_color3[3], float value,
                             struct gl_textbox_buffer *l_txt_img){
    sprintf(l_txt_img->texts, "% 3.2E", value);
    clear_kemoview_gl_texure(l_txt_img->image);
    set_line_text16_image(ICOLOR_FULL, ICOLOR_MID, l_txt_img->texts, l_txt_img->image);
    set_line_text_color(text_color3, l_txt_img->image);
    /* check_line_text_bitmap(l_txt_img->image) */
    return;
};

void set_time_text_image(float text_color3[3], struct gl_textbox_buffer *timelabel_buf){
    set_line_text16_image(ICOLOR_FULL, ICOLOR_MID, timelabel_buf->texts, timelabel_buf->image);
    set_line_text_color(text_color3, timelabel_buf->image);
    /* check_line_text_bitmap(timelabel_buf->image) */
    return;
};

float message_xmax(const int nx_win){return 0.05 * ((float) nx_win);};
float message_ymin(const int ny_win){return 0.92 * ((float) ny_win);};

void set_windowsize_image(const int npixel_x, const int npixel_y,
                          struct gl_textbox_buffer *message_buf){
	int i;
	float text_color3[4];
	
	for(i=0;i<3;i++){text_color3[i] = 1.0;};
    text_color3[3] = 1.0;
	
	sprintf(message_buf->texts, " Window size:(%4d,%4d)", npixel_x, npixel_y);
    set_line_msgbox_image(ICOLOR_FULL, ICOLOR_MID, message_buf->image);
    set_line_text24_image(ICOLOR_FULL, ICOLOR_MID, message_buf->texts, message_buf->image);
    set_line_text_color(text_color3, message_buf->image);
    set_line_text_opacity(message_buf->text_opacity, message_buf->image);
    /* check_line_text_bitmap(message_buf->image) */
};
