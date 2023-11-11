/*
// m_colorbar_work.c
*/

#include "m_colorbar_work.h"

struct line_text_image * alloc_line_text_image(int npix_x, int npix_y, int len_text)
{
    struct line_text_image *l_txt_img;
    if((l_txt_img = (struct line_text_image *) malloc(sizeof(struct line_text_image))) == NULL){
        printf("malloc error for line_text_image\n");
        exit(0);
    }
    l_txt_img->len_text =  len_text;
    l_txt_img->texts = alloc_string(len_text);

    l_txt_img->npix_img[0] = npix_x;
    l_txt_img->npix_img[1] = npix_y;
    l_txt_img->npixel = l_txt_img->npix_img[0] * l_txt_img->npix_img[1];
    l_txt_img->imgBMP =  (unsigned char *) calloc((4 * l_txt_img->npixel), sizeof(unsigned char));
    if(l_txt_img->imgBMP == NULL){
        printf("malloc error for l_txt_img->imgBMP\n");
        exit(0);
    }
    return l_txt_img;
};

void dealloc_line_text_image(struct line_text_image *l_txt_img)
{
    free(l_txt_img->imgBMP);
    free(l_txt_img->texts);
    free(l_txt_img);
    return;
};

void clear_line_text_image(struct line_text_image *l_txt_img){
    int i;
    for(i=0;i<4*l_txt_img->npixel;i++){l_txt_img->imgBMP[i] =  0;};
    return;
};

void check_line_text_bitmap(struct line_text_image *l_txt_img){
    unsigned char *testBMP = (unsigned char *) calloc((9 * l_txt_img->npixel), sizeof(unsigned char));
    if(testBMP == NULL){
        printf("malloc error for testBMP\n");
        exit(0);
    };
    for(int i=0;i<l_txt_img->npixel;i++){
        testBMP[3*i  ] = (unsigned char) (0.8 * (float) ((int) l_txt_img->imgBMP[4*i  ]));
        testBMP[3*i+1] = (unsigned char) (0.2 * (float) ((int) l_txt_img->imgBMP[4*i+1]));
        testBMP[3*i+2] = (unsigned char) (0.4 * (float) ((int) l_txt_img->imgBMP[4*i+2]));
    };
    pixout_BMP_c("/Users/matsui/Desktop/linetext", l_txt_img->npix_img[0], 3*l_txt_img->npix_img[1], testBMP);
    free(testBMP);
    return;
};

void set_line_text16_image(int icolor_txt, int icolor_mid, struct line_text_image *l_txt_img){
    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 0, 0, YsFont12x16, 14, 16);
    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 0, 2, YsFont12x16, 14, 16);
    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 1, 0, YsFont12x16, 14, 16);

    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_txt, icolor_txt, icolor_txt, icolor_txt,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 0, 1, YsFont12x16, 14, 16);
    return;
};

void set_line_text24_image(int icolor_txt, int icolor_mid, struct line_text_image *l_txt_img){
    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 0, 4, YsFont16x24, 20, 24);
    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 0, 6, YsFont16x24, 20, 24);
    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_mid, icolor_mid, icolor_mid, icolor_mid,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 1, 4, YsFont16x24, 20, 24);

    YsGlWriteStringToRGBA8Bitmap(l_txt_img->texts, icolor_txt, icolor_txt, icolor_txt, icolor_txt,
                                 l_txt_img->imgBMP, l_txt_img->npix_img[0], l_txt_img->npix_img[1],
                                 0, 5, YsFont16x24, 20, 24);
    return;
};

void set_line_msgbox_image(int icolor_txt, int icolor_mid,
                           struct line_text_image *l_txt_img){
    int i;
    /* Draw box in the texture */
    for(i=0;i<2*l_txt_img->npix_img[0];i++){
        l_txt_img->imgBMP[4*i  ] = icolor_txt;
        l_txt_img->imgBMP[4*i+1] = icolor_txt;
        l_txt_img->imgBMP[4*i+2] = icolor_txt;
        l_txt_img->imgBMP[4*i+3] = icolor_txt;
        l_txt_img->imgBMP[4*(l_txt_img->npixel-i-1)  ] = icolor_mid;
        l_txt_img->imgBMP[4*(l_txt_img->npixel-i-1)+1] = icolor_mid;
        l_txt_img->imgBMP[4*(l_txt_img->npixel-i-1)+2] = icolor_mid;
        l_txt_img->imgBMP[4*(l_txt_img->npixel-i-1)+3] = icolor_mid;
    };
    for(i=0;i<l_txt_img->npix_img[1];i++){
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i  ] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i+1] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i+2] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i+3] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i+4] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i+5] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i+6] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*i+7] = icolor_txt;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-4] = icolor_mid;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-3] = icolor_mid;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-2] = icolor_mid;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-1] = icolor_mid;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-8] = icolor_mid;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-7] = icolor_mid;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-6] = icolor_mid;
        l_txt_img->imgBMP[4*l_txt_img->npix_img[0]*(i+1)-5] = icolor_mid;
    };
};

void set_line_text_color(float text_color3[3], struct line_text_image *l_txt_img){
    int i;
    for(i=0;i<l_txt_img->npixel;i++){
        l_txt_img->imgBMP[4*i  ]
            = (unsigned char) (text_color3[0] * (float) ((int) l_txt_img->imgBMP[4*i  ]));
        l_txt_img->imgBMP[4*i+1]
            = (unsigned char) (text_color3[1] * (float) ((int) l_txt_img->imgBMP[4*i+1]));
        l_txt_img->imgBMP[4*i+2]
            = (unsigned char) (text_color3[2] * (float) ((int) l_txt_img->imgBMP[4*i+2]));
    };
    return;
};

static void set_line_text_opacity(float message_opacity, struct line_text_image *l_txt_img){
    int i;
    for(i=0;i<l_txt_img->npixel;i++){
        l_txt_img->imgBMP[4*i+3] = (unsigned char) ((float) 255 * message_opacity);
    };
    return;
};


struct cbar_work * alloc_colorbar_position(void){
	struct cbar_work *cbar_wk = (struct cbar_work *) malloc(sizeof(struct cbar_work));
	if(cbar_wk == NULL){
		printf("malloc error for cbar_work\n");
		exit(0);
	}
    cbar_wk->cbar_min_image =  alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, NCHARA_CBOX);
    cbar_wk->cbar_max_image =  alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, NCHARA_CBOX);
    cbar_wk->cbar_zero_image = alloc_line_text_image(IWIDTH_TXT, IHIGHT_TXT, NCHARA_CBOX);
    return cbar_wk;
};

void dealloc_colorbar_position(struct cbar_work *cbar_wk){
    dealloc_line_text_image(cbar_wk->cbar_min_image);
    dealloc_line_text_image(cbar_wk->cbar_max_image);
    dealloc_line_text_image(cbar_wk->cbar_zero_image);
    free(cbar_wk);
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
                             struct line_text_image *l_txt_img){
    sprintf(l_txt_img->texts, "% 3.2E", value);
    clear_line_text_image(l_txt_img);
    set_line_text24_image(ICOLOR_FULL, ICOLOR_MID, l_txt_img);
    set_line_text_color(text_color3, l_txt_img);
    /* check_line_text_bitmap(l_txt_img) */
    return;
};


struct tlabel_work * alloc_tlabel_work(void){
	struct tlabel_work *tlabel_wk = (struct tlabel_work *) malloc(sizeof(struct tlabel_work));
	if(tlabel_wk == NULL){
		printf("malloc error for tlabel_work\n");
		exit(0);
	}
    tlabel_wk->tlabel_image =  alloc_line_text_image(IWIDTH_TLABEL, IHIGHT_TXT, NCHARA_CBOX);
    return tlabel_wk;
};

void dealloc_tlabel_work(struct tlabel_work *tlabel_wk){
    dealloc_line_text_image(tlabel_wk->tlabel_image);
    free(tlabel_wk);
    return;
};

void clear_time_text_image(struct tlabel_work *tlabel_wk){
    clear_line_text_image(tlabel_wk->tlabel_image);
	return;
};

void set_time_text_image(float text_color3[3], struct tlabel_work *tlabel_wk){
    set_line_text24_image(ICOLOR_FULL, ICOLOR_MID, tlabel_wk->tlabel_image);
    set_line_text_color(text_color3, tlabel_wk->tlabel_image);
    /* check_line_text_bitmap(tlabel_wk->tlabel_image) */
    return;
};



struct msg_work * alloc_message_work(void){
	struct msg_work *msg_wk = (struct msg_work *) malloc(sizeof(struct msg_work));
	if(msg_wk == NULL){
		printf("malloc error for msg_work\n");
		exit(0);
	}
    msg_wk->message_image =  alloc_line_text_image(IWIDTH_MSG, IHIGHT_MSG, NCHARA_MSG);
    msg_wk->message_opacity = 0.0;
    return msg_wk;
};

void dealloc_message_work(struct msg_work *msg_wk){
    dealloc_line_text_image(msg_wk->message_image);
    free(msg_wk);
    return;
};


void set_message_opacity(float opacity, struct msg_work *msg_wk){
    msg_wk->message_opacity = opacity;
    return;
};

void set_message_position(int iflag_retina, int nx_win, int ny_win,
						  struct msg_work *msg_wk){
    msg_wk->xbar_max = 0.05 * ((float) nx_win);
	msg_wk->ybar_min = 0.92 * ((float) ny_win);
	return;
}

void set_windowsize_image(int npixel_x, int npixel_y, struct msg_work *msg_wk){
	int i;
	float text_color3[4];
	
	for(i=0;i<3;i++){text_color3[i] = 1.0;};
    text_color3[3] = 1.0;
	
	sprintf(msg_wk->message_image->texts, " Window size:(%4d,%4d)", npixel_x, npixel_y);
    set_line_msgbox_image(ICOLOR_FULL, ICOLOR_MID, msg_wk->message_image);
    set_line_text24_image(ICOLOR_FULL, ICOLOR_MID, msg_wk->message_image);
    set_line_text_color(text_color3, msg_wk->message_image);
    set_line_text_opacity(msg_wk->message_opacity, msg_wk->message_image);
    /* check_line_text_bitmap(msg_wk->message_image) */
};
