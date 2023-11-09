/*
// m_colorbar_work.c
*/

#include "m_colorbar_work.h"


struct cbar_work * alloc_colorbar_position(void){
	struct cbar_work *cbar_wk = (struct cbar_work *) malloc(sizeof(struct cbar_work));
	if(cbar_wk == NULL){
		printf("malloc error for cbar_work\n");
		exit(0);
	}
    
    cbar_wk->npix_x = IWIDTH_TXT;
    cbar_wk->npix_y = IHIGHT_TXT;
    cbar_wk->npixel = cbar_wk->npix_x * cbar_wk->npix_y;
    cbar_wk->minBMP =  (unsigned char *) calloc((4 * cbar_wk->npixel), sizeof(unsigned char));
    cbar_wk->maxBMP =  (unsigned char *) calloc((4 * cbar_wk->npixel), sizeof(unsigned char));
    cbar_wk->zeroBMP = (unsigned char *) calloc((4 * cbar_wk->npixel), sizeof(unsigned char));
/*
    cbar_wk->testBMP = (unsigned char *) calloc((9 * cbar_wk->npixel), sizeof(unsigned char));
 */
	return cbar_wk;
};

void dealloc_colorbar_position(struct cbar_work *cbar_wk){
    free(cbar_wk->minBMP);
    free(cbar_wk->maxBMP);
    free(cbar_wk->zeroBMP);
/*    free(cbar_wk->testBMP); */
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

void clear_colorbar_text_image(struct cbar_work *cbar_wk){
	int i;
	
	for(i=0;i<4*cbar_wk->npixel;i++){
		cbar_wk->minBMP[i] =  0;
		cbar_wk->maxBMP[i] =  0;
		cbar_wk->zeroBMP[i] = 0;
	};
/*
	for(i=0;i<9*cbar_wk->npixel;i++){
		cbar_wk->testBMP[i] =  0;
	};
*/
	return;
};

void set_colorbar_text_image(float text_color3[3], struct cbar_work *cbar_wk){
	int i;
	
	sprintf(cbar_wk->minlabel, "% 3.2E",cbar_wk->psf_min);
	sprintf(cbar_wk->maxlabel, "% 3.2E",cbar_wk->psf_max);
	sprintf(cbar_wk->zerolabel,"% 3.2E",ZERO);
	
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 191, 191, 191, 191, 
                                 cbar_wk->minBMP, cbar_wk->npix_x, cbar_wk->npix_y,
								 0, 0, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 191, 191, 191, 191, 
                                 cbar_wk->minBMP, cbar_wk->npix_x, cbar_wk->npix_y,
								 0, 2, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 191, 191, 191, 191, 
                                 cbar_wk->minBMP, cbar_wk->npix_x, cbar_wk->npix_y,
								 1, 0, YsFont12x16, 14, 16);

	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 255, 255, 255, 255, 
                                 cbar_wk->minBMP, cbar_wk->npix_x, cbar_wk->npix_y,
								 0, 1, YsFont12x16, 14, 16);

	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 191, 191, 191, 191, 
                                 cbar_wk->maxBMP, cbar_wk->npix_x, cbar_wk->npix_y,
								 0, 0, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 191, 191, 191, 191, 
                                 cbar_wk->maxBMP, cbar_wk->npix_x, cbar_wk->npix_y,
								 0, 2, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 191, 191, 191, 191, 
                                 cbar_wk->maxBMP, cbar_wk->npix_x, cbar_wk->npix_y,
								 1, 0, YsFont12x16, 14, 16);
	
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 255, 255, 255, 255, 
                                 cbar_wk->maxBMP, cbar_wk->npix_x, cbar_wk->npix_y, 
								 0, 1, YsFont12x16, 14, 16);

	if(cbar_wk->iflag_zero == 1){
		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 191, 191, 191, 191, 
                                     cbar_wk->zeroBMP, cbar_wk->npix_x, cbar_wk->npix_y,
									 0, 0, YsFont12x16, 14, 16);
		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 191, 191, 191, 191, 
                                     cbar_wk->zeroBMP, cbar_wk->npix_x, cbar_wk->npix_y,
									 0, 2, YsFont12x16, 14, 16);
		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 191, 191, 191, 191, 
                                     cbar_wk->zeroBMP, cbar_wk->npix_x, cbar_wk->npix_y,
									 1, 0, YsFont12x16, 14, 16);
	
		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 255, 255, 255, 255, 
                                     cbar_wk->zeroBMP, cbar_wk->npix_x, cbar_wk->npix_y,
									 0, 1, YsFont12x16, 14, 16);
	};
	
/*
	for(i=0;i<3*cbar_wk->npixel;i++){
		cbar_wk->testBMP[3*i  ] = (unsigned char) (0.8 * (float) ((int) cbar_wk->numBMP[4*i  ]));
		cbar_wk->testBMP[3*i+1] = (unsigned char) (0.2 * (float) ((int) cbar_wk->numBMP[4*i+1]));
		cbar_wk->testBMP[3*i+2] = (unsigned char) (0.4 * (float) ((int) cbar_wk->numBMP[4*i+2]));
	};
	pixout_BMP_c("/Users/matsui/Desktop/aho", cbar_wk->npix_x, 3*cbar_wk->npix_y, testBMP);
*/
	
	for(i=0;i<cbar_wk->npixel;i++){
		cbar_wk->minBMP[4*i  ] =  (unsigned char) (text_color3[0] * (float) ((int) cbar_wk->minBMP[4*i  ]));
		cbar_wk->minBMP[4*i+1] =  (unsigned char) (text_color3[1] * (float) ((int) cbar_wk->minBMP[4*i+1]));
		cbar_wk->minBMP[4*i+2] =  (unsigned char) (text_color3[2] * (float) ((int) cbar_wk->minBMP[4*i+2]));

		cbar_wk->maxBMP[4*i  ] =  (unsigned char) (text_color3[0] * (float) ((int) cbar_wk->maxBMP[4*i  ]));
		cbar_wk->maxBMP[4*i+1] =  (unsigned char) (text_color3[1] * (float) ((int) cbar_wk->maxBMP[4*i+1]));
		cbar_wk->maxBMP[4*i+2] =  (unsigned char) (text_color3[2] * (float) ((int) cbar_wk->maxBMP[4*i+2]));

		cbar_wk->zeroBMP[4*i  ] = (unsigned char) (text_color3[0] * (float) ((int) cbar_wk->zeroBMP[4*i  ]));
		cbar_wk->zeroBMP[4*i+1] = (unsigned char) (text_color3[1] * (float) ((int) cbar_wk->zeroBMP[4*i+1]));
		cbar_wk->zeroBMP[4*i+2] = (unsigned char) (text_color3[2] * (float) ((int) cbar_wk->zeroBMP[4*i+2]));
	};
	return;
};


struct tlabel_work * alloc_tlabel_work(void){
	struct tlabel_work *tlabel_wk = (struct tlabel_work *) malloc(sizeof(struct tlabel_work));
	if(tlabel_wk == NULL){
		printf("malloc error for tlabel_work\n");
		exit(0);
	}
    
    tlabel_wk->npix_x = IWIDTH_TLABEL;
    tlabel_wk->npix_y = IHIGHT_TXT;
    tlabel_wk->npixel = tlabel_wk->npix_x * tlabel_wk->npix_y;
    tlabel_wk->numBMP =  (unsigned char *) calloc((4 * tlabel_wk->npixel), sizeof(unsigned char));
/*
    tlabel_wk->testBMP = (unsigned char *) calloc((3 * tlabel_wk->npixel), sizeof(unsigned char));
*/
	return tlabel_wk;
};

void dealloc_tlabel_work(struct tlabel_work *tlabel_wk){
/*    free(tlabel_wk->testBMP); */
    free(tlabel_wk->numBMP);
    free(tlabel_wk);
    return;
};

void clear_time_text_image(struct tlabel_work *tlabel_wk){
	int i;
	
	for(i=0;i<4*tlabel_wk->npixel;i++){
		tlabel_wk->numBMP[i] =  0;
	};
/*
	for(i=0;i<3*tlabel_wk->npixel;i++){
		tlabel_wk->testBMP[i] =  0;
	};
*/
	return;
};

void set_time_text_image(float text_color3[3], struct tlabel_work *tlabel_wk){
	int i;
	
	YsGlWriteStringToRGBA8Bitmap(tlabel_wk->minlabel, 191, 191, 191, 191, 
                                 tlabel_wk->numBMP, tlabel_wk->npix_x, tlabel_wk->npix_y,
								 0, 0, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(tlabel_wk->minlabel, 191, 191, 191, 191, 
                                 tlabel_wk->numBMP, tlabel_wk->npix_x, tlabel_wk->npix_y,
								 0, 2, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(tlabel_wk->minlabel, 191, 191, 191, 191, 
                                 tlabel_wk->numBMP, tlabel_wk->npix_x, tlabel_wk->npix_y,
								 1, 0, YsFont12x16, 14, 16);

	YsGlWriteStringToRGBA8Bitmap(tlabel_wk->minlabel, 255, 255, 255, 255, 
                                 tlabel_wk->numBMP, tlabel_wk->npix_x, tlabel_wk->npix_y,
								 0, 1, YsFont12x16, 14, 16);
	
/*
	for(i=0;i<tlabel_wk->npixel;i++){
		tlabel_wk->testBMP[3*i  ] = (unsigned char) (0.8 * (float) ((int) tlabel_wk->numBMP[4*i  ]));
		tlabel_wk->testBMP[3*i+1] = (unsigned char) (0.2 * (float) ((int) tlabel_wk->numBMP[4*i+1]));
		tlabel_wk->testBMP[3*i+2] = (unsigned char) (0.4 * (float) ((int) tlabel_wk->numBMP[4*i+2]));
	};
	pixout_BMP_c("/Users/matsui/Desktop/aho", tlabel_wk->npix_x, 3*tlabel_wk->npix_y, testBMP);
*/
	
	for(i=0;i<tlabel_wk->npixel;i++){
		tlabel_wk->numBMP[4*i  ] = (unsigned char) (text_color3[0] * (float) ((int) tlabel_wk->numBMP[4*i  ]));
		tlabel_wk->numBMP[4*i+1] = (unsigned char) (text_color3[1] * (float) ((int) tlabel_wk->numBMP[4*i+1]));
		tlabel_wk->numBMP[4*i+2] = (unsigned char) (text_color3[2] * (float) ((int) tlabel_wk->numBMP[4*i+2]));
	};
	return;
};



struct msg_work * alloc_message_work(void){
	struct msg_work *msg_wk = (struct msg_work *) malloc(sizeof(struct msg_work));
	if(msg_wk == NULL){
		printf("malloc error for msg_work\n");
		exit(0);
	}

    msg_wk->message_opacity = 0.0;
    msg_wk->npix_x = IWIDTH_MSG;
    msg_wk->npix_y = IHIGHT_MSG;
    msg_wk->npixel = msg_wk->npix_x * msg_wk->npix_y;
    msg_wk->msgBMP =  (unsigned char *) calloc((4 * msg_wk->npixel), sizeof(unsigned char));
/*
    msg_wk->testBMP = (unsigned char *) calloc((3 * msg_wk->npixel), sizeof(unsigned char));
*/
    return msg_wk;
};

void dealloc_message_work(struct msg_work *msg_wk){
/*    free(msg_wk->testBMP); */

    free(msg_wk->msgBMP);
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

void clear_message_text_image(struct msg_work *msg_wk){
	int i;
	int icolor_mid[3];
	
	for(i=0;i<3;i++){icolor_mid[i] = 0;};
	
	for(i=0;i<msg_wk->npixel;i++){
		msg_wk->msgBMP[4*i  ] =  icolor_mid[0];
		msg_wk->msgBMP[4*i+1] =  icolor_mid[1];
		msg_wk->msgBMP[4*i+2] =  icolor_mid[2];
		msg_wk->msgBMP[4*i+3] =  msg_wk->message_opacity;
	};
/*
	for(i=0;i<3*msg_wk->npixel;i++){
		msg_wk->testBMP[i] =  0;
	};
*/
	return;
};

static void set_message_image(int icolor_txt[4], int icolor_mid[4], 
							  struct msg_work *msg_wk){
	int i;
	
	YsGlWriteStringToRGBA8Bitmap(msg_wk->minlabel, icolor_mid[0], icolor_mid[1],
								 icolor_mid[2], icolor_mid[3], 
								 msg_wk->msgBMP, msg_wk->npix_x, msg_wk->npix_y,
								 0, 4, YsFont16x24, 20, 24);
	YsGlWriteStringToRGBA8Bitmap(msg_wk->minlabel, icolor_mid[0], icolor_mid[1],
								 icolor_mid[2], icolor_mid[3], 
								 msg_wk->msgBMP, msg_wk->npix_x, msg_wk->npix_y,
								 0, 6, YsFont16x24, 20, 24);
	YsGlWriteStringToRGBA8Bitmap(msg_wk->minlabel, icolor_mid[0], icolor_mid[1], 
								 icolor_mid[2], icolor_mid[3], 
								 msg_wk->msgBMP, msg_wk->npix_x, msg_wk->npix_y,
								 1, 4, YsFont16x24, 20, 24);

	YsGlWriteStringToRGBA8Bitmap(msg_wk->minlabel, icolor_txt[0], icolor_txt[1], 
								 icolor_txt[2], icolor_txt[3], 
								 msg_wk->msgBMP, msg_wk->npix_x, msg_wk->npix_y,
								 0, 5, YsFont16x24, 20, 24);
	
	/* Draw box in the texture */
    for(i=0;i<2*msg_wk->npix_x;i++){
        msg_wk->msgBMP[4*i  ] = icolor_txt[0];
        msg_wk->msgBMP[4*i+1] = icolor_txt[1];
        msg_wk->msgBMP[4*i+2] = icolor_txt[2];
        msg_wk->msgBMP[4*i+3] = icolor_txt[3];
        msg_wk->msgBMP[4*(msg_wk->npixel-i-1)  ] = icolor_mid[0];
        msg_wk->msgBMP[4*(msg_wk->npixel-i-1)+1] = icolor_mid[1];
        msg_wk->msgBMP[4*(msg_wk->npixel-i-1)+2] = icolor_mid[2];
        msg_wk->msgBMP[4*(msg_wk->npixel-i-1)+3] = icolor_mid[3];
    };
    for(i=0;i<msg_wk->npix_y;i++){
        msg_wk->msgBMP[4*msg_wk->npix_x*i  ] = icolor_txt[0];
        msg_wk->msgBMP[4*msg_wk->npix_x*i+1] = icolor_txt[1];
        msg_wk->msgBMP[4*msg_wk->npix_x*i+2] = icolor_txt[2];
        msg_wk->msgBMP[4*msg_wk->npix_x*i+3] = icolor_txt[3];
        msg_wk->msgBMP[4*msg_wk->npix_x*i+4] = icolor_txt[0];
        msg_wk->msgBMP[4*msg_wk->npix_x*i+5] = icolor_txt[1];
        msg_wk->msgBMP[4*msg_wk->npix_x*i+6] = icolor_txt[2];
        msg_wk->msgBMP[4*msg_wk->npix_x*i+7] = icolor_txt[3];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-4] = icolor_mid[0];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-3] = icolor_mid[1];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-2] = icolor_mid[2];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-1] = icolor_mid[3];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-8] = icolor_mid[0];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-7] = icolor_mid[1];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-6] = icolor_mid[2];
        msg_wk->msgBMP[4*msg_wk->npix_x*(i+1)-5] = icolor_mid[3];
    };
	/*
    for(i=0;i<msg_wk->npixel;i++){
		msg_wk->testBMP[3*i  ] = (unsigned char) (0.8 * (float) ((int) msg_wk->msgBMP[4*i  ]));
		msg_wk->testBMP[3*i+1] = (unsigned char) (0.2 * (float) ((int) msg_wk->msgBMP[4*i+1]));
		msg_wk->testBMP[3*i+2] = (unsigned char) (0.4 * (float) ((int) msg_wk->msgBMP[4*i+2]));
	};
	pixout_BMP_c("/Users/matsui/Desktop/aho", msg_wk->npix_x, msg_wk->npix_y, msg_wk->testBMP);
*/
	
	for(i=0;i<msg_wk->npixel;i++){
		msg_wk->msgBMP[4*i+3] = (unsigned char) ((float) 255 * msg_wk->message_opacity);
	};
};


void set_windowsize_image(int npixel_x, int npixel_y, struct msg_work *msg_wk){
	int i;
	int icolor_txt[4];
	int icolor_mid[4];
	
	for(i=0;i<3;i++){
		icolor_txt[i] = 255;
		icolor_mid[i] = 191;
	};
	icolor_txt[3] = 255;
	icolor_txt[3] = 255;
	
	sprintf(msg_wk->minlabel, " Window size:(%4d,%4d)", npixel_x, npixel_y);
	
	set_message_image(icolor_txt, icolor_mid, msg_wk);
};
