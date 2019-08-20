/*
// m_colorbar_work.c
*/

#include "m_colorbar_work.h"


struct cbar_work * alloc_colorbar_position(){
	struct cbar_work *cbar_wk = (struct cbar_work *) malloc(sizeof(struct cbar_work));
	return cbar_wk;
};

void set_colorbar_position(int iflag_retina, int nx_win, int ny_win,
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk){
	cbar_wk->num_quad = 64;
	
	cbar_wk->xwin = (float)nx_win;
	cbar_wk->ywin = (float)ny_win;
	
    if( cbar_wk->xwin >= 640*(iflag_retina+1) ){
        cbar_wk->xbar_max = cbar_wk->xwin * 0.875;
    } else {
        cbar_wk->xbar_max = cbar_wk->xwin - (iflag_retina+1) * 80;
    }
	cbar_wk->xbar_min = cbar_wk->xbar_max - 0.025 * cbar_wk->xwin;
	cbar_wk->xbar_mid = (cbar_wk->xbar_min + cbar_wk->xbar_max) * 0.5;
	cbar_wk->ybar_min = 0.05 * cbar_wk->ywin;
	cbar_wk->ybar_max = 0.25 * cbar_wk->ywin;
	cbar_wk->ydelta =  (cbar_wk->ybar_max - cbar_wk->ybar_min) / ((float) cbar_wk->num_quad);
	
	cbar_wk->iflag_zero = 0;
	
	cbar_wk->psf_min = cmap_s->color_data[IZERO];
	cbar_wk->psf_max = cmap_s->color_data[cmap_s->n_color_point-1];
	if( (cbar_wk->psf_min*cbar_wk->psf_max) < ZERO ) cbar_wk->iflag_zero = 1;
	
	cbar_wk->yline_zero = cbar_wk->ybar_min 
		+ (cbar_wk->ybar_max - cbar_wk->ybar_min) * (-cbar_wk->psf_min) 
		/ (cbar_wk->psf_max - cbar_wk->psf_min);
	
	return;
}


void clear_colorbar_text_image(struct cbar_work *cbar_wk){
	int i;
	
	for(i=0;i<12*IWIDTH_TXT*IHIGHT_TXT;i++){
		cbar_wk->numBMP[i] =  0;
	};
	for(i=0;i<9*IWIDTH_TXT*IHIGHT_TXT;i++){
		cbar_wk->testBMP[i] =  0;
	};
	return;
};

void set_colorbar_text_image(float text_color3[3], struct cbar_work *cbar_wk){
	int i;
	
	sprintf(cbar_wk->minlabel, "% 3.2E",cbar_wk->psf_min);
	sprintf(cbar_wk->maxlabel, "% 3.2E",cbar_wk->psf_max);
	sprintf(cbar_wk->zerolabel,"% 3.2E",ZERO);
	
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 0, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 2, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 1, 0, YsFont12x16, 14, 16);

	YsGlWriteStringToRGBA8Bitmap(cbar_wk->minlabel, 255, 255, 255, 255, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 1, YsFont12x16, 14, 16);

	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, IHIGHT_TXT, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 0, 2+IHIGHT_TXT, YsFont12x16, 14, 16);
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
								 1, IHIGHT_TXT, YsFont12x16, 14, 16);
	
	YsGlWriteStringToRGBA8Bitmap(cbar_wk->maxlabel, 255, 255, 255, 255, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT, 
								 0, 1+IHIGHT_TXT, YsFont12x16, 14, 16);

	if(cbar_wk->iflag_zero == 1){
		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 0, 2*IHIGHT_TXT, YsFont12x16, 14, 16);
		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 0, 2+2*IHIGHT_TXT, YsFont12x16, 14, 16);
		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 191, 191, 191, 191, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 1, 2*IHIGHT_TXT, YsFont12x16, 14, 16);

		YsGlWriteStringToRGBA8Bitmap(cbar_wk->zerolabel, 255, 255, 255, 255, cbar_wk->numBMP, IWIDTH_TXT, 3*IHIGHT_TXT,
									 0, 1+2*IHIGHT_TXT, YsFont12x16, 14, 16);
	};
	for(i=0;i<3*IWIDTH_TXT*IHIGHT_TXT;i++){
		cbar_wk->testBMP[3*i  ] = (unsigned char) (0.8 * (float) ((int) cbar_wk->numBMP[4*i  ]));
		cbar_wk->testBMP[3*i+1] = (unsigned char) (0.2 * (float) ((int) cbar_wk->numBMP[4*i+1]));
		cbar_wk->testBMP[3*i+2] = (unsigned char) (0.4 * (float) ((int) cbar_wk->numBMP[4*i+2]));
	};
//	pixout_BMP_c("/Users/matsui/Desktop/aho", IWIDTH_TXT, 3*IHIGHT_TXT, testBMP);
	
	for(i=0;i<3*IWIDTH_TXT*IHIGHT_TXT;i++){
		cbar_wk->numBMP[4*i  ] = (unsigned char) (text_color3[0] * (float) ((int) cbar_wk->numBMP[4*i  ]));
		cbar_wk->numBMP[4*i+1] = (unsigned char) (text_color3[1] * (float) ((int) cbar_wk->numBMP[4*i+1]));
		cbar_wk->numBMP[4*i+2] = (unsigned char) (text_color3[2] * (float) ((int) cbar_wk->numBMP[4*i+2]));
	};
	return;
};


