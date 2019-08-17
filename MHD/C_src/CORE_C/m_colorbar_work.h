/*
// m_colorbar_work.h
*/

#ifndef M_COLORBAR_WORK_
#define M_COLORBAR_WORK_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "m_color_table_c.h"
#include "ysglfontdata.h"

#define IHIGHT_TXT 20
#define IWIDTH_TXT 140

struct cbar_work{
	int iflag_zero;
	
	float xwin;
	float ywin;
	
	float xbar_min;
	float xbar_max;
	float xbar_mid;
	
	float ybar_min;
	float ybar_max;
	float ydelta;
	float yline_zero;
	
	int num_quad;
	double psf_min;
	double psf_max;
	
	char minlabel[20];
	char maxlabel[20];
	char zerolabel[20];
	
	unsigned char numBMP[12*IHIGHT_TXT*IWIDTH_TXT];
	unsigned char testBMP[9*IHIGHT_TXT*IWIDTH_TXT];
};


/* prototypes */

struct cbar_work * alloc_colorbar_position();
void set_colorbar_position(int iflag_retina, int nx_win, int ny_win,
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk);
void set_colorbar_text_image(float text_color3[3], struct cbar_work *cbar_wk);

#endif

