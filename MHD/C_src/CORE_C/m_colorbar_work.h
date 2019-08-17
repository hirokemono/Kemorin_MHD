/*
// m_colorbar_work.h
*/

#ifndef M_COLORBAR_WORK_
#define M_COLORBAR_WORK_

#include <math.h>
#include "m_color_table_c.h"
#include "ysglusefontbitmap.h"

#define IHIGHT_TXT 20
#define IWIDTH_TXT 140

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
	
	unsigned char numBMP[12*IHIGHT_TXT*IWIDTH_TXT];
	unsigned char testBMP[9*IHIGHT_TXT*IWIDTH_TXT];
};


/* prototypes */

struct cbar_work * alloc_colorbar_position();
void set_colorbar_position(int iflag_retina, GLint nx_win, GLint ny_win,
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk);
void set_colorbar_text_image(const char minlabel[], const char maxlabel[], 
			const char zerolabel[], float text_color3[3], struct cbar_work *cbar_wk);

#endif

