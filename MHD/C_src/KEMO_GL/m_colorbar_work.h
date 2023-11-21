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
#include "write_image_2_bmp.h"

#define ICOLOR_FULL   255
#define ICOLOR_MID    191

#define NCHARA_CBOX   20
#define NCHARA_MSG    26

#define IHIGHT_TXT     20
#define IWIDTH_TXT    140
#define IWIDTH_TLABEL 280

#define IHIGHT_MSG 40
#define IWIDTH_MSG 488


struct line_text_image{
    float text_opacity;
    int len_text;
    char *texts;
    
    int npixel;
    int npix_img[2];
    unsigned char *imgBMP;

    int npix_box[2];
};

struct cbar_work{
	int iflag_zero;
	
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
	
    struct line_text_image *cbar_min_image;
    struct line_text_image *cbar_max_image;
    struct line_text_image *cbar_zero_image;
};

/* prototypes */
struct line_text_image * alloc_line_text_image(int npix_x, int npix_y, int len_text);
void dealloc_line_text_image(struct line_text_image *l_txt_img);
void clear_line_text_image(struct line_text_image *l_txt_img);
void set_line_text16_image(int icolor_txt, int icolor_mid, struct line_text_image *l_txt_img);
void set_line_text24_image(int icolor_txt, int icolor_mid, struct line_text_image *l_txt_img);
void set_line_msgbox_image(int icolor_txt, int icolor_mid,
                           struct line_text_image *l_txt_img);
void set_line_text_color(float text_color3[3], struct line_text_image *l_txt_img);

void set_colorbar_position(int iflag_retina, int nx_win, int ny_win,
						   struct colormap_params *cmap_s, struct cbar_work *cbar_wk);

void set_colorbar_text_image(float text_color3[3], float value,
                             struct line_text_image *l_txt_img);

void set_time_text_image(float text_color3[3], struct line_text_image *tlabel_image);

float message_xmax(const int nx_win);
float message_ymin(const int ny_win);
void set_windowsize_image(int npixel_x, int npixel_y,
                          struct line_text_image *message_image);

#endif

