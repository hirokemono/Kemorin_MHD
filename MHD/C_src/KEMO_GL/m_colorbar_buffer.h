/*
// m_colorbar_buffer.h
*/

#ifndef m_colorbar_buffer_
#define m_colorbar_buffer_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "m_vertex_buffer.h"
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


struct textbox_gl_buffer{
    float text_opacity;
    int len_text;
    char *texts;

    struct gl_strided_buffer *vertex;
    struct gl_texure_image *image;
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
	
    struct textbox_gl_buffer *cbar_min_image;
    struct textbox_gl_buffer *cbar_max_image;
};

/* prototypes */
struct textbox_gl_buffer * alloc_line_text_image(int npix_x, int npix_y, int len_text);
void dealloc_line_text_image(struct textbox_gl_buffer *l_txt_img);

void set_colorbar_position(int iflag_retina, int nx_win, int ny_win,
						   struct colormap_params *cmap_s, struct cbar_work *cbar_wk);

void set_colorbar_text_image(float text_color3[3], float value,
                             struct textbox_gl_buffer *l_txt_img);

void set_time_text_image(float text_color3[3], struct textbox_gl_buffer *timelabel_buf);

float message_xmax(const int nx_win);
float message_ymin(const int ny_win);
void set_windowsize_image(const int npixel_x, const int npixel_y,
                          struct textbox_gl_buffer *message_buf);

#endif // m_colorbar_buffer_

