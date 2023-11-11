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

#define NCHARA_CBOX   20
#define NCHARA_MSG    26

#define IHIGHT_TXT     20
#define IWIDTH_TXT    140
#define IWIDTH_TLABEL 280

#define IHIGHT_MSG 40
#define IWIDTH_MSG 488


struct line_text_image{
    int len_text;
    char *texts;
    
    int npixel;
    int npix_img[2];
    unsigned char *imgBMP;
    unsigned char *testBMP;

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
	
	int id_texture[3];
    struct line_text_image *cbar_min_image;
    struct line_text_image *cbar_max_image;
    struct line_text_image *cbar_zero_image;
};

struct tlabel_work{
	float xwin;
	float ywin;
    int id_texture;
    struct line_text_image *tlabel_image;
};

struct msg_work{
    float message_opacity;
    float xbar_max;
    float ybar_min;
    
    int id_texture;
    struct line_text_image *message_image;
};



/* prototypes */
struct line_text_image * alloc_line_text_image(int npix_x, int npix_y, int len_text);
void dealloc_line_text_image(struct line_text_image *l_txt_img);
void clear_line_text_image(struct line_text_image *l_txt_img);
void set_line_text_image(float text_color3[3], struct line_text_image * l_txt_img);


struct cbar_work * alloc_colorbar_position(void);
void dealloc_colorbar_position(struct cbar_work *cbar_wk);
void set_colorbar_position(int iflag_retina, int nx_win, int ny_win,
						   struct colormap_params *cmap_s, struct cbar_work *cbar_wk);

void clear_colorbar_text_image(struct cbar_work *cbar_wk);
void set_colorbar_text_image(float text_color3[3], struct cbar_work *cbar_wk);

struct tlabel_work * alloc_tlabel_work(void);
void dealloc_tlabel_work(struct tlabel_work *tlabel_wk);
void clear_time_text_image(struct tlabel_work *tlabel_wk);
void set_time_text_image(float text_color3[3], struct tlabel_work *tlabel_wk);

struct msg_work * alloc_message_work(void);
void dealloc_message_work(struct msg_work *msg_wk);  
void set_message_opacity(float opacity, struct msg_work *msg_wk);

void set_message_position(int iflag_retina, int nx_win, int ny_win,
						  struct msg_work *msg_wk);
void clear_message_text_image(struct msg_work *msg_wk);
void set_windowsize_image(int npixel_x, int npixel_y, struct msg_work *msg_wk);

#endif

