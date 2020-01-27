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

#define IHIGHT_MSG 40
#define IWIDTH_MSG 488

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
	
	int id_texture;
    int npixel;
    int npix_x;
    int npix_y;
	unsigned char *numBMP;
	unsigned char *testBMP;
};

struct msg_work{
    int iflag_message;
    float xwin;
    float ywin;
    
    float xbar_max;
    float ybar_min;
    
    char minlabel[26];
    
    int id_texture;
    int npixel;
    int npix_x;
    int npix_y;
    unsigned char *msgBMP;
    unsigned char *testBMP;
};


/* prototypes */

struct cbar_work * alloc_colorbar_position(void);
void dealloc_colorbar_position(struct cbar_work *cbar_wk);
void set_colorbar_position(int iflag_retina, int nx_win, int ny_win,
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk);

void clear_colorbar_text_image(struct cbar_work *cbar_wk);
void set_colorbar_text_image(float text_color3[3], struct cbar_work *cbar_wk);

struct msg_work * alloc_message_work(void);
void dealloc_message_work(struct msg_work *msg_wk);  
void set_message_switch(int iflag, struct msg_work *msg_wk);

void set_message_position(int iflag_retina, int nx_win, int ny_win,
						  struct msg_work *msg_wk);
void clear_message_text_image(struct msg_work *msg_wk);
void set_windowsize_image(int npixel_x, int npixel_y, 
                          float text_color3[3], struct msg_work *msg_wk);

#endif

