
/* m_color_table_c.h */

#ifndef M_COLOR_TABLE_C_
#define M_COLOR_TABLE_C_

#include "set_rgb_colors_c.h"

#define RAINBOW_MODE    0
#define GRAYSCALE_MODE  1
#define RED_BLUE_MODE   2
#define SYM_GRAY_MODE   3

struct colormap_params{
	int id_color_mode;
	int n_color_point, nbuf_color_point;
	double *color_data;
	double *color_value;
	
	int n_opacity_point, nbuf_opacity_point;
	double min_opacity, max_opacity;
	double *opacity_data;
	double *opacity_value;
    
    double *single_color;
};

/* prototypes */

void alloc_single_color_code(struct colormap_params *cmap_s);
void alloc_color_index_list_s(struct colormap_params *cmap_s, int id_cmode, int num);
void alloc_opacity_index_list_s(struct colormap_params *cmap_s, int num);

void dealloc_single_color_code(struct colormap_params *cmap_s);
void dealloc_color_index_list_s(struct colormap_params *cmap_s);
void dealloc_opacity_index_list_s(struct colormap_params *cmap_s);

void realloc_color_index_list_s(struct colormap_params *cmap_s, int id_cmode, int num);
void realloc_opacity_index_list_s(struct colormap_params *cmap_s, int num);

#endif

