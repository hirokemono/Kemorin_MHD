/*
// m_color_table_c.h
*/

#ifndef M_COLOR_TABLE_C_
#define M_COLOR_TABLE_C_

#include "set_rgb_colors_c.h"
#include "t_control_real2_IO.h"

#define RAINBOW_MODE    0
#define GRAYSCALE_MODE  1
#define RED_BLUE_MODE   2
#define SYM_GRAY_MODE   3
#define ORANGE_CYAN_MODE   4
#define MOLTEN_METAL_MODE  5
#define SPACE_COLOR_MODE   6

#define MAX_COLORMAP_POINT   16


struct colormap_params{
	int id_color_mode;
	struct real2_clist *colormap;
	struct real2_clist *opacitymap;
	
	double min_opacity, max_opacity;
	
	double *single_color;
};

struct colormap_array{
	int num;
	double *data;
	double *value;
};

/* prototypes */

void alloc_color_index_list_s(struct colormap_params *cmap_s, int id_cmode);
void dealloc_color_index_list_s(struct colormap_params *cmap_s);

void delete_color_index_list_s(struct colormap_params *cmap_s, int i_delete);
void delete_opacity_index_list_s(struct colormap_params *cmap_s, int i_delete);

void add_color_index_list_s(struct colormap_params *cmap_s, double add_value, double add_color);
void add_opacity_index_list_s(struct colormap_params *cmap_s, double add_value, double add_opacity);

struct colormap_array * init_colormap_from_list(struct real2_clist *f_colortbl_ctl);
void dealloc_colormap_array(struct colormap_array *cmap);


#endif

