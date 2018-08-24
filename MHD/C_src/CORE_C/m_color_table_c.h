
/* m_color_table_c.h */

#ifndef M_COLOR_TABLE_C_
#define M_COLOR_TABLE_C_

#include "set_rgb_colors_c.h"
#include "t_control_chara_IO.h"
#include "t_control_real2_IO.h"

#define RAINBOW_MODE    0
#define GRAYSCALE_MODE  1
#define RED_BLUE_MODE   2
#define SYM_GRAY_MODE   3

struct colormap_params{
	struct chara_ctl_item *colormap_mode;
	struct real2_clist *colormap_clist;
	struct real2_clist *opacitymap_clist;
	
	double cmap_min;
	double cmap_max;
	
	double min_opacity, max_opacity;
    
    double *single_color;
};

/* prototypes */

void alloc_single_color_code(struct colormap_params *cmap_s);
void alloc_color_index_list_s(struct colormap_params *cmap_s);
void alloc_opacity_index_list_s(struct colormap_params *cmap_s);

void dealloc_single_color_code(struct colormap_params *cmap_s);
void dealloc_color_index_list_s(struct colormap_params *cmap_s);
void dealloc_opacity_index_list_s(struct colormap_params *cmap_s);

void delete_color_index_list_s(struct colormap_params *cmap_s, int i_delete);
void delete_opacity_index_list_s(struct colormap_params *cmap_s, int i_delete);

void add_color_index_list_s(struct colormap_params *cmap_s, double add_value, double add_color);
void add_opacity_index_list_s(struct colormap_params *cmap_s, double add_value, double add_opacity);

#endif

