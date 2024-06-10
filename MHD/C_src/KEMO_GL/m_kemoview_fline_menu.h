
/* m_kemoview_fline_menu.h */

#ifndef M_KEMOVIEW_FLINE_MENU_
#define M_KEMOVIEW_FLINE_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "m_color_table_c.h"
#include "m_fline_data_4_viewer_c.h"
#include "kemoviewer_base.h"

struct fline_menu_val{
	struct kv_string *viz_prefix_c;
	int viz_step_c;
	int iformat_viz_file;
	
    int iflag_draw_time;
    double time;
    
    long if_draw_viz;
    long ic_draw_viz;
	long icomp_draw_viz;
	
	int iflag_draw_viz;
	int iflag_draw_cbar;
	
	struct colormap_params **cmap_viz_comp;
	struct colormap_params **cmap_viz_fld;
	
	int viz_color_mode;
    
    long viz_line_type;
    int ncorner_viz_line;
	double viz_line_width;
};

/* Prototypes */
struct fline_menu_val * init_fline_menu_val(void);

void alloc_draw_fline_flags(struct fline_data *fline_d, struct fline_menu_val *fline_m);
void dealloc_draw_fline_flags(struct fline_data *fline_d, struct fline_menu_val *fline_m);

void init_fline_parameters(struct fline_menu_val *fline_m);

void set_fline_color_field(int selected, struct fline_data *fline_d,
                           struct fline_menu_val *fline_m);
void set_fline_color_component(int selected, struct fline_data *fline_d,
                               struct fline_menu_val *fline_m);


#endif
