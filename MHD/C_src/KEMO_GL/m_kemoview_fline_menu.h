
/* m_kemoview_fline_menu.h */

#ifndef M_KEMOVIEW_FLINE_MENU_
#define M_KEMOVIEW_FLINE_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "m_color_table_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "kemoviewer_base.h"

struct fline_menu_val{
	struct kv_string *fline_header;
	int fline_step;
	int iformat_fline_file;
	
    int iflag_draw_time;
    double time;

	int iflag_draw_fline;
	
    long if_draw_fline;
    long ic_draw_fline;
	long icomp_draw_fline;
	
	int fieldline_color;
	long fieldline_type;
	double fieldline_thick;
	
	struct colormap_params *cmap_fline;
	struct colormap_params **cmap_fline_comp;
	struct colormap_params **cmap_fline_fld;
};

/* Prototypes */

void alloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m);
void dealloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m);

void init_fline_parameters(struct fline_menu_val *fline_m);

void set_fline_color_field(int selected, struct psf_data *fline_s, 
			struct fline_menu_val *fline_m);
void set_fline_color_component(int selected, struct psf_data *fline_s,
			struct fline_menu_val *fline_m);


#endif
