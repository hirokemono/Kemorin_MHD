
/* m_kemoview_fline_menu.h */

#ifndef M_KEMOVIEW_FLINE_MENU_
#define M_KEMOVIEW_FLINE_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "m_color_table_c.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"
#include "kemoviewer_base.h"


/* Prototypes */
struct psf_menu_val * init_fline_menu_val(void);

void alloc_draw_fline_flags(struct fline_data *fline_d, struct psf_menu_val *fline_m);
void dealloc_draw_fline_flags(struct fline_data *fline_d, struct psf_menu_val *fline_m);

void init_fline_parameters(struct psf_menu_val *fline_m);

void set_fline_color_field(int selected, struct fline_data *fline_d,
                           struct psf_menu_val *fline_m);
void set_fline_color_component(int selected, struct fline_data *fline_d,
                               struct psf_menu_val *fline_m);


#endif
