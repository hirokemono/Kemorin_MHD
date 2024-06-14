/*
// m_kemoview_fline_menu.c
*/

#include "m_kemoview_fline_menu.h"

void init_fline_parameters(struct psf_menu_val *fline_m){
	fline_m ->iflag_draw_viz =  IZERO;
	
	fline_m->if_draw_viz =    INIT_IF_DRAW_FLINE;
	fline_m->ic_draw_viz =    INIT_IC_DRAW_FLINE;
	fline_m->icomp_draw_viz = INIT_IC_DRAW_FLINE;
	
	fline_m->viz_color_mode =  INIT_FLDLINE_COLOR;
	fline_m->viz_line_type =   INIT_FLDLINE_TYPE;
	fline_m->viz_line_width = INIT_FLDLINE_THICK;
    
    fline_m->scale_vect = ONE;
	return;
}

