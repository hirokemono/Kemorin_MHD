/*
//  set_each_fline_parameters.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "set_each_fline_parameters.h"


/* Subroutines for field lines */

void set_fline_file_step(struct psf_menu_val *fline_m, int istep){
	fline_m->viz_step_c = istep;
};


void set_fline_type(struct psf_menu_val *fline_m, long iflag) {
	fline_m->viz_line_type = iflag;
};
long get_fline_type(struct psf_menu_val *fline_m) {return fline_m->viz_line_type;};
