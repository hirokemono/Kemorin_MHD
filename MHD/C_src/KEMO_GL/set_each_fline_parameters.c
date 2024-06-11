/*
//  set_each_fline_parameters.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "set_each_fline_parameters.h"


/* Subroutines for field lines */

void get_fline_full_path_file_name(struct psf_menu_val *fline_m, struct kv_string *ucd_m){
	alloc_set_ucd_file_name_by_fline(fline_m, ucd_m);
	return;
}
int get_fline_file_step_prefix(struct psf_menu_val *fline_m, struct kv_string *fline_filehead){
	alloc_copy_string(fline_m->viz_prefix_c->string, fline_filehead);
	return fline_m->viz_step_c;
};
void set_fline_file_step(struct psf_menu_val *fline_m, int istep){
	fline_m->viz_step_c = istep;
};


void set_fline_type(struct psf_menu_val *fline_m, long iflag) {
	fline_m->viz_line_type = iflag;
};
long get_fline_type(struct psf_menu_val *fline_m) {return fline_m->viz_line_type;};
