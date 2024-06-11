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


long get_fline_color_num_field(struct fline_data *fline_d){return fline_d->nfield;};
long get_fline_color_ncomptot(struct fline_data *fline_d){return fline_d->ncomptot;};
long fline_color_num_comps(struct fline_data *fline_d, int i){return fline_d->ncomp[i];};
long get_fline_color_istack(struct fline_data *fline_d, int i){return fline_d->istack_comp[i];};
void get_fline_color_data_name(struct fline_data *fline_d,
                               struct kv_string *colorname, int i){
    alloc_copy_string(fline_d->data_name[i], colorname);
};

void set_fline_type(struct psf_menu_val *fline_m, long iflag) {
	fline_m->viz_line_type = iflag;
};
long get_fline_type(struct psf_menu_val *fline_m) {return fline_m->viz_line_type;};



double get_fline_data_min(struct fline_data *fline_d, int i){
	return fline_d->d_min[i];
};
double get_fline_data_max(struct fline_data *fline_d, int i){
	return fline_d->d_max[i];
};

int send_coordinate_id_fline(struct fline_data *fline_d, struct psf_menu_val *fline_m){
    long id_current = fline_m->if_draw_viz;
    return fline_d->id_coord[id_current];
};


void set_fline_constant_opacity(struct fline_data *fline_d,
                                struct psf_menu_val *fline_m, double opacity){
	set_constant_opacitymap(fline_m->cmap_viz_comp[fline_m->icomp_draw_viz],
                            fline_d->d_min[fline_m->icomp_draw_viz],
                            fline_d->d_max[fline_m->icomp_draw_viz], opacity);
}
