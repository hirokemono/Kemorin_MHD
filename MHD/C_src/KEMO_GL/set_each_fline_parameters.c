/*
//  set_each_fline_parameters.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "set_each_fline_parameters.h"


/* Subroutines for field lines */

void get_fline_full_path_file_name(struct fline_menu_val *fline_m, struct kv_string *ucd_m){
	alloc_set_ucd_file_name_by_fline(fline_m, ucd_m);
	return;
}
int get_fline_file_step_prefix(struct fline_menu_val *fline_m, struct kv_string *fline_filehead){
	alloc_copy_string(fline_m->fline_header->string, fline_filehead);
	return fline_m->fline_step;
};
void set_fline_file_step(struct fline_menu_val *fline_m, int istep){
	fline_m->fline_step = istep;
};

void set_fline_switch(struct fline_menu_val *fline_m, int iflag) {
	fline_m->iflag_draw_fline = iflag;
};
void set_fline_color_type(struct fline_menu_val *fline_m, int iflag) {
	fline_m->fieldline_color = iflag;
};


int get_fline_switch(struct fline_menu_val *fline_m){return fline_m->iflag_draw_fline;};
long get_fline_color_num_field(struct fline_data *fline_d){return fline_d->nfield;};
long get_fline_color_ncomptot(struct fline_data *fline_d){return fline_d->ncomptot;};
long fline_color_num_comps(struct fline_data *fline_d, int i){return fline_d->ncomp[i];};
long get_fline_color_istack(struct fline_data *fline_d, int i){return fline_d->istack_comp[i];};
void get_fline_color_data_name(struct fline_data *fline_d,
                               struct kv_string *colorname, int i){
    alloc_copy_string(fline_d->data_name[i], colorname);
};
long get_fline_color_field(struct fline_menu_val *fline_m){
	return fline_m->if_draw_fline;
};
long get_fline_color_component(struct fline_menu_val *fline_m){
	return fline_m->ic_draw_fline;
};
long get_fline_color_data_adress(struct fline_menu_val *fline_m){
	return fline_m->icomp_draw_fline;
};
int get_fline_colormode(struct fline_menu_val *fline_m) {
	return fline_m->fieldline_color;
};


void set_fline_type(struct fline_menu_val *fline_m, long iflag) {
	fline_m->fieldline_type = iflag;
};
long get_fline_type(struct fline_menu_val *fline_m) {return fline_m->fieldline_type;};

void set_fline_thickness(double value, struct fline_menu_val *fline_m){fline_m->fieldline_thick = value;};
double get_fline_thickness(struct fline_menu_val *fline_m){return fline_m->fieldline_thick;};

double get_fline_data_min(struct fline_data *fline_d, int i){
	return fline_d->d_min[i];
};
double get_fline_data_max(struct fline_data *fline_d, int i){
	return fline_d->d_max[i];
};

int send_coordinate_id_fline(struct fline_data *fline_d, struct fline_menu_val *fline_m){
    long id_current = fline_m->if_draw_fline;
    return fline_d->id_coord[id_current];
};

void set_fline_linear_colormap(double minvalue, int i_min_digit, double maxvalue, int i_max_digit, 
							   struct fline_menu_val *fline_m){
	double range_min = const_from_digit_order(minvalue, i_min_digit);
	double range_max = const_from_digit_order(maxvalue, i_max_digit);
	set_linear_colormap(fline_m->cmap_fline, range_min, range_max);
}
void set_fline_constant_opacity(struct fline_data *fline_d,
                                struct fline_menu_val *fline_m, double opacity){
	set_constant_opacitymap(fline_m->cmap_fline,
                            fline_d->d_min[fline_m->icomp_draw_fline],
                            fline_d->d_max[fline_m->icomp_draw_fline], opacity);
}

double get_fline_opacity_at_value(struct fline_menu_val *fline_m, double value){
    struct colormap_params *cmap_s = fline_m->cmap_fline;
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	double opacity =  set_opacity_from_value_s(omap_array, value);
    dealloc_colormap_array(omap_array);
    return opacity;
}
void set_fline_color_data(struct fline_menu_val *fline_m, 
			int i_point, double value, double color){
	set_each_color_point_s(fline_m->cmap_fline, i_point, value, color);
}
void set_fline_opacity_data(struct fline_menu_val *fline_m,
			int i_point, double value, double opacity){
	set_each_opacity_point_s(fline_m->cmap_fline, i_point, value, opacity);
}

void set_fline_color_mode_id(struct fline_menu_val *fline_m, int isel){
	set_color_mode_by_id(fline_m->cmap_fline, isel);
}

double get_fline_min_color(struct fline_menu_val *fline_m){
	double d, c;
    get_color_table_items_s(fline_m->cmap_fline, 0, &d, &c);
	return d;
}
double get_fline_max_color(struct fline_menu_val *fline_m){
	double d, c;
	int n = get_color_table_num_s(fline_m->cmap_fline);
    get_color_table_items_s(fline_m->cmap_fline, n-1, &d, &c);
	return d;
}
double get_fline_min_opacity(struct fline_menu_val *fline_m){
	return get_minimum_opacity_s(fline_m->cmap_fline);
};
double get_fline_max_opacity(struct fline_menu_val *fline_m){
	return get_maximum_opacity_s(fline_m->cmap_fline);
};

int get_fline_color_num(struct fline_menu_val *fline_m){
	return get_color_table_num_s(fline_m->cmap_fline);
};
int get_fline_opacity_num(struct fline_menu_val *fline_m){
	return get_opacity_table_num_s(fline_m->cmap_fline);
};


void get_fline_color_item(struct fline_menu_val *fline_m,
                          int i_point, double *value, double *color){
    get_color_table_items_s(fline_m->cmap_fline, i_point, value, color);
}
void get_fline_opacity_item(struct fline_menu_val *fline_m,
                            int i_point, double *value, double *opacity){
    get_opacity_table_items_s(fline_m->cmap_fline, i_point, value, opacity);
}

void get_fline_colormap_tables(struct fline_menu_val *fline_m, int *id_cmap, int *num_cmap, int *num_alpha,
                               float *cmap_data, float *cmap_norm, float *alpha_data, float *alpha_norm){
    get_colormap_to_tables(fline_m->cmap_fline, id_cmap, num_cmap, num_alpha,
                           cmap_data, cmap_norm, alpha_data, alpha_norm);
}


void write_fline_colormap_file(struct kv_string *filename, const int iflag_draw_axis,
                               struct fline_menu_val *fline_m){
	write_colormap_control_file_s(filename->string, fline_m->iflag_draw_time, iflag_draw_axis,
                                  0, fline_m->cmap_fline);
}
void read_fline_colormap_file(struct kv_string *filename, struct fline_menu_val *fline_m){
	read_colormap_control_file_s(filename->string, fline_m->cmap_fline);
}

