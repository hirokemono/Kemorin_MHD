/*
//  set_each_psf_parameters.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include <stdio.h>
#include "set_each_psf_parameters.h"

int send_VIZ_file_prefix_step_format(struct psf_menu_val *psf_menu,
                                     struct kv_string *psf_filehead,
                                     int *i_file_step){
	alloc_copy_string(psf_menu->viz_prefix_c->string, psf_filehead);
    *i_file_step = psf_menu->viz_step_c;
	return psf_menu->iformat_viz_file;
};

int send_each_psf_file_dir_prefix(struct psf_menu_val *psf_menu,
                                  struct kv_string *stripped_dir,
                                  struct kv_string *stripped_filehead){
	alloc_kvstringitem(strlen(psf_menu->viz_prefix_c->string), stripped_filehead);
	alloc_kvstringitem(strlen(psf_menu->viz_prefix_c->string), stripped_dir);
	split_dir_and_file_name_c(psf_menu->viz_prefix_c->string,
				stripped_dir->string, stripped_filehead->string);
	return psf_menu->viz_step_c;
};


long send_nfield_each_VIZ(struct psf_data *psf_d){return psf_d->nfield;};
long send_ncomptot_each_VIZ(struct psf_data *psf_d){return psf_d->ncomptot;};
long send_VIZ_num_component(struct psf_data *psf_d, int i){return psf_d->ncomp[i];};
long send_istack_each_comp_psf(struct psf_data *psf_d, int i){return psf_d->istack_comp[i];};
void send_VIZ_field_name(struct psf_data *psf_d, struct kv_string *colorname, int i){
	alloc_copy_string(psf_d->data_name[i], colorname);
};


int send_field_draw_each_VIZ(struct psf_menu_val *psf_menu){return psf_menu->if_draw_viz;};
int send_draw_comp_id_VIZ(struct psf_menu_val *psf_menu){return psf_menu->ic_draw_viz;};
long send_draw_component_VIZ(struct psf_menu_val *psf_menu){return psf_menu->icomp_draw_viz;};
int send_coordinate_id_VIZ(struct psf_data *psf_d, struct psf_menu_val *psf_menu){
	int id_current = psf_menu->if_draw_viz;
	return psf_d->id_coord[id_current];
};

void set_texture_psf_from_bgra(struct kemo_array_control *psf_a,
                               int width, int height, const unsigned char *bgra_in){
    set_texture_4_psf(width, height, bgra_in, psf_a->psf_texure);
};

void set_each_psf_polygon_mode(struct psf_menu_val *psf_menu, int iflag){psf_menu->polygon_mode_psf = iflag;};
int send_each_psf_polygon_mode(struct psf_menu_val *psf_menu){return psf_menu->polygon_mode_psf;};
int toggle_each_psf_polygon_mode(struct psf_menu_val *psf_menu){
	psf_menu->polygon_mode_psf = toggle_value_c(psf_menu->polygon_mode_psf);
	return psf_menu->polygon_mode_psf;
};

void set_each_psf_vector_mode(struct psf_menu_val *psf_menu, int iflag){psf_menu->ivect_tangential = iflag;};
int send_VIZ_vector_mode(struct psf_menu_val *psf_menu){return psf_menu->ivect_tangential;};
int toggle_each_psf_vector_mode(struct psf_menu_val *psf_menu){
	psf_menu->ivect_tangential = toggle_value_c(psf_menu->ivect_tangential);
	return psf_menu->ivect_tangential;
};

void set_draw_psf_solid(int iflag, struct psf_menu_val *psf_menu){psf_menu->iflag_draw_viz = iflag;};
int send_draw_psf_solid(struct psf_menu_val *psf_menu){return psf_menu->iflag_draw_viz;};

void set_draw_psf_grid(int iflag, struct psf_menu_val *psf_menu){psf_menu->draw_psf_grid = iflag;};
int send_draw_psf_grid(struct psf_menu_val *psf_menu) {return psf_menu->draw_psf_grid;};

void set_draw_psf_zero(int iflag, struct psf_menu_val *psf_menu){psf_menu->draw_psf_zero = iflag;};
int send_draw_psf_zero(struct psf_menu_val *psf_menu) {return psf_menu->draw_psf_zero;};

void set_draw_VIZ_cbar(int iflag, struct psf_menu_val *psf_menu){psf_menu->iflag_draw_cbar = iflag;};
int send_draw_VIZ_cbar(struct psf_menu_val *psf_menu) {return psf_menu->iflag_draw_cbar;};

void set_draw_VIZ_vector(int iflag, struct psf_menu_val *psf_menu){psf_menu->draw_psf_vect = iflag;};
int send_draw_VIZ_vector(struct psf_menu_val *psf_menu) {return psf_menu->draw_psf_vect;};

int send_draw_each_psf_refv(struct psf_menu_val *psf_menu){return psf_menu->draw_psf_refv;};
int toggle_draw_psf_refv(struct psf_menu_val *psf_menu){
	psf_menu->draw_psf_refv = toggle_value_c(psf_menu->draw_psf_refv);
	return psf_menu->draw_psf_refv;
};

void set_VIZ_patch_color_mode(struct psf_menu_val *psf_menu, int iflag){psf_menu->viz_color_mode = iflag;};

void set_each_isoline_color(struct psf_menu_val *psf_menu, int iflag)     {psf_menu->isoline_color = iflag;};
void set_each_n_isoline(struct psf_menu_val *psf_menu, int nlline)        {psf_menu->n_isoline = nlline;};
void set_VIZ_line_width(double value, struct psf_menu_val *psf_menu){psf_menu->viz_line_width = value;};
void set_each_vector_patch_color(struct psf_menu_val *psf_menu, int iflag){psf_menu->vector_patch_color = iflag;};

void set_each_increment_vect(int increment, struct psf_menu_val *psf_menu){
    if(increment > 0) psf_menu->increment_vect = increment;
	return;
};
void set_each_scale_vect(double value, struct psf_menu_val *psf_menu)  {psf_menu->scale_vect = value;};
void set_each_vector_thick(double value, struct psf_menu_val *psf_menu){psf_menu->vector_thick = value;};

int send_VIZ_patch_color_mode(struct psf_menu_val *psf_menu)   {return psf_menu->viz_color_mode;};
int send_each_isoline_color(struct psf_menu_val *psf_menu)     {return psf_menu->isoline_color;};
int send_num_isoline(struct psf_menu_val *psf_menu)            {return psf_menu->n_isoline;};
double get_VIZ_line_width(struct psf_menu_val *psf_menu)       {return psf_menu->viz_line_width;};
int send_each_vector_patch_color(struct psf_menu_val *psf_menu){return psf_menu->vector_patch_color;};

int send_each_increment_vect(struct psf_menu_val *psf_menu){return psf_menu->increment_vect;};
double send_scale_vector(struct psf_menu_val *psf_menu){return psf_menu->scale_vect;};
double send_vector_thick(struct psf_menu_val *psf_menu){return psf_menu->vector_thick;};


void set_viz_colormap_id(struct psf_menu_val *psf_menu, int isel){
	set_color_mode_by_id(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz], isel);
	return;
}
int get_PSF_colormap_id(struct psf_menu_val *psf_menu){
	return get_color_mode_id_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
};

double send_VIZ_data_min(struct psf_data *psf_d, int icomp){return psf_d->d_min[icomp];};
double send_VIZ_data_max(struct psf_data *psf_d, int icomp){return psf_d->d_max[icomp];};

void delete_VIZ_color_index_list(struct psf_menu_val *psf_menu, int i_delete){
    delete_color_index_list_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz], i_delete);
	return;
}
void delete_VIZ_opacity_index_list(struct psf_menu_val *psf_menu, int i_delete){
    delete_opacity_index_list_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz], i_delete);
	return;
}

void add_VIZ_color_index_list(struct psf_menu_val *psf_menu,
                              double add_value, double add_color){
    add_color_index_list_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                           add_value, add_color);
	return;
}
void add_VIZ_opacity_index_list(struct psf_menu_val *psf_menu,
                                double add_value, double add_opacity){
    add_opacity_index_list_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                             add_value, add_opacity);
    return;
}

void set_VIZ_linear_colormap(double minvalue, int i_min_digit,
                             double maxvalue, int i_max_digit,
                             struct psf_menu_val *psf_menu){
	set_linear_colormap(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz], 
						const_from_digit_order(minvalue, i_min_digit),
						const_from_digit_order(maxvalue, i_max_digit));
	return;
}

void set_VIZ_fixed_color(struct psf_data *psf_d,
                         struct psf_menu_val *psf_menu,
                         double *rgba){
    long icomp = psf_menu->icomp_draw_viz;
    set_rgb_from_rgb(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz], rgba[0], rgba[1], rgba[2]);	
    set_constant_opacitymap(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                            psf_d->d_min[icomp], psf_d->d_max[icomp], rgba[3]);
    return;
}

void set_VIZ_constant_opacity(struct psf_data *psf_d,
                              struct psf_menu_val *psf_menu,
                              double opacity){
	long icomp = psf_menu->icomp_draw_viz;
	set_constant_opacitymap(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                            psf_d->d_min[icomp], psf_d->d_max[icomp], opacity);
    return;
}

void get_VIZ_rgb_from_value(struct psf_menu_val *psf_menu,
                            double value, double *red, double *green, double *blue){
    struct colormap_params *cmap_s = psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    double rnorm = color_normalize_linear_segment_c(cmap_array->num,
                                                    cmap_array->data,
                                                    cmap_array->value, value);
    cal_rgb_from_value_s(cmap_s->id_color_mode, rnorm,
                         red, green, blue);
    dealloc_colormap_array(cmap_array);
	return;
}
double get_VIZ_opacity_at_value(struct psf_menu_val *psf_menu, double value){
    struct colormap_params *cmap_s = psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz];
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	return set_opacity_from_value_s(omap_array, value);
    dealloc_colormap_array(omap_array);
}
void set_VIZ_color_point(struct psf_menu_val *psf_menu, int i_point,
                         double value, double color){
    set_each_color_point_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                           i_point, value, color);
	return;
}
void set_VIZ_opacity_point(struct psf_menu_val *psf_menu, int i_point,
                           double value, double opacity){
    set_each_opacity_point_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                             i_point, value, opacity);
	return;
}

double get_VIZ_color_table_min(struct psf_menu_val *psf_menu){
	double d, c;
    get_color_table_items_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                            0, &d, &c);
	return d;
}
double get_each_PSF_color_table_max(struct psf_menu_val *psf_menu){
	double d, c;
	int n = get_color_table_num_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
    get_color_table_items_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                            n-1, &d, &c);
	return d;
}
double get_VIZ_minimum_opacity(struct psf_menu_val *psf_menu){
	return get_minimum_opacity_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
}
double get_VIZ_maximum_opacity(struct psf_menu_val *psf_menu){
	return get_maximum_opacity_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
}
int get_VIZ_color_table_num(struct psf_menu_val *psf_menu){
	return get_color_table_num_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
}
int get_VIZ_opacity_table_num(struct psf_menu_val *psf_menu){
	return get_opacity_table_num_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
}

void get_VIZ_color_RGB_value(struct psf_menu_val *psf_menu, int i_point,
                             double *value, double *color){
    get_color_table_items_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz],
                            i_point, value, color);
}
void get_VIZ_opacity_table_items(struct psf_menu_val *psf_menu, int i_point,
                                 double *value, double *opacity){
    get_opacity_table_items_s(psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz], i_point, value, opacity);
}

void write_VIZ_colormap_control_file(struct kv_string *filename, const int iflag_draw_axis,
                                     struct psf_menu_val *psf_menu){
	write_colormap_control_file_s(filename->string, psf_menu->iflag_draw_time,
                                  iflag_draw_axis, psf_menu->iflag_draw_cbar,
                                  psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
}
void read_VIZ_colormap_control_file(struct kv_string *filename,
                                    struct psf_menu_val *psf_menu){
	read_colormap_control_file_s(filename->string, psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
}

void check_each_PSF_colormap_control(int iflag_draw_axis, struct psf_menu_val *psf_menu){
	check_colormap_control_file_s(psf_menu->iflag_draw_time, iflag_draw_axis, 
                                  psf_menu->iflag_draw_cbar, 
                                  psf_menu->cmap_viz_comp[psf_menu->icomp_draw_viz]);
}
