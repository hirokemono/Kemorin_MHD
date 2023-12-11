/*
//  set_each_psf_parameters.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include <stdio.h>
#include "set_each_psf_parameters.h"

int send_each_psf_file_header_full(struct psf_menu_val *psf_menu, 
			struct kv_string *psf_filehead, int *iflag){
	alloc_copy_string(psf_menu->psf_header->string, psf_filehead);
	*iflag = psf_menu->iflag_psf_file;
	return psf_menu->psf_step;
};

int send_each_psf_file_dir_head(struct psf_menu_val *psf_menu,
			struct kv_string *stripped_dir, struct kv_string *stripped_filehead){
	alloc_kvstringitem(strlen(psf_menu->psf_header->string), stripped_filehead);
	alloc_kvstringitem(strlen(psf_menu->psf_header->string), stripped_dir);
	split_dir_and_file_name_c(psf_menu->psf_header->string,
				stripped_dir->string, stripped_filehead->string);
	return psf_menu->psf_step;
};



int send_nfield_each_psf(struct psf_data *psf_d){return psf_d->nfield;};
int send_ncomptot_each_psf(struct psf_data *psf_d){return psf_d->ncomptot;};
int send_ncomp_each_psf(struct psf_data *psf_d, int i){return psf_d->ncomp[i];};
int send_istack_each_comp_psf(struct psf_data *psf_d, int i){return psf_d->istack_comp[i];};
void send_each_psf_data_name(struct psf_data *psf_d, struct kv_string *colorname, int i){
	alloc_copy_string(psf_d->data_name[i], colorname);
};


int send_field_draw_each_psf(struct psf_menu_val *psf_menu){return psf_menu->if_draw_psf;};
int send_draw_comp_id_psf(struct psf_menu_val *psf_menu){return psf_menu->ic_draw_psf;};
int send_draw_component_psf(struct psf_menu_val *psf_menu){return psf_menu->icomp_draw_psf;};
int send_coordinate_id_psf(struct psf_data *psf_d, struct psf_menu_val *psf_menu){
	int id_current = psf_menu->if_draw_psf;
	return psf_d->id_coord[id_current];
};

void set_texture_psf_from_bgra(struct kemo_array_control *psf_a,
                               int width, int height, const unsigned char *bgra_in){
    set_texture_4_psf(width, height, bgra_in, psf_a->psf_texure);
};

void set_psf_polygon_mode(struct psf_menu_val *psf_menu, int iflag){psf_menu->polygon_mode_psf = iflag;};
int send_each_psf_polygon_mode(struct psf_menu_val *psf_menu){return psf_menu->polygon_mode_psf;};
int toggle_each_psf_polygon_mode(struct psf_menu_val *psf_menu){
	psf_menu->polygon_mode_psf = toggle_value_c(psf_menu->polygon_mode_psf);
	return psf_menu->polygon_mode_psf;
};

void set_psf_vector_mode(struct psf_menu_val *psf_menu, int iflag){psf_menu->ivect_tangential = iflag;};
int send_each_psf_vector_mode(struct psf_menu_val *psf_menu){return psf_menu->ivect_tangential;};
int toggle_each_psf_vector_mode(struct psf_menu_val *psf_menu){
	psf_menu->ivect_tangential = toggle_value_c(psf_menu->ivect_tangential);
	return psf_menu->ivect_tangential;
};

int send_draw_psf_solid(struct psf_menu_val *psf_menu){return psf_menu->draw_psf_solid;};
int toggle_draw_psf_solid(struct psf_menu_val *psf_menu){
	psf_menu->draw_psf_solid = toggle_value_c(psf_menu->draw_psf_solid);
	return psf_menu->draw_psf_solid;
};

int send_draw_psf_grid(struct psf_menu_val *psf_menu) {return psf_menu->draw_psf_grid;};
int toggle_draw_psf_grid(struct psf_menu_val *psf_menu){
	psf_menu->draw_psf_grid = toggle_value_c(psf_menu->draw_psf_grid);
	return psf_menu->draw_psf_grid;
};

int send_draw_psf_zero(struct psf_menu_val *psf_menu) {return psf_menu->draw_psf_zero;};
int toggle_draw_psf_zero(struct psf_menu_val *psf_menu){
	psf_menu->draw_psf_zero = toggle_value_c(psf_menu->draw_psf_zero);
	return psf_menu->draw_psf_zero;
};

int send_draw_psf_cbar(struct psf_menu_val *psf_menu) {return psf_menu->draw_psf_cbar;};
int toggle_draw_psf_cbar(struct psf_menu_val *psf_menu){
	psf_menu->draw_psf_cbar = toggle_value_c(psf_menu->draw_psf_cbar);
	return psf_menu->draw_psf_cbar;
};

int send_draw_psf_vect(struct psf_menu_val *psf_menu) {return psf_menu->draw_psf_vect;};
int toggle_draw_psf_vect(struct psf_menu_val *psf_menu){
	psf_menu->draw_psf_vect = toggle_value_c(psf_menu->draw_psf_vect);
	return psf_menu->draw_psf_vect;
};

int send_draw_psf_refv(struct psf_menu_val *psf_menu){return psf_menu->draw_psf_refv;};
int toggle_draw_psf_refv(struct psf_menu_val *psf_menu){
	psf_menu->draw_psf_refv = toggle_value_c(psf_menu->draw_psf_refv);
	return psf_menu->draw_psf_refv;
};

void set_psf_patch_color_mode(struct kemo_array_control *psf_a,
                              struct psf_menu_val *psf_menu, int iflag){
	psf_menu->psf_patch_color = iflag;
    if(iflag != TEXTURED_SURFACE){
        psf_a->psf_texure->ipsf_texured = -1;
    };
	return;
};

void set_each_isoline_color(struct psf_menu_val *psf_menu, int iflag)     {psf_menu->isoline_color = iflag;};
void set_each_n_isoline(struct psf_menu_val *psf_menu, int nlline)        {psf_menu->n_isoline = nlline;};
void set_each_isoline_width(double value, struct psf_menu_val *psf_menu){psf_menu->isoline_width = value;};
void set_each_vector_patch_color(struct psf_menu_val *psf_menu, int iflag){psf_menu->vector_patch_color = iflag;};

void set_each_increment_vect(int increment, struct psf_menu_val *psf_menu){
    if(increment > 0) psf_menu->increment_vect = increment;
	return;
};
void set_each_scale_vect(double value, struct psf_menu_val *psf_menu)  {psf_menu->scale_vect = value;};
void set_each_vector_thick(double value, struct psf_menu_val *psf_menu){psf_menu->vector_thick = value;};

int send_each_psf_patch_color(struct psf_menu_val *psf_menu)   {return psf_menu->psf_patch_color;};
int send_each_isoline_color(struct psf_menu_val *psf_menu)     {return psf_menu->isoline_color;};
int send_num_isoline(struct psf_menu_val *psf_menu)            {return psf_menu->n_isoline;};
double send_isoline_width(struct psf_menu_val *psf_menu)       {return psf_menu->isoline_width;};
int send_each_vector_patch_color(struct psf_menu_val *psf_menu){return psf_menu->vector_patch_color;};

int send_each_increment_vect(struct psf_menu_val *psf_menu){return psf_menu->increment_vect;};
double send_scale_vector(struct psf_menu_val *psf_menu){return psf_menu->scale_vect;};
double send_vector_thick(struct psf_menu_val *psf_menu){return psf_menu->vector_thick;};


void set_PSF_colormap_id(struct psf_menu_val *psf_menu, int isel){
	set_color_mode_by_id(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], isel);
	return;
}
int send_PSF_colormap_id(struct psf_menu_val *psf_menu){
	return send_color_mode_id_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
};

double send_psf_data_min(struct psf_data *psf_d, int icomp){return psf_d->d_min[icomp];};
double send_psf_data_max(struct psf_data *psf_d, int icomp){return psf_d->d_max[icomp];};

void delete_PSF_color_index_list(struct psf_menu_val *psf_menu, int i_delete){
    delete_color_index_list_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], i_delete);
	return;
}
void delete_PSF_opacity_index_list(struct psf_menu_val *psf_menu, int i_delete){
    delete_opacity_index_list_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], i_delete);
	return;
}

void add_PSF_color_index_list(struct psf_menu_val *psf_menu, double add_value, double add_color){
    add_color_index_list_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], add_value, add_color);
	return;
}
void add_PSF_opacity_index_list(struct psf_menu_val *psf_menu, double add_value, double add_opacity){
    add_opacity_index_list_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], add_value, add_opacity);
    return;
}



void set_PSF_linear_colormap(double minvalue, int i_min_digit, double maxvalue, int i_max_digit,
							 struct psf_menu_val *psf_menu){
	set_linear_colormap(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], 
						const_from_digit_order(minvalue, i_min_digit),
						const_from_digit_order(maxvalue, i_max_digit));
	return;
}

void set_PSF_fixed_color(struct psf_data *psf_d, struct psf_menu_val *psf_menu,
                         double *rgba){
    int icomp = psf_menu->icomp_draw_psf;
    set_rgb_from_rgb(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], rgba[0], rgba[1], rgba[2]);	
    set_constant_opacitymap(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf],
                            psf_d->d_min[icomp], psf_d->d_max[icomp], rgba[3]);
    return;
}

void set_PSF_constant_opacity(struct psf_data *psf_d, struct psf_menu_val *psf_menu,
                                 double opacity){
	int icomp = psf_menu->icomp_draw_psf;
	set_constant_opacitymap(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf],
                            psf_d->d_min[icomp], psf_d->d_max[icomp], opacity);
    return;
}

void set_PSF_rgb_from_value(struct psf_menu_val *psf_menu,
                            double value, double *red, double *green, double *blue){
	set_rgb_from_value_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf],
                         value, red, green, blue);
	return;
}
double get_PSF_opacity_at_value(struct psf_menu_val *psf_menu, double value){
	return set_opacity_from_value_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], value);
}
void set_each_PSF_color_point(struct psf_menu_val *psf_menu, int i_point, double value, double color){
	set_each_color_point_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], i_point, value, color);
	return;
}
void set_each_PSF_opacity_point(struct psf_menu_val *psf_menu, int i_point, double value, double opacity){
	set_each_opacity_point_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], i_point, value, opacity);
	return;
}

double send_each_PSF_color_table_min(struct psf_menu_val *psf_menu){
	double d, c;
	send_color_table_items_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], 0, &d, &c);
	return d;
}
double send_each_PSF_color_table_max(struct psf_menu_val *psf_menu){
	double d, c;
	int n = send_color_table_num_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
	send_color_table_items_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], n-1, &d, &c);
	return d;
}
double send_each_PSF_minimum_opacity(struct psf_menu_val *psf_menu){
	return send_minimum_opacity_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
}
double send_each_PSF_maximum_opacity(struct psf_menu_val *psf_menu){
	return send_maximum_opacity_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
}
int send_each_PSF_color_table_num(struct psf_menu_val *psf_menu){
	return send_color_table_num_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
}
int send_each_PSF_opacity_table_num(struct psf_menu_val *psf_menu){
	return send_opacity_table_num_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
}

void send_each_PSF_color_table_items(struct psf_menu_val *psf_menu, int i_point, double *value, double *color){
	send_color_table_items_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], i_point, value, color);
}
void send_each_PSF_opacity_table_items(struct psf_menu_val *psf_menu, int i_point, double *value, double *opacity){
	send_opacity_table_items_s(psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf], i_point, value, opacity);
}

void write_each_PSF_colormap_control_file(const char *file_name, const int iflag_draw_axis, 
                                          struct psf_menu_val *psf_menu){
	write_colormap_control_file_s(file_name, psf_menu->iflag_draw_time, iflag_draw_axis, psf_menu->draw_psf_cbar,
                                  psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
}
void read_each_PSF_colormap_control_file(struct psf_menu_val *psf_menu, const char *file_name){
	read_colormap_control_file_s(file_name, psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
}

void check_each_PSF_colormap_control(int iflag_draw_axis, struct psf_menu_val *psf_menu){
	check_colormap_control_file_s(psf_menu->iflag_draw_time, iflag_draw_axis, psf_menu->draw_psf_cbar, 
                                  psf_menu->cmap_psf_comp[psf_menu->icomp_draw_psf]);
}
