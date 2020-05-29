/*
// set_rgba_table_c.c
*/

#include <stdio.h>
#include <stdlib.h>
#include "set_rgba_table_c.h"


const char *label_rainbow =   "rainbow";
const char *label_bluered =   "blue_to_red";
const char *label_grayscale = "grayscale";
const char *label_sym_gray  = "symmetric_grayscale";

const char color_labels[4][KCHARA_C] = {
    "rainbow", 
    "grayscale",
    "blue_to_red",
    "symmetric_grayscale"
};

struct pvr_colormap_bar_ctl_c *cmap_cbar_c0;

void copy_colormap_name_to_ctl(struct colormap_params *cmap_s, 
			struct chara_ctl_item *colormap_mode){	
	if(cmap_s->id_color_mode == RED_BLUE_MODE){
		copy_to_chara_ctl_item(label_bluered, colormap_mode);
	} else if(cmap_s->id_color_mode == GRAYSCALE_MODE){
		copy_to_chara_ctl_item(label_grayscale, colormap_mode);
	} else if(cmap_s->id_color_mode == SYM_GRAY_MODE){
		copy_to_chara_ctl_item(label_sym_gray, colormap_mode);
	} else {
		copy_to_chara_ctl_item(label_rainbow, colormap_mode);
	};
	return;
};

void set_rgb_from_value_s(struct colormap_params *cmap_s,
			double value, double *red, double *green, double *blue){
	struct colormap_array *cmap_tmp = init_colormap_from_list(cmap_s->colormap);
	double rnorm = color_normalize_linear_segment_c(cmap_tmp->num, 
			cmap_tmp->data, cmap_tmp->value, value);
	dealloc_colormap_array(cmap_tmp);
	
	if(cmap_s->id_color_mode == GRAYSCALE_MODE){
		color_grayscale_c(rnorm, red, green, blue);
	} else if(cmap_s->id_color_mode == SYM_GRAY_MODE){
		color_sym_grayscale_c(rnorm, red, green, blue);
	} else if(cmap_s->id_color_mode == RED_BLUE_MODE){
		color_redblue_c(rnorm, red, green, blue);
	} else {
        color_rainbow_c(rnorm, red, green, blue);
	}
	return;
}

void set_rgb_from_rgb(struct colormap_params *cmap_s,
                      double red, double green, double blue){
    
    cmap_s->single_color[0] = red;
    cmap_s->single_color[1] = green;
    cmap_s->single_color[2] = blue;
    return;
}

double set_opacity_from_value_s(struct colormap_params *cmap_s, double value){
	struct colormap_array *cmap_tmp = init_colormap_from_list(cmap_s->opacitymap);
	double rnorm = color_normalize_linear_segment_c(cmap_tmp->num,
				cmap_tmp->data, cmap_tmp->value, value);
	dealloc_colormap_array(cmap_tmp);
	return rnorm;
}

void set_each_color_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double color){
	update_real2_clist_by_index(i_point, value, color, cmap_s->colormap);
	return;
}

void set_each_opacity_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double opacity){
	int i;
	double d, v;
	
	update_real2_clist_by_index(i_point, value, opacity, cmap_s->opacitymap);
	
	cmap_s->max_opacity = opacity;
	cmap_s->min_opacity = opacity;
	for (i=0;i<count_real2_clist(cmap_s->opacitymap);i++){
		set_from_real2_clist_at_index(i, cmap_s->opacitymap, &d, &v);
		if(v > cmap_s->max_opacity){cmap_s->max_opacity = v;};
		if(v < cmap_s->min_opacity){cmap_s->min_opacity = v;};
	};
	return;
}

void set_color_mode_by_id(struct colormap_params *cmap_s, int isel){
	cmap_s->id_color_mode = isel;
	return;
}

double send_minimum_opacity_s(struct colormap_params *cmap_s){return cmap_s->min_opacity;}
double send_maximum_opacity_s(struct colormap_params *cmap_s){return cmap_s->max_opacity;}
int send_color_mode_id_s(struct colormap_params *cmap_s){return cmap_s->id_color_mode;}
int send_color_table_num_s(struct colormap_params *cmap_s){
	return count_real2_clist(cmap_s->colormap);
};
int send_opacity_table_num_s(struct colormap_params *cmap_s){
	return count_real2_clist(cmap_s->opacitymap);
};

void send_color_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *color){
	set_from_real2_clist_at_index(i_point, cmap_s->colormap, value, color);
	return;
}

void send_opacity_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *opacity){
	set_from_real2_clist_at_index(i_point, cmap_s->opacitymap, value, opacity);
	return;
}

void set_linear_colormap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	clear_real2_clist(cmap_s->colormap);
	append_real2_clist(val_min, ZERO, cmap_s->colormap);
	append_real2_clist(val_max, ONE,  cmap_s->colormap);
	return;
}
void set_constant_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max, double opacity){
	clear_real2_clist(cmap_s->opacitymap);
	append_real2_clist(val_min, opacity, cmap_s->opacitymap);
	append_real2_clist(val_max, opacity, cmap_s->opacitymap);
	cmap_s->max_opacity = opacity;
	cmap_s->min_opacity = opacity;
	return;
}
void set_full_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	set_constant_opacitymap(cmap_s, val_min, val_max, ONE);
	return;
}


static void copy_color_opacity_to_ctl(struct colormap_params *cmap_s, 
			struct colormap_ctl_c *cmap_c){
	int i;
	double color;
	double d, v;
	
	copy_colormap_name_to_ctl(cmap_s, cmap_c->colormap_mode_ctl);
	copy_to_chara_ctl_item("colormap_list", cmap_c->data_mapping_ctl);
	
	dup_real2_clist(cmap_s->colormap, cmap_c->colortbl_list);
	
	copy_to_chara_ctl_item("point_linear", cmap_c->opacity_style_ctl);
	struct colormap_array *cmap_tmp = init_colormap_from_list(cmap_s->colormap);
	for(i=0; i<count_real2_clist(cmap_s->opacitymap); i++){
		set_from_real2_clist_at_index(i, cmap_s->opacitymap, &d, &v);
		color = color_normalize_linear_segment_c(cmap_tmp->num, 
					 cmap_tmp->data, cmap_tmp->value, d);
	}
	dealloc_colormap_array(cmap_tmp);
	
	dup_real2_clist(cmap_s->opacitymap, cmap_c->colortbl_list);
	update_real_ctl_item_c(cmap_s->min_opacity, cmap_c->fix_opacity_ctl);
	return;
	}

void make_colorbar_for_ctl(struct colormap_params *cmap_s, 
			struct pvr_colorbar_ctl_c *cbar_c){
	int num;
	double d1, v1, d2, v2;
	set_boolean_by_chara_ctl_item(1, cbar_c->colorbar_switch_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->colorbar_scale_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->zeromarker_flag_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->axis_switch_ctl);
	
	num = count_real2_clist(cmap_s->colormap);
	set_from_real2_clist_at_index(0,     cmap_s->colormap, &d1, &v1);
	set_from_real2_clist_at_index(num-1, cmap_s->colormap, &d2, &v2);
	update_real2_ctl_item_c(d1, d2, cbar_c->cbar_range_ctl);
	update_int_ctl_item_c(1, cbar_c->font_size_ctl);
	update_int_ctl_item_c(3, cbar_c->ngrid_cbar_ctl);

	return;
}


void copy_colormap_from_ctl(struct chara_ctl_item *colormap_mode_ctl, 
			struct real2_clist *colortbl_list, struct colormap_params *cmap_s){
	if(compare_string(11, label_bluered, colormap_mode_ctl->c_tbl) > 0){
		cmap_s->id_color_mode = RED_BLUE_MODE;
	} else if(compare_string(9, label_grayscale, colormap_mode_ctl->c_tbl) > 0){
		cmap_s->id_color_mode = GRAYSCALE_MODE;
	} else if(compare_string(18, label_sym_gray, colormap_mode_ctl->c_tbl) > 0){
		cmap_s->id_color_mode = SYM_GRAY_MODE;
	} else {
		cmap_s->id_color_mode = RAINBOW_MODE;
	};
   	
	dup_real2_clist(colortbl_list, cmap_s->colormap);
	return;
}
void copy_opacity_from_ctl(struct real2_clist *linear_opacity_list, 
			struct colormap_params *cmap_s){
	dup_real2_clist(linear_opacity_list, cmap_s->opacitymap);
	return;
}

static void copy_color_opacity_from_ctl(struct colormap_ctl_c *cmap_c, 
			struct colormap_params *cmap_s){
	if(compare_string(13, "colormap_list", cmap_c->data_mapping_ctl->c_tbl) == 0){
		printf("Something Wrong in colormap file\n)");
		return;
	}
	
	if(compare_string(12, "point_linear", cmap_c->opacity_style_ctl->c_tbl) == 0){
		printf("Something Wrong in opacity_style_ctl\n)");
		return;
	};
	copy_colormap_from_ctl(cmap_c->colormap_mode_ctl, 
				cmap_c->colortbl_list, cmap_s);
	copy_opacity_from_ctl(cmap_c->linear_opacity_list, cmap_s);
    set_from_real_ctl_item_c(cmap_c->fix_opacity_ctl, &cmap_s->min_opacity);
	
	return;
}


void check_colormap_control_file_s(struct colormap_params *cmap_s){
	cmap_cbar_c0 = init_colormap_colorbar_ctl_c();
	
	cmap_cbar_c0->cmap_c->iflag_use = 1;
	copy_color_opacity_to_ctl(cmap_s, cmap_cbar_c0->cmap_c);
	cmap_cbar_c0->cbar_c->iflag_use = 1;
	make_colorbar_for_ctl(cmap_s, cmap_cbar_c0->cbar_c);
	
	write_colormap_colorbar_ctl_c(stdout, 0, 
				"Colormap data", cmap_cbar_c0);
	dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	
    return;
}

void write_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s){
	cmap_cbar_c0 = init_colormap_colorbar_ctl_c();
	
	cmap_cbar_c0->cmap_c->iflag_use = 1;
	copy_color_opacity_to_ctl(cmap_s, cmap_cbar_c0->cmap_c);
	cmap_cbar_c0->cbar_c->iflag_use = 1;
	make_colorbar_for_ctl(cmap_s, cmap_cbar_c0->cbar_c);
	
	write_colormap_file_c(file_name, cmap_cbar_c0);
	dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	
    return;
}

void read_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s){
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
	cmap_cbar_c0 = init_colormap_colorbar_ctl_c();
	read_colormap_file_c(file_name, buf, cmap_cbar_c0);
	copy_color_opacity_from_ctl(cmap_cbar_c0->cmap_c, cmap_s);
	
	dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	
	return;
}
