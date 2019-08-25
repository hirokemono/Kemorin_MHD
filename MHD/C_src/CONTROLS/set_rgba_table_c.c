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
	int i;
	
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
	double rnorm;
	rnorm = color_normalize_linear_segment_c(cmap_s->n_color_point, 
			cmap_s->color_data, cmap_s->color_value, value);
	
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
	return color_normalize_linear_segment_c(cmap_s->n_opacity_point, cmap_s->opacity_data, 
                                            cmap_s->opacity_value, value);
}

void set_each_color_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double color){
	cmap_s->color_data[i_point] = value;
	cmap_s->color_value[i_point] = color;
	
	return;
}

void set_each_opacity_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double opacity){
	int i;
	cmap_s->opacity_data[i_point] = value;
	cmap_s->opacity_value[i_point] = opacity;
	
	cmap_s->max_opacity = cmap_s->opacity_value[0];
	cmap_s->min_opacity = cmap_s->opacity_value[0];
	for (i=1; i<cmap_s->n_opacity_point; i++) {
		if(cmap_s->opacity_value[i] > cmap_s->max_opacity){
			cmap_s->max_opacity = cmap_s->opacity_value[i];
		}
		if(cmap_s->opacity_value[i] < cmap_s->min_opacity){
			cmap_s->min_opacity = cmap_s->opacity_value[i];
		}
		
	}
	return;
}

void set_color_mode_by_id(struct colormap_params *cmap_s, int isel){
	cmap_s->id_color_mode = isel;
	return;
}

double send_minimum_opacity_s(struct colormap_params *cmap_s){return cmap_s->min_opacity;}
double send_maximum_opacity_s(struct colormap_params *cmap_s){return cmap_s->max_opacity;}
int send_color_mode_id_s(struct colormap_params *cmap_s){return cmap_s->id_color_mode;}
int send_color_table_num_s(struct colormap_params *cmap_s){return cmap_s->n_color_point;}
int send_opacity_table_num_s(struct colormap_params *cmap_s){return cmap_s->n_opacity_point;}

void send_color_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *color){
	*value =   cmap_s->color_data[i_point];
	*color =   cmap_s->color_value[i_point];
	return;
}

void send_opacity_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *opacity){
	*value =   cmap_s->opacity_data[i_point];
	*opacity = cmap_s->opacity_value[i_point];
	return;
}

void set_linear_colormap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	realloc_color_index_list_s(cmap_s, ITWO);
	set_each_color_point_s(cmap_s, IZERO, val_min, ZERO);
	set_each_color_point_s(cmap_s, IONE,  val_max, ONE);
	return;
}
void set_constant_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max, double opaciy){
	realloc_opacity_index_list_s(cmap_s, ITWO);
	set_each_opacity_point_s(cmap_s, IZERO, val_min, opaciy);
	set_each_opacity_point_s(cmap_s, IONE,  val_max, opaciy);
	return;
}
void set_full_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	set_constant_opacitymap(cmap_s, val_min, val_max, ONE);
	return;
}


void copy_color_opacity_to_ctl(struct colormap_params *cmap_s, 
			struct colormap_ctl_c *cmap_c){
	int i;
	double color;
	
	copy_colormap_name_to_ctl(cmap_s, cmap_c->colormap_mode_ctl);
	copy_to_chara_ctl_item("colormap_list", cmap_c->data_mapping_ctl);
	
	copy_to_real2_clist(cmap_s->n_color_point, cmap_s->color_data, cmap_s->color_value,
				cmap_c->colortbl_list);
	
	copy_to_chara_ctl_item("point_linear", cmap_c->opacity_style_ctl);
	for(i=0; i<cmap_s->n_opacity_point; i++){
		color = color_normalize_linear_segment_c(cmap_s->n_color_point, 
					 cmap_s->color_data, cmap_s->color_value, cmap_s->opacity_data[i]);
	}
	copy_to_real2_clist(cmap_s->n_opacity_point, cmap_s->opacity_data, cmap_s->opacity_value,
				cmap_c->linear_opacity_list);
	update_real_ctl_item_c(cmap_s->min_opacity, cmap_c->fix_opacity_ctl);
	return;
	}

void make_colorbar_for_ctl(struct colormap_params *cmap_s, 
			struct pvr_colorbar_ctl_c *cbar_c){
	
	set_boolean_by_chara_ctl_item(1, cbar_c->colorbar_switch_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->colorbar_scale_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->zeromarker_flag_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->axis_switch_ctl);
	
	update_real2_ctl_item_c(cmap_s->color_data[IZERO], cmap_s->color_data[cmap_s->n_color_point-1],
				cbar_c->cbar_range_ctl);
	update_int_ctl_item_c(1, cbar_c->font_size_ctl);
	update_int_ctl_item_c(3, cbar_c->ngrid_cbar_ctl);

	return;
}


void copy_colormap_from_ctl(struct chara_ctl_item *colormap_mode_ctl, 
			struct real2_clist *colortbl_list, struct colormap_params *cmap_s){
	int i, num;
	double v, d;
	
	if(compare_string(11, label_bluered, colormap_mode_ctl->c_tbl) > 0){
		cmap_s->id_color_mode = RED_BLUE_MODE;
	} else if(compare_string(9, label_grayscale, colormap_mode_ctl->c_tbl) > 0){
		cmap_s->id_color_mode = GRAYSCALE_MODE;
	} else if(compare_string(18, label_sym_gray, colormap_mode_ctl->c_tbl) > 0){
		cmap_s->id_color_mode = SYM_GRAY_MODE;
	} else {
		cmap_s->id_color_mode = RAINBOW_MODE;
	};
   	
	num = count_real2_clist(colortbl_list);
	if(num > cmap_s->nbuf_color_point) realloc_color_index_list_s(cmap_s, num);
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, colortbl_list, &v, &d);
		cmap_s->color_data[i] = v;
		cmap_s->color_value[i] = d;
	};
	return;
}
void copy_opacity_from_ctl(struct real2_clist *linear_opacity_list, 
			struct colormap_params *cmap_s){
	int i, num;
	double v, d;
	num = count_real2_clist(linear_opacity_list);
	if(num > cmap_s->nbuf_opacity_point) realloc_opacity_index_list_s(cmap_s, num);
	for(i=0;i<num;i++){
		set_from_real2_clist_at_index(i, linear_opacity_list, &v, &d);
		cmap_s->opacity_data[i] = v;
		cmap_s->opacity_value[i] = d;
	};
	return;
}

void copy_color_opacity_from_ctl(struct colormap_ctl_c *cmap_c, 
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
	cmap_cbar_c0 = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c));
	alloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	
	cmap_cbar_c0->cmap_c->iflag_use = 1;
	copy_color_opacity_to_ctl(cmap_s, cmap_cbar_c0->cmap_c);
	cmap_cbar_c0->cbar_c->iflag_use = 1;
	make_colorbar_for_ctl(cmap_s, cmap_cbar_c0->cbar_c);
	
	write_colormap_colorbar_ctl_c(stdout, 0, 
				"Colormap data", cmap_cbar_c0);
	dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	free(cmap_cbar_c0);
	
    return;
}

void write_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s){
	cmap_cbar_c0 = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c));
	alloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	
	cmap_cbar_c0->cmap_c->iflag_use = 1;
	copy_color_opacity_to_ctl(cmap_s, cmap_cbar_c0->cmap_c);
	cmap_cbar_c0->cbar_c->iflag_use = 1;
	make_colorbar_for_ctl(cmap_s, cmap_cbar_c0->cbar_c);
	
	write_colormap_file_c(file_name, cmap_cbar_c0);
	dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	free(cmap_cbar_c0);
	
    return;
}

void read_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s){
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
	cmap_cbar_c0 = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c));
	alloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	read_colormap_file_c(file_name, buf, cmap_cbar_c0);
	copy_color_opacity_from_ctl(cmap_cbar_c0->cmap_c, cmap_s);
	
	dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	free(cmap_cbar_c0);
	
	return;
}
