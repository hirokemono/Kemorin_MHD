
/* set_rgba_table_c.c */

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

void set_rgb_from_value_s(const char *color_mode_name, struct real2_clist *colormap_clist,
			double value, double *red, double *green, double *blue){
	double rnorm;
	rnorm = color_normalize_linear_segment_c(colormap_clist, value);
	
	if(cmp_no_case_c(color_mode_name, label_grayscale) > 0){
		color_grayscale_c(rnorm, red, green, blue);
	} else if(cmp_no_case_c(color_mode_name, label_sym_gray) > 0){
		color_sym_grayscale_c(rnorm, red, green, blue);
	} else if(cmp_no_case_c(color_mode_name, label_bluered) > 0){
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

void set_each_color_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double color){
	update_real2_clist_by_index(i_point, value, color, cmap_s->colormap_clist);
	return;
}

void set_each_opacity_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double opacity){
	struct real2_ctl_list *head_cmap;
    update_real2_clist_by_index(i_point, value, opacity, cmap_s->opacitymap_clist);
	
    head_cmap = &cmap_s->opacitymap_clist->r2_item_head;
    head_cmap = head_cmap->_next;
    cmap_s->max_opacity = head_cmap->r2_item->r_data[1];
	cmap_s->min_opacity = head_cmap->r2_item->r_data[1];
    while (head_cmap != NULL){
		if(head_cmap->r2_item->r_data[1] > cmap_s->max_opacity){
			cmap_s->max_opacity = head_cmap->r2_item->r_data[1];
		}
		if(head_cmap->r2_item->r_data[1] < cmap_s->min_opacity){
			cmap_s->min_opacity = head_cmap->r2_item->r_data[1];
		}
        head_cmap = head_cmap->_next;
	}
	return;
}

void set_color_mode_by_id(struct colormap_params *cmap_s, int isel){
	if(isel == RED_BLUE_MODE){
		copy_to_chara_ctl_item(label_bluered, cmap_s->colormap_mode);
	} else if(isel == GRAYSCALE_MODE){
		copy_to_chara_ctl_item(label_grayscale, cmap_s->colormap_mode);
	} else if(isel == SYM_GRAY_MODE){
		copy_to_chara_ctl_item(label_sym_gray, cmap_s->colormap_mode);
	} else {
		copy_to_chara_ctl_item(label_rainbow, cmap_s->colormap_mode);
	};
	return;
}

double send_minimum_opacity_s(struct colormap_params *cmap_s){return cmap_s->min_opacity;}
double send_maximum_opacity_s(struct colormap_params *cmap_s){return cmap_s->max_opacity;}
int send_color_mode_id_s(struct colormap_params *cmap_s){
	int isel;
	if(cmp_no_case_c(cmap_s->colormap_mode->c_tbl, label_grayscale) > 0){
		isel = GRAYSCALE_MODE;
	} else if(cmp_no_case_c(cmap_s->colormap_mode->c_tbl, label_sym_gray) > 0){
		isel = SYM_GRAY_MODE;
	} else if(cmp_no_case_c(cmap_s->colormap_mode->c_tbl, label_bluered) > 0){
		isel = RED_BLUE_MODE;
	} else {
		isel = RAINBOW_MODE;
	}
	return isel;
}
int send_color_table_num_s(struct colormap_params *cmap_s){
    return count_real2_clist(cmap_s->colormap_clist);
}
int send_opacity_table_num_s(struct colormap_params *cmap_s){
    return count_real2_clist(cmap_s->opacitymap_clist);
}

void send_color_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *color){
	set_from_real2_clist_at_index(i_point, cmap_s->colormap_clist, value, color);
	return;
}

void send_opacity_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *opacity){
	set_from_real2_clist_at_index(i_point, cmap_s->opacitymap_clist, value, opacity);
	return;
}

void set_linear_colormap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	clear_real2_clist(cmap_s->colormap_clist);
	init_real2_clist(cmap_s->colormap_clist);
    sprintf(cmap_s->colormap_clist->r1_name, "data");
    sprintf(cmap_s->colormap_clist->r2_name, "color");
	
	append_real2_clist(val_min, ZERO, cmap_s->colormap_clist);
	append_real2_clist(val_max, ONE, cmap_s->colormap_clist);
    update_real_ctl_item_c(val_min, cmap_s->range_min);
    update_real_ctl_item_c(val_max, cmap_s->range_max);
	return;
}
void set_constant_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max, double opaciy){
	clear_real2_clist(cmap_s->opacitymap_clist);
	init_real2_clist(cmap_s->opacitymap_clist);
    sprintf(cmap_s->opacitymap_clist->r1_name, "data");
    sprintf(cmap_s->opacitymap_clist->r2_name, "opacity");
	
	append_real2_clist(val_min, opaciy, cmap_s->opacitymap_clist);
    append_real2_clist(val_max, opaciy, cmap_s->opacitymap_clist);
	return;
}
void set_full_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	set_constant_opacitymap(cmap_s, val_min, val_max, ONE);
	return;
}


void copy_colormap_to_ctl(struct colormap_params *cmap_s, 
			struct colormap_ctl_c *cmap_c){	
	copy_to_chara_ctl_item(cmap_s->colormap_mode->c_tbl, cmap_c->colormap_mode_ctl);
	copy_to_chara_ctl_item("colormap_list", cmap_c->data_mapping_ctl);
	
	dup_real2_clist(cmap_s->colormap_clist, cmap_c->colortbl_list);
	
	copy_to_chara_ctl_item("point_linear", cmap_c->opacity_style_ctl);
    dup_real2_clist(cmap_s->opacitymap_clist, cmap_c->colortbl_list);
	update_real_ctl_item_c(cmap_s->min_opacity, cmap_c->fix_opacity_ctl);
	return;
	}

void make_colorbar_for_ctl(struct colormap_params *cmap_s, 
			struct pvr_colorbar_ctl_c *cbar_c){
	
	set_boolean_by_chara_ctl_item(1, cbar_c->colorbar_switch_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->colorbar_scale_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->zeromarker_flag_ctl);
	set_boolean_by_chara_ctl_item(1, cbar_c->axis_switch_ctl);
	
	update_real2_ctl_item_c(cmap_s->range_min->r_data, cmap_s->range_max->r_data,
                            cbar_c->cbar_range_ctl);
	update_int_ctl_item_c(1, cbar_c->font_size_ctl);
	update_int_ctl_item_c(3, cbar_c->ngrid_cbar_ctl);

	return;
}


void copy_colormap_from_ctl(struct colormap_ctl_c *cmap_c, 
			struct colormap_params *cmap_s){
	
	if(cmap_c->colormap_mode_ctl->iflag == 0){
		copy_to_chara_ctl_item(label_rainbow, cmap_s->colormap_mode);
	}else{
		copy_to_chara_ctl_item(cmap_c->colormap_mode_ctl->c_tbl, cmap_s->colormap_mode);
	};
   	
	if(compare_string(13, "colormap_list", cmap_c->data_mapping_ctl->c_tbl) == 0){
		printf("Something Wrong in colormap file\n)");
		return;
	}
	
	dup_real2_clist(cmap_c->colortbl_list, cmap_s->colormap_clist);
	
	if(compare_string(12, "point_linear", cmap_c->opacity_style_ctl->c_tbl) == 0){
		printf("Something Wrong in opacity_style_ctl\n)");
		return;
	};
    dup_real2_clist(cmap_c->colortbl_list, cmap_s->opacitymap_clist);
    set_from_real_ctl_item_c(cmap_c->fix_opacity_ctl, &cmap_s->min_opacity);
	
	return;
}


void check_colormap_control_file_s(struct colormap_params *cmap_s){
	cmap_cbar_c0 = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c));
	alloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	
	cmap_cbar_c0->iflag_colormap_ctl = 1;
	copy_colormap_to_ctl(cmap_s, cmap_cbar_c0->cmap_c);
	cmap_cbar_c0->iflag_colorbar_ctl = 1;
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
	
	cmap_cbar_c0->iflag_colormap_ctl = 1;
	copy_colormap_to_ctl(cmap_s, cmap_cbar_c0->cmap_c);
	cmap_cbar_c0->iflag_colorbar_ctl = 1;
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
	copy_colormap_from_ctl(cmap_cbar_c0->cmap_c, cmap_s);
	
	dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	free(cmap_cbar_c0);
	
	return;
}
