/*
//  t_ctl_data_pvr_colormap_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_ctl_data_pvr_colormap_c.h"

#define NLBL_COLORMAP_CTL   13
#define NLBL_LIGHTING_CTL    4

const char label_colormap_ctl[NLBL_COLORMAP_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colormap_mode_ctl"},
	
	/*[ 1]*/	{"LIC_color_field"},
	/*[ 2]*/	{"LIC_color_componenet"},
	/*[ 3]*/	{"LIC_transparent_field"},
	/*[ 4]*/	{"LIC_transparent_componenet"},
	
	/*[ 5]*/	{"data_mapping_ctl"},
	/*[ 6]*/	{"color_table_ctl"},
	
	/*[ 7]*/	{"opacity_style_ctl"},
	/*[ 8]*/	{"constant_opacity_ctl"},
	/*[ 9]*/	{"linear_opacity_ctl"},
	/*[10]*/	{"step_opacity_ctl"},
	
	/*[11]*/	{"range_min_ctl"},
	/*[12]*/	{"range_max_ctl"}
};

const char label_lighting_ctl[NLBL_LIGHTING_CTL][KCHARA_C] = {
	/*[ 0]*/	{"position_of_lights"},
	
	/*[ 1]*/	{"ambient_coef"},
	/*[ 2]*/	{"diffuse_coef"},
	/*[ 3]*/	{"specular_coef"}
};

void alloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c){
	int i;
	
	cmap_c->maxlen = 0;
	for (i=0;i<NLBL_COLORMAP_CTL;i++){
		if(strlen(label_colormap_ctl[i]) > cmap_c->maxlen){
			cmap_c->maxlen = strlen(label_colormap_ctl[i]);
		};
	};
	
	cmap_c->colormap_mode_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(cmap_c->colormap_mode_ctl);
	
	cmap_c->lic_color_fld_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cmap_c->lic_color_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cmap_c->lic_opacity_fld_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cmap_c->lic_opacity_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(cmap_c->lic_color_fld_ctl);
	alloc_ctl_chara_item(cmap_c->lic_color_comp_ctl);
	alloc_ctl_chara_item(cmap_c->lic_opacity_fld_ctl);
	alloc_ctl_chara_item(cmap_c->lic_opacity_comp_ctl);
	
	cmap_c->data_mapping_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(cmap_c->data_mapping_ctl);
	
	cmap_c->colortbl_ctl = (struct real2_ctl_array *) malloc(sizeof(struct real2_ctl_array));
	
	cmap_c->opacity_style_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(cmap_c->opacity_style_ctl);
	
	cmap_c->fix_opacity_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_ctl_real_item(cmap_c->fix_opacity_ctl);
	
	cmap_c->linear_opacity_ctl = (struct real2_ctl_array *) malloc(sizeof(struct real2_ctl_array));
	init_ctl_real2_array(cmap_c->linear_opacity_ctl);
	cmap_c->step_opacity_ctl = (struct real3_ctl_array *) malloc(sizeof(struct real3_ctl_array));
	init_ctl_real3_array(cmap_c->step_opacity_ctl);
	
	cmap_c->range_min_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	cmap_c->range_max_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_ctl_real_item(cmap_c->range_min_ctl);
	init_ctl_real_item(cmap_c->range_max_ctl);
	
	return;
};

void dealloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c){
	
	dealloc_ctl_chara_item(cmap_c->colormap_mode_ctl);
	free(cmap_c->colormap_mode_ctl);
	
	dealloc_ctl_chara_item(cmap_c->lic_color_fld_ctl);
	dealloc_ctl_chara_item(cmap_c->lic_color_comp_ctl);
	dealloc_ctl_chara_item(cmap_c->lic_opacity_fld_ctl);
	dealloc_ctl_chara_item(cmap_c->lic_opacity_comp_ctl);
	free(cmap_c->lic_color_fld_ctl);
	free(cmap_c->lic_color_comp_ctl);
	free(cmap_c->lic_opacity_fld_ctl);
	free(cmap_c->lic_opacity_comp_ctl);
	
	dealloc_ctl_chara_item(cmap_c->data_mapping_ctl);
	free(cmap_c->data_mapping_ctl);
	
	free(cmap_c->colortbl_ctl);
	
	dealloc_ctl_chara_item(cmap_c->opacity_style_ctl);
	free(cmap_c->opacity_style_ctl);
	free(cmap_c->fix_opacity_ctl);
	
	dealloc_ctl_real2_array(cmap_c->linear_opacity_ctl);
	free(cmap_c->linear_opacity_ctl);
	dealloc_ctl_real3_array(cmap_c->step_opacity_ctl);
	free(cmap_c->step_opacity_ctl);
	
	free(cmap_c->range_min_ctl);
	free(cmap_c->range_max_ctl);
	
	return;
};

int read_colormap_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct colormap_ctl_c *cmap_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_colormap_ctl[ 0], cmap_c->colormap_mode_ctl);
		
		read_character_ctl_item_c(buf, label_colormap_ctl[ 1], cmap_c->lic_color_fld_ctl);
		read_character_ctl_item_c(buf, label_colormap_ctl[ 2], cmap_c->lic_color_comp_ctl);
		read_character_ctl_item_c(buf, label_colormap_ctl[ 3], cmap_c->lic_opacity_fld_ctl);
		read_character_ctl_item_c(buf, label_colormap_ctl[ 4], cmap_c->lic_opacity_comp_ctl);
		
		read_character_ctl_item_c(buf, label_colormap_ctl[ 5], cmap_c->data_mapping_ctl);
		
		read_real2_ctl_array_c(fp, buf, label_colormap_ctl[ 6], cmap_c->colortbl_ctl);
		
		read_character_ctl_item_c(buf, label_colormap_ctl[ 7], cmap_c->opacity_style_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[ 8], cmap_c->fix_opacity_ctl);
		
		read_real2_ctl_array_c(fp, buf, label_colormap_ctl[ 9], cmap_c->linear_opacity_ctl);
		read_real3_ctl_array_c(fp, buf, label_colormap_ctl[10], cmap_c->step_opacity_ctl);
		
		read_real_ctl_item_c(buf, label_colormap_ctl[11], cmap_c->range_min_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[12], cmap_c->range_max_ctl);
	};
	return 1;
};

int write_colormap_ctl_c(FILE *fp, int level, const char *label, 
			struct colormap_ctl_c *cmap_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 0], cmap_c->colormap_mode_ctl);
	
	write_character_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 1], cmap_c->lic_color_fld_ctl);
	write_character_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 2], cmap_c->lic_color_comp_ctl);
	write_character_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 3], cmap_c->lic_opacity_fld_ctl);
	write_character_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 4], cmap_c->lic_opacity_comp_ctl);
	
	write_character_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 5], cmap_c->data_mapping_ctl);
	
	if(cmap_c->colortbl_ctl->num > 0) fprintf(fp, "!\n");
	write_real2_ctl_array_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 6], cmap_c->colortbl_ctl);
	
	write_character_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 7], cmap_c->opacity_style_ctl);
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 8], cmap_c->fix_opacity_ctl);
	
	if(cmap_c->linear_opacity_ctl->num > 0) fprintf(fp, "!\n");
	write_real2_ctl_array_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 9], cmap_c->linear_opacity_ctl);
	if(cmap_c->step_opacity_ctl->num > 0) fprintf(fp, "!\n");
	write_real3_ctl_array_c(fp, level, cmap_c->maxlen, label_colormap_ctl[10], cmap_c->step_opacity_ctl);
	
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[11], cmap_c->range_min_ctl);
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[12], cmap_c->range_max_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_lighting_ctl_c(struct lighting_ctl_c *light_c){
	int i;
	
	light_c->maxlen = 0;
	for (i=0;i<NLBL_LIGHTING_CTL;i++){
		if(strlen(label_lighting_ctl[i]) > light_c->maxlen){
			light_c->maxlen = strlen(label_lighting_ctl[i]);
		};
	};
	
	light_c->ambient_coef_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	light_c->diffuse_coef_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	light_c->specular_coef_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_ctl_real_item(light_c->ambient_coef_ctl);
	init_ctl_real_item(light_c->diffuse_coef_ctl);
	init_ctl_real_item(light_c->specular_coef_ctl);
	
	light_c->light_position_ctl = (struct real3_ctl_array *) malloc(sizeof(struct real3_ctl_array));
	init_ctl_real3_array(light_c->light_position_ctl);
	
	return;
};

void dealloc_lighting_ctl_c(struct lighting_ctl_c *light_c){
	
	free(light_c->ambient_coef_ctl);
	free(light_c->diffuse_coef_ctl);
	free(light_c->specular_coef_ctl);
	
	dealloc_ctl_real3_array(light_c->light_position_ctl);
	free(light_c->light_position_ctl);
	
	return;
};

int read_lighting_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lighting_ctl_c *light_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_real3_ctl_array_c(fp, buf, label_lighting_ctl[ 0], light_c->light_position_ctl);
		
		read_real_ctl_item_c(buf, label_lighting_ctl[ 1], light_c->ambient_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 2], light_c->diffuse_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 3], light_c->specular_coef_ctl);
	};
	return 1;
};

int write_lighting_ctl_c(FILE *fp, int level, const char *label, 
			struct lighting_ctl_c *light_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real3_ctl_array_c(fp, level, light_c->maxlen, label_lighting_ctl[ 0], light_c->light_position_ctl);
	
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 1], light_c->ambient_coef_ctl);
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 2], light_c->diffuse_coef_ctl);
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 3], light_c->specular_coef_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
