/*
//  t_ctl_data_pvr_colormap_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_ctl_data_pvr_colormap_c.h"

FILE *FP_Colormap;

const char label_colormap_ctl[NLBL_COLORMAP_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colormap_ctl"},
	
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
	
	/*[ 1]*/	{"ambient_coef_ctl"},
	/*[ 2]*/	{"diffuse_coef_ctl"},
	/*[ 3]*/	{"specular_coef_ctl"}
};

const char label_colorbar_ctl[NLBL_PVR_COLORBAR_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colorbar_switch_ctl"},
	/*[ 1]*/	{"colorbar_scale_ctl"},
	/*[ 2]*/	{"iflag_zeromarker"},
	/*[ 3]*/	{"colorbar_range"},
	/*[ 4]*/	{"font_size_ctl"},
	/*[ 5]*/	{"num_grid_ctl"},
	
	/*[ 6]*/	{"axis_label_switch"},
};

const char label_cmap_cbar_ctl[NLBL_CMAP_CBAR_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colormap_ctl"},
	/*[ 1]*/	{"colorbar_ctl"}
};

const char label_colormap_head[KCHARA_C] = "pvr_color_ctl";


void get_label_colormap_ctl(int index, char *label){
    if(index < NLBL_COLORMAP_CTL) strngcopy(label, label_colormap_ctl[index]);
    return;
};
void get_label_lighting_ctl(int index, char *label){
    if(index < NLBL_LIGHTING_CTL) strngcopy(label, label_lighting_ctl[index]);
    return;
};
void get_label_colorbar_ctl(int index, char *label){
    if(index < NLBL_PVR_COLORBAR_CTL) strngcopy(label, label_colorbar_ctl[index]);
    return;
};


void alloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c){
	int i;
	
	cmap_c->maxlen = 0;
	for (i=0;i<NLBL_COLORMAP_CTL;i++){
		if((int) strlen(label_colormap_ctl[i]) > cmap_c->maxlen){
			cmap_c->maxlen = (int) strlen(label_colormap_ctl[i]);
		};
	};
	
	cmap_c->colormap_mode_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(cmap_c->colormap_mode_ctl);
	
	cmap_c->lic_color_fld_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cmap_c->lic_color_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cmap_c->lic_opacity_fld_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cmap_c->lic_opacity_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(cmap_c->lic_color_fld_ctl);
	alloc_chara_ctl_item_c(cmap_c->lic_color_comp_ctl);
	alloc_chara_ctl_item_c(cmap_c->lic_opacity_fld_ctl);
	alloc_chara_ctl_item_c(cmap_c->lic_opacity_comp_ctl);
	
	cmap_c->data_mapping_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(cmap_c->data_mapping_ctl);
	
    cmap_c->colortbl_list = (struct real2_clist *) malloc(sizeof(struct real2_clist));
	init_real2_clist(cmap_c->colortbl_list);
	
	cmap_c->opacity_style_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(cmap_c->opacity_style_ctl);
	
	cmap_c->fix_opacity_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(cmap_c->fix_opacity_ctl);
	
    cmap_c->linear_opacity_list = (struct real2_clist *) malloc(sizeof(struct real2_clist));
	init_real2_clist(cmap_c->linear_opacity_list);
	init_real3_ctl_list(&cmap_c->step_opacity_list);
	
	cmap_c->range_min_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	cmap_c->range_max_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(cmap_c->range_min_ctl);
	init_real_ctl_item_c(cmap_c->range_max_ctl);
	
	return;
};

void dealloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c){
	
	dealloc_chara_ctl_item_c(cmap_c->colormap_mode_ctl);
	free(cmap_c->colormap_mode_ctl);
	
	dealloc_chara_ctl_item_c(cmap_c->lic_color_fld_ctl);
	dealloc_chara_ctl_item_c(cmap_c->lic_color_comp_ctl);
	dealloc_chara_ctl_item_c(cmap_c->lic_opacity_fld_ctl);
	dealloc_chara_ctl_item_c(cmap_c->lic_opacity_comp_ctl);
	free(cmap_c->lic_color_fld_ctl);
	free(cmap_c->lic_color_comp_ctl);
	free(cmap_c->lic_opacity_fld_ctl);
	free(cmap_c->lic_opacity_comp_ctl);
	
	dealloc_chara_ctl_item_c(cmap_c->data_mapping_ctl);
	free(cmap_c->data_mapping_ctl);
	
	clear_real2_clist(cmap_c->colortbl_list);
    free(cmap_c->colortbl_list);
	
	dealloc_chara_ctl_item_c(cmap_c->opacity_style_ctl);
	free(cmap_c->opacity_style_ctl);
	free(cmap_c->fix_opacity_ctl);
	
	clear_real2_clist(cmap_c->linear_opacity_list);
    free(cmap_c->linear_opacity_list);
	clear_real3_ctl_list(&cmap_c->step_opacity_list);
	
	free(cmap_c->range_min_ctl);
	free(cmap_c->range_max_ctl);
	
	return;
};

int read_colormap_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct colormap_ctl_c *cmap_c){
	if(right_begin_flag_c(buf, label) == 0) return 0;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 0], cmap_c->colormap_mode_ctl);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 1], cmap_c->lic_color_fld_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 2], cmap_c->lic_color_comp_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 3], cmap_c->lic_opacity_fld_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 4], cmap_c->lic_opacity_comp_ctl);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 5], cmap_c->data_mapping_ctl);
		
		read_real2_clist(fp, buf, label_colormap_ctl[ 6], cmap_c->colortbl_list);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 7], cmap_c->opacity_style_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[ 8], cmap_c->fix_opacity_ctl);
		
		read_real2_clist(fp, buf, label_colormap_ctl[ 9], cmap_c->linear_opacity_list);
		read_real3_ctl_list(fp, buf, label_colormap_ctl[10], &cmap_c->step_opacity_list);
		
		read_real_ctl_item_c(buf, label_colormap_ctl[11], cmap_c->range_min_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[12], cmap_c->range_max_ctl);
	};
	return 1;
};

int write_colormap_ctl_c(FILE *fp, int level, const char *label, 
			struct colormap_ctl_c *cmap_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 0], cmap_c->colormap_mode_ctl);
	
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 1], cmap_c->lic_color_fld_ctl);
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 2], cmap_c->lic_color_comp_ctl);
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 3], cmap_c->lic_opacity_fld_ctl);
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 4], cmap_c->lic_opacity_comp_ctl);
	
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 5], cmap_c->data_mapping_ctl);
	
	write_real2_clist(fp, level, label_colormap_ctl[ 6], cmap_c->colortbl_list);
	
	fprintf(fp, "!\n");
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 7], cmap_c->opacity_style_ctl);
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 8], cmap_c->fix_opacity_ctl);
	
	write_real2_clist(fp, level, label_colormap_ctl[ 9], cmap_c->linear_opacity_list);
	write_real3_ctl_list(fp, level, label_colormap_ctl[10], &cmap_c->step_opacity_list);
	
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[11], cmap_c->range_min_ctl);
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[12], cmap_c->range_max_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_lighting_ctl_c(struct lighting_ctl_c *light_c){
	int i;
	
	light_c->maxlen = 0;
	for (i=0;i<NLBL_LIGHTING_CTL;i++){
		if((int) strlen(label_lighting_ctl[i]) > light_c->maxlen){
			light_c->maxlen = (int) strlen(label_lighting_ctl[i]);
		};
	};
	
	light_c->ambient_coef_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	light_c->diffuse_coef_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	light_c->specular_coef_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(light_c->ambient_coef_ctl);
	init_real_ctl_item_c(light_c->diffuse_coef_ctl);
	init_real_ctl_item_c(light_c->specular_coef_ctl);
	
	init_real3_ctl_list(&light_c->light_position_list);
	
	return;
};

void dealloc_lighting_ctl_c(struct lighting_ctl_c *light_c){
	
	free(light_c->ambient_coef_ctl);
	free(light_c->diffuse_coef_ctl);
	free(light_c->specular_coef_ctl);
	
	clear_real3_ctl_list(&light_c->light_position_list);	
	return;
};

int read_lighting_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lighting_ctl_c *light_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real3_ctl_list(fp, buf, label_lighting_ctl[ 0], &light_c->light_position_list);
		
		read_real_ctl_item_c(buf, label_lighting_ctl[ 1], light_c->ambient_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 2], light_c->diffuse_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 3], light_c->specular_coef_ctl);
	};
	return 1;
};

int write_lighting_ctl_c(FILE *fp, int level, const char *label, 
			struct lighting_ctl_c *light_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real3_ctl_list(fp, level, label_lighting_ctl[ 0], &light_c->light_position_list);
	
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 1], light_c->ambient_coef_ctl);
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 2], light_c->diffuse_coef_ctl);
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 3], light_c->specular_coef_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c){
	int i;
	
	cbar_c->maxlen = 0;
	for (i=0;i<NLBL_PVR_COLORBAR_CTL;i++){
		if(strlen(label_colorbar_ctl[i]) > cbar_c->maxlen){
			cbar_c->maxlen = (int) strlen(label_colorbar_ctl[i]);
		};
	};
	
	cbar_c->colorbar_switch_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cbar_c->colorbar_scale_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	cbar_c->zeromarker_flag_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(cbar_c->colorbar_switch_ctl);
	alloc_chara_ctl_item_c(cbar_c->colorbar_scale_ctl);
	alloc_chara_ctl_item_c(cbar_c->zeromarker_flag_ctl);
	
	cbar_c->font_size_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	cbar_c->ngrid_cbar_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_int_ctl_item_c(cbar_c->font_size_ctl);
	init_int_ctl_item_c(cbar_c->ngrid_cbar_ctl);
	
	cbar_c->cbar_range_ctl = (struct real2_ctl_item *) malloc(sizeof(struct real2_ctl_item));
	init_real2_ctl_item_c(cbar_c->cbar_range_ctl);
	
	cbar_c->axis_switch_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(cbar_c->axis_switch_ctl);
	
	return;
};

void dealloc_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c){
	
	dealloc_chara_ctl_item_c(cbar_c->colorbar_switch_ctl);
	dealloc_chara_ctl_item_c(cbar_c->colorbar_scale_ctl);
	dealloc_chara_ctl_item_c(cbar_c->zeromarker_flag_ctl);
	free(cbar_c->colorbar_switch_ctl);
	free(cbar_c->colorbar_scale_ctl);
	free(cbar_c->zeromarker_flag_ctl);
	
	free(cbar_c->font_size_ctl);
	free(cbar_c->ngrid_cbar_ctl);
	
	free(cbar_c->cbar_range_ctl);
	
	dealloc_chara_ctl_item_c(cbar_c->axis_switch_ctl);
	free(cbar_c->axis_switch_ctl);
	
	return;
};

int read_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_colorbar_ctl_c *cbar_c){
	if(right_begin_flag_c(buf, label) == 0) return 0;
	
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 0], cbar_c->colorbar_switch_ctl);
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 1], cbar_c->colorbar_scale_ctl);
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 2], cbar_c->zeromarker_flag_ctl);
		
		read_real2_ctl_item_c(buf, label_colorbar_ctl[ 3], cbar_c->cbar_range_ctl);
		
		read_integer_ctl_item_c(buf, label_colorbar_ctl[ 4], cbar_c->font_size_ctl);
		read_integer_ctl_item_c(buf, label_colorbar_ctl[ 5], cbar_c->ngrid_cbar_ctl);
		
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 6], cbar_c->axis_switch_ctl);
	};
	return 1;
};

int write_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colorbar_ctl_c *cbar_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 0], cbar_c->colorbar_switch_ctl);
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 1], cbar_c->colorbar_scale_ctl);
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 2], cbar_c->zeromarker_flag_ctl);
	
	write_real2_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 3], cbar_c->cbar_range_ctl);
	
	write_integer_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 4], cbar_c->font_size_ctl);
	write_integer_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 5], cbar_c->ngrid_cbar_ctl);
	
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 6], cbar_c->axis_switch_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_colormap_colorbar_ctl_c(struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	int i;
	
	cmap_cbar_c->maxlen = 0;
	for (i=0;i<NLBL_CMAP_CBAR_CTL;i++){
		if(strlen(label_cmap_cbar_ctl[i]) > cmap_cbar_c->maxlen){
			cmap_cbar_c->maxlen = (int) strlen(label_cmap_cbar_ctl[i]);
		};
	};
	
	cmap_cbar_c->iflag_colormap_ctl =     0;
	cmap_cbar_c->iflag_colorbar_ctl =     0;
	
	cmap_cbar_c->cmap_c = (struct colormap_ctl_c *) malloc(sizeof(struct colormap_ctl_c));
	alloc_colormap_ctl_c(cmap_cbar_c->cmap_c);
	
	cmap_cbar_c->cbar_c = (struct pvr_colorbar_ctl_c *) malloc(sizeof(struct pvr_colorbar_ctl_c));
	alloc_colorbar_ctl_c(cmap_cbar_c->cbar_c);
	
	return;
};

void dealloc_colormap_colorbar_ctl_c(struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	dealloc_colormap_ctl_c(cmap_cbar_c->cmap_c);
	free(cmap_cbar_c->cmap_c);
	dealloc_colorbar_ctl_c(cmap_cbar_c->cbar_c);
	free(cmap_cbar_c->cbar_c);
	
	cmap_cbar_c->iflag_colormap_ctl =     0;
	cmap_cbar_c->iflag_colorbar_ctl =     0;
	return;
};

int read_colormap_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		cmap_cbar_c->iflag_colormap_ctl = read_colormap_ctl_c(fp, buf, 
					label_cmap_cbar_ctl[0], cmap_cbar_c->cmap_c);
		cmap_cbar_c->iflag_colorbar_ctl = read_colorbar_ctl_c(fp, buf, 
					label_cmap_cbar_ctl[1], cmap_cbar_c->cbar_c);
	};
	return 1;
};


int write_colormap_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	if(cmap_cbar_c->iflag_colormap_ctl == 1){
		level = write_colormap_ctl_c(fp, level, label_cmap_cbar_ctl[0], cmap_cbar_c->cmap_c);
	};
	
	if(cmap_cbar_c->iflag_colorbar_ctl > 0){
		fprintf(fp, "!\n");
		level = write_colorbar_ctl_c(fp, level, label_cmap_cbar_ctl[1], cmap_cbar_c->cbar_c);
	};
	level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
};



int read_colormap_file_c(const char *file_name, char buf[LENGTHBUF],
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	int iflag = 0;
	
    printf("Read PVR colormap file name: %s\n", file_name);
	if ((FP_Colormap = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
    skip_comment_read_line(FP_Colormap, buf);
	if(right_begin_flag_c(buf, label_colormap_head) > 0){
		iflag = read_colormap_colorbar_ctl_c(FP_Colormap, buf,
					label_colormap_head, cmap_cbar_c);
	};
	fclose(FP_Colormap);
	
	return -1;
};

int write_colormap_file_c(const char *file_name, 
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	int level;
	
    printf("Write PVR colormap file name: %s\n", file_name);
	if ((FP_Colormap = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	level = write_colormap_colorbar_ctl_c(FP_Colormap, 0, 
				label_colormap_head, cmap_cbar_c);
	fclose(FP_Colormap);
	
	return -1;
};
