/*
//  t_ctl_data_pvr_colormap_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_ctl_data_pvr_colormap_c.h"

FILE *FP_Colormap;

const char label_colormap_ctl[NLBL_COLORMAP_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colormap_mode_ctl"},
	
	/*[ 1]*/	{"LIC_color_field"},
	/*[ 2]*/	{"LIC_color_componenet"},
	/*[ 3]*/	{"LIC_transparent_field"},
	/*[ 4]*/	{"LIC_transparent_componenet"},
	
	/*[ 5]*/	{"data_mapping_ctl"},
	/*[ 6]*/	{"range_min_ctl"},
	/*[ 7]*/	{"range_max_ctl"},
	/*[ 8]*/	{"color_table_ctl"},
	
	/*[ 9]*/	{"opacity_style_ctl"},
	/*[10]*/	{"constant_opacity_ctl"},
	/*[11]*/	{"linear_opacity_ctl"},
	/*[12]*/	{"step_opacity_ctl"}
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
	/*[ 2]*/	{"font_size_ctl"},
	/*[ 3]*/	{"num_grid_ctl"},
	/*[ 4]*/	{"iflag_zeromarker"},
	/*[ 5]*/	{"colorbar_range"},
	
	/*[ 6]*/	{"axis_label_switch"},
};

const char label_cmap_cbar_ctl[NLBL_CMAP_CBAR_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colormap_ctl"},
	/*[ 1]*/	{"colorbar_ctl"}
};

const char label_colormap_head[KCHARA_C] = "pvr_color_ctl";


struct colormap_ctl_c * init_colormap_ctl_c(){
	int i;
    struct colormap_ctl_c *cmap_c;
    if((cmap_c = (struct colormap_ctl_c *) malloc(sizeof(struct colormap_ctl_c))) == NULL) {
        printf("malloc error for colormap_ctl_c \n");
        exit(0);
    }
	/* lic_ctl_labls->label_lic_cmap = init_label_LIC_cmap(); */
	
    cmap_c->iflag_use = 0;
	cmap_c->maxlen = 0;
	for (i=0;i<NLBL_COLORMAP_CTL;i++){
		if((int) strlen(label_colormap_ctl[i]) > cmap_c->maxlen){
			cmap_c->maxlen = (int) strlen(label_colormap_ctl[i]);
		};
	};
	
	cmap_c->colormap_mode_ctl = init_chara_ctl_item_c();
	
	cmap_c->lic_color_fld_ctl =    init_chara_ctl_item_c();
	cmap_c->lic_color_comp_ctl =   init_chara_ctl_item_c();
	cmap_c->lic_opacity_fld_ctl =  init_chara_ctl_item_c();
	cmap_c->lic_opacity_comp_ctl = init_chara_ctl_item_c();
	
	cmap_c->data_mapping_ctl = init_chara_ctl_item_c();
	
	cmap_c->colortbl_list = init_real2_clist();
    sprintf(cmap_c->colortbl_list->r1_name, "data");
    sprintf(cmap_c->colortbl_list->r2_name, "color");
	
	cmap_c->opacity_style_ctl = init_chara_ctl_item_c();
	
    cmap_c->fix_opacity_ctl = init_real_ctl_item_c();
	
	cmap_c->linear_opacity_list = init_real2_clist();
    sprintf(cmap_c->linear_opacity_list->r1_name, "data");
    sprintf(cmap_c->linear_opacity_list->r2_name, "opacity");

    cmap_c->step_opacity_list = init_real3_clist();
    sprintf(cmap_c->step_opacity_list->r1_name, "lower_value");
    sprintf(cmap_c->step_opacity_list->r2_name, "upper_value");
    sprintf(cmap_c->step_opacity_list->r3_name, "opacity");
	
    cmap_c->range_min_ctl = init_real_ctl_item_c();
    cmap_c->range_max_ctl = init_real_ctl_item_c();
	
	return cmap_c;
};

void dealloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c){
	
	dealloc_chara_ctl_item_c(cmap_c->colormap_mode_ctl);
	
	dealloc_chara_ctl_item_c(cmap_c->lic_color_fld_ctl);
	dealloc_chara_ctl_item_c(cmap_c->lic_color_comp_ctl);
	dealloc_chara_ctl_item_c(cmap_c->lic_opacity_fld_ctl);
	dealloc_chara_ctl_item_c(cmap_c->lic_opacity_comp_ctl);
	
	dealloc_chara_ctl_item_c(cmap_c->data_mapping_ctl);
	
	dealloc_real2_clist(cmap_c->colortbl_list);
	
	dealloc_chara_ctl_item_c(cmap_c->opacity_style_ctl);
	free(cmap_c->fix_opacity_ctl);
	
	dealloc_real2_clist(cmap_c->linear_opacity_list);
	dealloc_real3_clist(cmap_c->step_opacity_list);
	
	free(cmap_c->range_min_ctl);
	free(cmap_c->range_max_ctl);
	
    free(cmap_c);
	return;
};

void read_colormap_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct colormap_ctl_c *cmap_c){
    if(cmap_c->iflag_use > 0) return;
	if(right_begin_flag_c(buf, label) == 0) return;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 0], cmap_c->colormap_mode_ctl);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 1], cmap_c->lic_color_fld_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 2], cmap_c->lic_color_comp_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 3], cmap_c->lic_opacity_fld_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 4], cmap_c->lic_opacity_comp_ctl);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 5], cmap_c->data_mapping_ctl);
		
		read_real2_clist(fp, buf, label_colormap_ctl[ 8], cmap_c->colortbl_list);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 9], cmap_c->opacity_style_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[10], cmap_c->fix_opacity_ctl);
		
		read_real2_clist(fp, buf, label_colormap_ctl[11], cmap_c->linear_opacity_list);
		read_real3_clist(fp, buf, label_colormap_ctl[12], cmap_c->step_opacity_list);
		
		read_real_ctl_item_c(buf, label_colormap_ctl[ 6], cmap_c->range_min_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[ 7], cmap_c->range_max_ctl);
	};
	cmap_c->iflag_use = 1;
    return;
};

int write_colormap_ctl_c(FILE *fp, int level, const char *label, 
			struct colormap_ctl_c *cmap_c){
    if(cmap_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 0], cmap_c->colormap_mode_ctl);
	
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 1], cmap_c->lic_color_fld_ctl);
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 2], cmap_c->lic_color_comp_ctl);
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 3], cmap_c->lic_opacity_fld_ctl);
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 4], cmap_c->lic_opacity_comp_ctl);
	
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 5], cmap_c->data_mapping_ctl);
	
	write_real2_clist(fp, level, label_colormap_ctl[ 8], cmap_c->colortbl_list);
	
	fprintf(fp, "!\n");
	write_chara_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 9], cmap_c->opacity_style_ctl);
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[10], cmap_c->fix_opacity_ctl);
	
	write_real2_clist(fp, level, label_colormap_ctl[11], cmap_c->linear_opacity_list);
	write_real3_clist(fp, level, label_colormap_ctl[12], cmap_c->step_opacity_list);
	
    fprintf(fp, "!\n");
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 6], cmap_c->range_min_ctl);
	write_real_ctl_item_c(fp, level, cmap_c->maxlen, label_colormap_ctl[ 7], cmap_c->range_max_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct lighting_ctl_c * init_lighting_ctl_c(void){
	int i;
    struct lighting_ctl_c *light_c;
    if((light_c = (struct lighting_ctl_c *) malloc(sizeof(struct lighting_ctl_c))) == NULL) {
        printf("malloc error for lighting_ctl_c \n");
        exit(0);
    }
    
    light_c->iflag_use = 0;
	light_c->maxlen = 0;
	for (i=0;i<NLBL_LIGHTING_CTL;i++){
		if((int) strlen(label_lighting_ctl[i]) > light_c->maxlen){
			light_c->maxlen = (int) strlen(label_lighting_ctl[i]);
		};
	};
	
    light_c->ambient_coef_ctl =  init_real_ctl_item_c();
    light_c->diffuse_coef_ctl =  init_real_ctl_item_c();
    light_c->specular_coef_ctl = init_real_ctl_item_c();
	
    light_c->light_position_list = init_real3_clist();
    sprintf(light_c->light_position_list->r1_name, "x");
    sprintf(light_c->light_position_list->r2_name, "y");
    sprintf(light_c->light_position_list->r3_name, "z");
	
	return light_c;
};

void dealloc_lighting_ctl_c(struct lighting_ctl_c *light_c){
	free(light_c->ambient_coef_ctl);
	free(light_c->diffuse_coef_ctl);
	free(light_c->specular_coef_ctl);
	
	dealloc_real3_clist(light_c->light_position_list);	

    free(light_c);
    return;
};

void read_lighting_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lighting_ctl_c *light_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real3_clist(fp, buf, label_lighting_ctl[ 0], light_c->light_position_list);
		
		read_real_ctl_item_c(buf, label_lighting_ctl[ 1], light_c->ambient_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 2], light_c->diffuse_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 3], light_c->specular_coef_ctl);
	};
	light_c->iflag_use = 1;
    return;
};

int write_lighting_ctl_c(FILE *fp, int level, const char *label, 
			struct lighting_ctl_c *light_c){
    if(light_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real3_clist(fp, level, label_lighting_ctl[ 0], light_c->light_position_list);
	
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 1], light_c->ambient_coef_ctl);
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 2], light_c->diffuse_coef_ctl);
	write_real_ctl_item_c(fp, level, light_c->maxlen, label_lighting_ctl[ 3], light_c->specular_coef_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct pvr_colorbar_ctl_c * init_colorbar_ctl_c(void){
	int i;
    struct pvr_colorbar_ctl_c *cbar_c;
    if((cbar_c = (struct pvr_colorbar_ctl_c *) malloc(sizeof(struct pvr_colorbar_ctl_c))) == NULL) {
        printf("malloc error for pvr_colorbar_ctl_c \n");
        exit(0);
    }
	/*cbar_c->label_pvr_cbar = init_label_pvr_cbar(); */
	
    cbar_c->iflag_use = 0;
	cbar_c->maxlen = 0;
	for (i=0;i<NLBL_PVR_COLORBAR_CTL;i++){
		if(strlen(label_colorbar_ctl[i]) > cbar_c->maxlen){
			cbar_c->maxlen = (int) strlen(label_colorbar_ctl[i]);
		};
	};
	
	cbar_c->colorbar_switch_ctl = init_chara_ctl_item_c();
	cbar_c->colorbar_scale_ctl =  init_chara_ctl_item_c();
	cbar_c->zeromarker_flag_ctl = init_chara_ctl_item_c();
	
    cbar_c->font_size_ctl =  init_int_ctl_item_c();
    cbar_c->ngrid_cbar_ctl = init_int_ctl_item_c();
	
	cbar_c->cbar_range_ctl = init_real2_ctl_item_c();
	
	cbar_c->axis_switch_ctl = init_chara_ctl_item_c();
	
	return cbar_c;
};

void dealloc_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c){
	
	dealloc_chara_ctl_item_c(cbar_c->colorbar_switch_ctl);
	dealloc_chara_ctl_item_c(cbar_c->colorbar_scale_ctl);
	dealloc_chara_ctl_item_c(cbar_c->zeromarker_flag_ctl);
	
	free(cbar_c->font_size_ctl);
	free(cbar_c->ngrid_cbar_ctl);
	
	free(cbar_c->cbar_range_ctl);
	
	dealloc_chara_ctl_item_c(cbar_c->axis_switch_ctl);
	
    free(cbar_c);
	return;
};

void read_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_colorbar_ctl_c *cbar_c){
	if(right_begin_flag_c(buf, label) == 0) return;
	
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 0], cbar_c->colorbar_switch_ctl);
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 1], cbar_c->colorbar_scale_ctl);
		
		read_integer_ctl_item_c(buf, label_colorbar_ctl[ 2], cbar_c->font_size_ctl);
		read_integer_ctl_item_c(buf, label_colorbar_ctl[ 3], cbar_c->ngrid_cbar_ctl);
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 4], cbar_c->zeromarker_flag_ctl);
		read_real2_ctl_item_c(buf, label_colorbar_ctl[ 5], cbar_c->cbar_range_ctl);
		
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 6], cbar_c->axis_switch_ctl);
	};
    cbar_c->iflag_use = 1;
	return;
};

int write_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colorbar_ctl_c *cbar_c){
    if(cbar_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 0],
						   cbar_c->colorbar_switch_ctl);
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 1],
						   cbar_c->colorbar_scale_ctl);
	
	write_integer_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 2],
							 cbar_c->font_size_ctl);
	write_integer_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 3],
							 cbar_c->ngrid_cbar_ctl);
	
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 4],
						   cbar_c->zeromarker_flag_ctl);
	write_real2_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 5],
						   cbar_c->cbar_range_ctl);
	
	write_chara_ctl_item_c(fp, level, cbar_c->maxlen, label_colorbar_ctl[ 6],
						   cbar_c->axis_switch_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct pvr_colormap_bar_ctl_c * init_colormap_colorbar_ctl_c(void){
	int i;
    struct pvr_colormap_bar_ctl_c *cmap_cbar_c;
    if((cmap_cbar_c = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c))) == NULL) {
        printf("malloc error for pvr_colormap_bar_ctl_c \n");
        exit(0);
    }
	
	/*cmap_cbar_c->label_pvr_cmap_bar = init_label_pvr_cmap_bar(); */
	
	cmap_cbar_c->iflag_use = 0;
	cmap_cbar_c->maxlen = 0;
	for (i=0;i<NLBL_CMAP_CBAR_CTL;i++){
		if(strlen(label_cmap_cbar_ctl[i]) > cmap_cbar_c->maxlen){
			cmap_cbar_c->maxlen = (int) strlen(label_cmap_cbar_ctl[i]);
		};
	};
		
	cmap_cbar_c->cmap_c = init_colormap_ctl_c();
	cmap_cbar_c->cbar_c = init_colorbar_ctl_c();	
	return cmap_cbar_c;
};

void dealloc_colormap_colorbar_ctl_c(struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	dealloc_colormap_ctl_c(cmap_cbar_c->cmap_c);
	dealloc_colorbar_ctl_c(cmap_cbar_c->cbar_c);
	
    free(cmap_cbar_c);
	return;
};

void read_colormap_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_colormap_ctl_c(fp, buf, label_cmap_cbar_ctl[0], cmap_cbar_c->cmap_c);
		read_colorbar_ctl_c(fp, buf, label_cmap_cbar_ctl[1], cmap_cbar_c->cbar_c);
	};
    cmap_cbar_c->iflag_use = 1;
    return;
};


int write_colormap_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    level = write_colormap_ctl_c(fp, level, label_cmap_cbar_ctl[0], cmap_cbar_c->cmap_c);
    level = write_colorbar_ctl_c(fp, level, label_cmap_cbar_ctl[1], cmap_cbar_c->cbar_c);
	
    level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
};



void read_colormap_file_c(const char *file_name, char buf[LENGTHBUF],
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	
    printf("Read PVR colormap file name: %s\n", file_name);
	if ((FP_Colormap = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
    skip_comment_read_line(FP_Colormap, buf);
	if(right_begin_flag_c(buf, label_colormap_head) > 0){
		read_colormap_colorbar_ctl_c(FP_Colormap, buf, label_colormap_head, cmap_cbar_c);
	};
	fclose(FP_Colormap);
	
    cmap_cbar_c->iflag_use = -1;
	return;
};

void write_colormap_file_c(const char *file_name, 
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	int level = 0;
	
    printf("Write PVR colormap file name: %s\n", file_name);
	if ((FP_Colormap = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	level = write_colormap_colorbar_ctl_c(FP_Colormap, 0, 
				label_colormap_head, cmap_cbar_c);
	fclose(FP_Colormap);
	
	return;
};
