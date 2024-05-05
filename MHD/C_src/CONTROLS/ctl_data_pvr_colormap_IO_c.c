/*
//  ctl_data_pvr_colormap_IO_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "ctl_data_pvr_colormap_IO_c.h"

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
	/*[12]*/	{"step_opacity_ctl"},

    /*[13]*/    {"background_color_ctl"}
};

const char label_lighting_ctl[NLBL_LIGHTING_CTL][KCHARA_C] = {
	/*[ 0]*/	{"position_of_lights"},
	
	/*[ 1]*/	{"ambient_coef_ctl"},
	/*[ 2]*/	{"diffuse_coef_ctl"},
	/*[ 3]*/	{"specular_coef_ctl"},
    /*[ 4]*/    {"sph_position_of_lights"}
};

const char label_colorbar_ctl[NLBL_PVR_COLORBAR_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colorbar_switch_ctl"},
    /*[ 1]*/    {"colorbar_position_ctl"},
	/*[ 2]*/	{"colorbar_scale_ctl"},
	/*[ 3]*/	{"font_size_ctl"},
	/*[ 4]*/	{"num_grid_ctl"},
	/*[ 5]*/	{"zeromarker_switch"},
	/*[ 6]*/	{"colorbar_range"},
	
	/*[ 7]*/	{"axis_label_switch"},
    /*[ 8]*/    {"time_label_switch"},
    /*[ 9]*/    {"map_grid_switch"}
};

const char label_cmap_cbar_ctl[NLBL_CMAP_CBAR_CTL][KCHARA_C] = {
	/*[ 0]*/	{"colormap_ctl"},
	/*[ 1]*/	{"colorbar_ctl"}
};

const char label_colormap_head[KCHARA_C] = "pvr_color_ctl";

void read_colormap_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct colormap_ctl_c *cmap_c){
    if(cmap_c->f_iflag[0] > 0) return;
	if(right_begin_flag_c(buf, label) == 0) return;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 0], cmap_c->f_colormap_mode_ctl);
        read_real3_ctl_item_c(buf, label_colormap_ctl[13], cmap_c->f_background_color_ctl);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 1], cmap_c->f_lic_color_fld_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 2], cmap_c->f_lic_color_comp_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 3], cmap_c->f_lic_opacity_fld_ctl);
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 4], cmap_c->f_lic_opacity_comp_ctl);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 5], cmap_c->f_data_mapping_ctl);
		
		read_real2_clist(fp, buf, label_colormap_ctl[ 8], cmap_c->f_colortbl_ctl);
		
		read_chara_ctl_item_c(buf, label_colormap_ctl[ 9], cmap_c->f_opacity_style_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[10], cmap_c->f_fix_opacity_ctl);
		
		read_real2_clist(fp, buf, label_colormap_ctl[11], cmap_c->f_linear_opacity_ctl);
		
		read_real_ctl_item_c(buf, label_colormap_ctl[ 6], cmap_c->f_range_min_ctl);
		read_real_ctl_item_c(buf, label_colormap_ctl[ 7], cmap_c->f_range_max_ctl);
	};
	cmap_c->f_iflag[0] = 1;
    return;
};

int write_colormap_ctl_c(FILE *fp, int level, const char *label, 
			struct colormap_ctl_c *cmap_c){
    if(cmap_c->f_iflag[0] == 0) return level;
    
    int i;
    int maxlen = 0;
	for (i=0;i<NLBL_COLORMAP_CTL;i++){
		if((int) strlen(label_colormap_ctl[i]) > maxlen){
			maxlen = (int) strlen(label_colormap_ctl[i]);
		};
	};
	
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 0], cmap_c->f_colormap_mode_ctl);
    write_real3_ctl_item_c(fp, level, maxlen, label_colormap_ctl[13], cmap_c->f_background_color_ctl);
	
	write_chara_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 1], cmap_c->f_lic_color_fld_ctl);
	write_chara_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 2], cmap_c->f_lic_color_comp_ctl);
	write_chara_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 3], cmap_c->f_lic_opacity_fld_ctl);
	write_chara_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 4], cmap_c->f_lic_opacity_comp_ctl);
	
	write_chara_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 5], cmap_c->f_data_mapping_ctl);
	
	write_real2_clist(fp, level, label_colormap_ctl[ 8], cmap_c->f_colortbl_ctl);
	
	fprintf(fp, "!\n");
	write_chara_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 9], cmap_c->f_opacity_style_ctl);
	write_real_ctl_item_c(fp, level, maxlen, label_colormap_ctl[10], cmap_c->f_fix_opacity_ctl);
	
	write_real2_clist(fp, level, label_colormap_ctl[11], cmap_c->f_linear_opacity_ctl);
	
    fprintf(fp, "!\n");
	write_real_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 6], cmap_c->f_range_min_ctl);
	write_real_ctl_item_c(fp, level, maxlen, label_colormap_ctl[ 7], cmap_c->f_range_max_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void read_lighting_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lighting_ctl_c *light_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real3_clist(fp, buf, label_lighting_ctl[ 0], light_c->f_light_position_ctl);
        read_real3_clist(fp, buf, label_lighting_ctl[ 4], light_c->f_light_sph_posi_ctl);

		read_real_ctl_item_c(buf, label_lighting_ctl[ 1], light_c->f_ambient_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 2], light_c->f_diffuse_coef_ctl);
		read_real_ctl_item_c(buf, label_lighting_ctl[ 3], light_c->f_specular_coef_ctl);
	};
	light_c->f_iflag[0] = 1;
    return;
};

int write_lighting_ctl_c(FILE *fp, int level, const char *label, 
			struct lighting_ctl_c *light_c){
    if(light_c->f_iflag[0] == 0) return level;
    
    int i;
	int maxlen = 0;
	for (i=0;i<NLBL_LIGHTING_CTL;i++){
		if((int) strlen(label_lighting_ctl[i]) > maxlen){
			maxlen = (int) strlen(label_lighting_ctl[i]);
		};
	};
    
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real3_clist(fp, level, label_lighting_ctl[ 0], light_c->f_light_position_ctl);
    write_real3_clist(fp, level, label_lighting_ctl[ 4], light_c->f_light_sph_posi_ctl);

	write_real_ctl_item_c(fp, level, maxlen, label_lighting_ctl[ 1], light_c->f_ambient_coef_ctl);
	write_real_ctl_item_c(fp, level, maxlen, label_lighting_ctl[ 2], light_c->f_diffuse_coef_ctl);
	write_real_ctl_item_c(fp, level, maxlen, label_lighting_ctl[ 3], light_c->f_specular_coef_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void read_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_colorbar_ctl_c *cbar_c){
	if(right_begin_flag_c(buf, label) == 0) return;
	
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 0], cbar_c->f_colorbar_switch_ctl);
        read_chara_ctl_item_c(buf, label_colorbar_ctl[ 1], cbar_c->f_colorbar_position_ctl);
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 2], cbar_c->f_colorbar_scale_ctl);
		
		read_integer_ctl_item_c(buf, label_colorbar_ctl[ 3], cbar_c->f_font_size_ctl);
		read_integer_ctl_item_c(buf, label_colorbar_ctl[ 4], cbar_c->f_ngrid_cbar_ctl);
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 5], cbar_c->f_zeromarker_flag_ctl);
		read_real2_ctl_item_c(buf, label_colorbar_ctl[ 6], cbar_c->f_cbar_range_ctl);
		
		read_chara_ctl_item_c(buf, label_colorbar_ctl[ 7], cbar_c->f_axis_switch_ctl);
        read_chara_ctl_item_c(buf, label_colorbar_ctl[ 8], cbar_c->f_time_switch_ctl);
        read_chara_ctl_item_c(buf, label_colorbar_ctl[ 9], cbar_c->f_mapgrid_switch_ctl);
	};
    cbar_c->f_iflag[0] = 1;
	return;
};

int write_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colorbar_ctl_c *cbar_c){
    if(cbar_c->f_iflag[0] == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    int i;
	int maxlen = 0;
	for (i=0;i<NLBL_PVR_COLORBAR_CTL;i++){
		if(strlen(label_colorbar_ctl[i]) > maxlen){
			maxlen = (int) strlen(label_colorbar_ctl[i]);
		};
	};
    
    write_chara_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 0],
						   cbar_c->f_colorbar_switch_ctl);
    write_chara_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 1],
                           cbar_c->f_colorbar_position_ctl);
	write_chara_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 2],
						   cbar_c->f_colorbar_scale_ctl);
	
	write_integer_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 3],
							 cbar_c->f_font_size_ctl);
	write_integer_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 4],
							 cbar_c->f_ngrid_cbar_ctl);
	
	write_chara_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 5],
						   cbar_c->f_zeromarker_flag_ctl);
	write_real2_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 6],
						   cbar_c->f_cbar_range_ctl);
	
	write_chara_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 7],
						   cbar_c->f_axis_switch_ctl);
    write_chara_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 8],
                           cbar_c->f_time_switch_ctl);
    write_chara_ctl_item_c(fp, level, maxlen, label_colorbar_ctl[ 9],
                           cbar_c->f_mapgrid_switch_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void read_colormap_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_colormap_ctl_c(fp, buf, label_cmap_cbar_ctl[0], cmap_cbar_c->cmap_c);
		read_colorbar_ctl_c(fp, buf, label_cmap_cbar_ctl[1], cmap_cbar_c->cbar_c);
	};
    cmap_cbar_c->f_iflag[0] = 1;
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
	
    cmap_cbar_c->f_iflag[0] = -1;
	return;
};

void write_colormap_file_c(const char *file_name, 
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
    printf("Write PVR colormap file name: %s\n", file_name);
	if ((FP_Colormap = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	write_colormap_colorbar_ctl_c(FP_Colormap, 0,
                                  label_colormap_head, cmap_cbar_c);
	fclose(FP_Colormap);
	return;
};
