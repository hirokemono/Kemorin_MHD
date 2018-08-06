/*
//  t_control_data_LIC_pvr_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/06.
*/

#include "t_control_data_LIC_pvr_c.h"

#define NLBL_LIC_PVR_CTL            18

const char label_LIC_pvr_ctl[NLBL_LIC_PVR_CTL][KCHARA_C] = {
	/*[ 0]*/	{"updated_sign"},
	
	/*[ 1]*/	{"pvr_file_head"},
	/*[ 2]*/	{"pvr_output_type"},
	
	/*[ 3]*/	{"monitoring_mode"},
	/*[ 4]*/	{"image_tranceparency"},
	/*[ 5]*/	{"streo_imaging"},
	/*[ 6]*/	{"anaglyph_image"},
	
	/*[ 7]*/	{"output_field"},
	/*[ 8]*/	{"output_component"},
	
	/*[ 9]*/	{"plot_area_ctl"},
	/*[10]*/	{"view_transform_ctl"},
	/*[11]*/	{"pvr_color_ctl"},
	/*[12]*/	{"lighting_ctl"},
	/*[13]*/	{"colorbar_ctl"},
	/*[14]*/	{"image_rotation_ctl"},
	
	/*[15]*/	{"section_ctl"},
	/*[16]*/	{"isosurface_ctl"},
	
	/*[17]*/	{"LIC_ctl"}
};


void alloc_lic_pvr_sections_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	int i;
	
	for(i=0;i<lic_pvr_c->num_pvr_sect_ctl;i++){
		lic_pvr_c->pvr_sect_ctl[i] = (struct pvr_section_ctl_c *) malloc(sizeof(struct pvr_section_ctl_c));
		alloc_pvr_section_ctl_c(lic_pvr_c->pvr_sect_ctl[i]);
	};
	
	return;
};

int read_lic_pvr_sections_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_pvr_ctl_c *lic_pvr_c){
	int iflag = 0;
	int icou = 0;
	
	alloc_lic_pvr_sections_ctl_c(lic_pvr_c);
	while(find_control_end_array_flag_c(buf, label, lic_pvr_c->num_pvr_sect_ctl, icou) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		if(right_begin_flag_c(buf, label) > 0){
			iflag = read_pvr_section_ctl_c(fp, buf, label, lic_pvr_c->pvr_sect_ctl[icou]);
			icou = icou + iflag;
		};
	};
	return 1;
}

int write_lic_pvr_sections_ctl_c(FILE *fp, int level, const char *label, 
			struct LIC_pvr_ctl_c *lic_pvr_c){
	int i;
	
	if(lic_pvr_c->num_pvr_sect_ctl == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, lic_pvr_c->num_pvr_sect_ctl);
	for(i=0;i<lic_pvr_c->num_pvr_sect_ctl;i++){
		write_pvr_section_ctl_c(fp, level, label, lic_pvr_c->pvr_sect_ctl[i]);
		fprintf(fp, "!\n");
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_lic_pvr_isosurfs_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	int i;
	
	for(i=0;i<lic_pvr_c->num_pvr_iso_ctl;i++){
		lic_pvr_c->pvr_iso_ctl[i] = (struct pvr_isosurf_ctl_c *) malloc(sizeof(struct pvr_isosurf_ctl_c));
		alloc_pvr_isosurf_ctl_c(lic_pvr_c->pvr_iso_ctl[i]);
	};
	
	return;
};

int read_lic_pvr_isosurfs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_pvr_ctl_c *lic_pvr_c){
	int iflag = 0;
	int icou = 0;
	
	alloc_lic_pvr_isosurfs_ctl_c(lic_pvr_c);
	while(find_control_end_array_flag_c(buf, label, lic_pvr_c->num_pvr_iso_ctl, icou) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		if(right_begin_flag_c(buf, label) > 0){
			iflag = read_pvr_isosurf_ctl_c(fp, buf, label, lic_pvr_c->pvr_iso_ctl[icou]);
			icou = icou + iflag;
		};
	};
	return 1;
}

int write_lic_pvr_isosurfs_ctl_c(FILE *fp, int level, const char *label, 
			struct LIC_pvr_ctl_c *lic_pvr_c){
	int i;
	
	if(lic_pvr_c->num_pvr_iso_ctl == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, lic_pvr_c->num_pvr_iso_ctl);
	for(i=0;i<lic_pvr_c->num_pvr_iso_ctl;i++){
		write_pvr_isosurf_ctl_c(fp, level, label, lic_pvr_c->pvr_iso_ctl[i]);
		fprintf(fp, "!\n");
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};



void alloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	int i;
	
	lic_pvr_c->maxlen = 0;
	for (i=0;i<NLBL_LIC_PVR_CTL;i++){
		if(strlen(label_LIC_pvr_ctl[i]) > lic_pvr_c->maxlen){
			lic_pvr_c->maxlen = strlen(label_LIC_pvr_ctl[i]);
		};
	};
	
	lic_pvr_c->updated_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(lic_pvr_c->updated_ctl);
	
	lic_pvr_c->file_head_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_pvr_c->file_fmt_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_pvr_c->monitoring_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_pvr_c->transparent_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(lic_pvr_c->file_head_ctl);
	alloc_ctl_chara_item(lic_pvr_c->file_fmt_ctl);
	alloc_ctl_chara_item(lic_pvr_c->monitoring_ctl);
	alloc_ctl_chara_item(lic_pvr_c->transparent_ctl);
	
	lic_pvr_c->streo_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_pvr_c->anaglyph_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(lic_pvr_c->streo_ctl);
	alloc_ctl_chara_item(lic_pvr_c->anaglyph_ctl);
	
	lic_pvr_c->pvr_field_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	lic_pvr_c->pvr_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(lic_pvr_c->pvr_field_ctl);
	alloc_ctl_chara_item(lic_pvr_c->pvr_comp_ctl);
	
	lic_pvr_c->iflag_plot_area_ctl =    0;
	lic_pvr_c->iflag_modeview_ctl =     0;
	lic_pvr_c->iflag_colormap_ctl =     0;
	lic_pvr_c->iflag_lighting_ctl =     0;
	lic_pvr_c->iflag_pvr_colorbar_ctl = 0;
	lic_pvr_c->iflag_pvr_movie_ctl =    0;
	lic_pvr_c->iflag_lic_ctl =          0;
	
	lic_pvr_c->area_c = (struct pvr_plot_area_ctl_c *) malloc(sizeof(struct pvr_plot_area_ctl_c));
	alloc_pvr_plot_area_ctl_c(lic_pvr_c->area_c);
	
	lic_pvr_c->mat_c = (struct modeview_ctl_c *) malloc(sizeof(struct modeview_ctl_c));
	alloc_modeview_ctl_c(lic_pvr_c->mat_c);
	
	lic_pvr_c->color_c = (struct colormap_ctl_c *) malloc(sizeof(struct colormap_ctl_c));
	alloc_colormap_ctl_c(lic_pvr_c->color_c);
	
	lic_pvr_c->light_c = (struct lighting_ctl_c *) malloc(sizeof(struct lighting_ctl_c));
	alloc_lighting_ctl_c(lic_pvr_c->light_c);
	
	lic_pvr_c->cbar_c = (struct pvr_colorbar_ctl_c *) malloc(sizeof(struct pvr_colorbar_ctl_c));
	alloc_pvr_colorbar_ctl_c(lic_pvr_c->cbar_c);
	
	lic_pvr_c->movie_c = (struct pvr_movie_ctl_c *) malloc(sizeof(struct pvr_movie_ctl_c));
	alloc_pvr_movie_ctl_c(lic_pvr_c->movie_c);
	
	lic_pvr_c->lic_c = (struct lic_ctl_c *) malloc(sizeof(struct lic_ctl_c));
	alloc_lic_ctl_c(lic_pvr_c->lic_c);
	
	lic_pvr_c->num_pvr_sect_ctl = 0;
	lic_pvr_c->num_pvr_iso_ctl = 0;
	lic_pvr_c->pvr_sect_ctl = (struct pvr_section_ctl_c **) malloc(sizeof(struct pvr_section_ctl_c *));
	lic_pvr_c->pvr_iso_ctl = (struct pvr_isosurf_ctl_c **) malloc(sizeof(struct pvr_isosurf_ctl_c *));
	
	return;
};

void dealloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	int i;
	
	dealloc_ctl_chara_item(lic_pvr_c->updated_ctl);
	free(lic_pvr_c->updated_ctl);
	
	dealloc_ctl_chara_item(lic_pvr_c->file_head_ctl);
	dealloc_ctl_chara_item(lic_pvr_c->file_fmt_ctl);
	dealloc_ctl_chara_item(lic_pvr_c->monitoring_ctl);
	dealloc_ctl_chara_item(lic_pvr_c->transparent_ctl);
	free(lic_pvr_c->file_head_ctl);
	free(lic_pvr_c->file_fmt_ctl);
	free(lic_pvr_c->monitoring_ctl);
	free(lic_pvr_c->transparent_ctl);
	
	dealloc_ctl_chara_item(lic_pvr_c->streo_ctl);
	dealloc_ctl_chara_item(lic_pvr_c->anaglyph_ctl);
	free(lic_pvr_c->streo_ctl);
	free(lic_pvr_c->anaglyph_ctl);
	
	dealloc_ctl_chara_item(lic_pvr_c->pvr_field_ctl);
	dealloc_ctl_chara_item(lic_pvr_c->pvr_comp_ctl);
	free(lic_pvr_c->pvr_field_ctl);
	free(lic_pvr_c->pvr_comp_ctl);
	
	dealloc_pvr_plot_area_ctl_c(lic_pvr_c->area_c);
	free(lic_pvr_c->area_c);
	
	dealloc_modeview_ctl_c(lic_pvr_c->mat_c);
	free(lic_pvr_c->mat_c);
	
	dealloc_colormap_ctl_c(lic_pvr_c->color_c);
	free(lic_pvr_c->color_c);
	
	dealloc_lighting_ctl_c(lic_pvr_c->light_c);
	free(lic_pvr_c->light_c);
	
	dealloc_pvr_colorbar_ctl_c(lic_pvr_c->cbar_c);
	free(lic_pvr_c->cbar_c);
	
	dealloc_pvr_movie_ctl_c(lic_pvr_c->movie_c);
	free(lic_pvr_c->movie_c);
	
	dealloc_lic_ctl_c(lic_pvr_c->lic_c);
	free(lic_pvr_c->lic_c);
	
	
	for(i=0;i<lic_pvr_c->num_pvr_sect_ctl;i++){
		dealloc_pvr_section_ctl_c(lic_pvr_c->pvr_sect_ctl[i]);
		free(lic_pvr_c->pvr_sect_ctl[i]);
	};
	free(lic_pvr_c->pvr_sect_ctl);
	lic_pvr_c->num_pvr_sect_ctl = 0;
	
	for(i=0;i<lic_pvr_c->num_pvr_iso_ctl;i++){
		dealloc_pvr_isosurf_ctl_c(lic_pvr_c->pvr_iso_ctl[i]);
		free(lic_pvr_c->pvr_iso_ctl[i]);
	};
	free(lic_pvr_c->pvr_iso_ctl);
	lic_pvr_c->num_pvr_iso_ctl = 0;
	
	lic_pvr_c->iflag_plot_area_ctl =    0;
	lic_pvr_c->iflag_modeview_ctl =     0;
	lic_pvr_c->iflag_colormap_ctl =     0;
	lic_pvr_c->iflag_lighting_ctl =     0;
	lic_pvr_c->iflag_pvr_colorbar_ctl = 0;
	lic_pvr_c->iflag_pvr_movie_ctl =    0;
	lic_pvr_c->iflag_lic_ctl =          0;
	
	return;
};


int read_LIC_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 0], lic_pvr_c->updated_ctl);
		
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 1], lic_pvr_c->file_head_ctl);
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 2], lic_pvr_c->file_fmt_ctl);
		
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 3], lic_pvr_c->monitoring_ctl);
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 4], lic_pvr_c->transparent_ctl);
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 5], lic_pvr_c->streo_ctl);
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 6], lic_pvr_c->anaglyph_ctl);
		
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 7], lic_pvr_c->pvr_field_ctl);
		read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 8], lic_pvr_c->pvr_comp_ctl);
		
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[17]) > 0){
			lic_pvr_c->iflag_lic_ctl = read_lic_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[17], lic_pvr_c->lic_c);
		};
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[ 9]) > 0){
			lic_pvr_c->iflag_plot_area_ctl = read_pvr_plot_area_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[ 9], lic_pvr_c->area_c);
		};
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[10]) > 0){
			lic_pvr_c->iflag_modeview_ctl = read_modeview_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[10], lic_pvr_c->mat_c);
		};
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[11]) > 0){
			lic_pvr_c->iflag_colormap_ctl = read_colormap_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[11], lic_pvr_c->color_c);
		};
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[12]) > 0){
			lic_pvr_c->iflag_lighting_ctl = read_lighting_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[12], lic_pvr_c->light_c);
		};
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[13]) > 0){
			lic_pvr_c->iflag_pvr_colorbar_ctl = read_pvr_colorbar_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[13], lic_pvr_c->cbar_c);
		};
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[14]) > 0){
			lic_pvr_c->iflag_pvr_movie_ctl = read_pvr_movie_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[14], lic_pvr_c->movie_c);
		};
		
		iflag = find_control_array_flag_c(buf, label_LIC_pvr_ctl[15], &lic_pvr_c->num_pvr_sect_ctl);
		if(iflag > 0) iflag = read_lic_pvr_sections_ctl_c(fp, buf, label_LIC_pvr_ctl[15], lic_pvr_c);
		
		iflag = find_control_array_flag_c(buf, label_LIC_pvr_ctl[16], &lic_pvr_c->num_pvr_iso_ctl);
		if(iflag > 0) iflag = read_lic_pvr_isosurfs_ctl_c(fp, buf, label_LIC_pvr_ctl[16], lic_pvr_c);
	};
	return 1;
};

int write_LIC_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 0], lic_pvr_c->updated_ctl);
	
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 1], lic_pvr_c->file_head_ctl);
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 2], lic_pvr_c->file_fmt_ctl);
	
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 3], lic_pvr_c->monitoring_ctl);
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 4], lic_pvr_c->transparent_ctl);
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 5], lic_pvr_c->streo_ctl);
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 6], lic_pvr_c->anaglyph_ctl);
	
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 7], lic_pvr_c->pvr_field_ctl);
	write_character_ctl_item_c(fp, level, lic_pvr_c->maxlen, label_LIC_pvr_ctl[ 8], lic_pvr_c->pvr_comp_ctl);
	
	if(lic_pvr_c->iflag_lic_ctl > 0){
		fprintf(fp, "!\n");
		write_lic_ctl_c(fp, level, label_LIC_pvr_ctl[17], lic_pvr_c->lic_c);
	};
	if(lic_pvr_c->iflag_plot_area_ctl > 0){
		fprintf(fp, "!\n");
		write_pvr_plot_area_ctl_c(fp, level, label_LIC_pvr_ctl[ 9], lic_pvr_c->area_c);
	};
	
	if(lic_pvr_c->iflag_modeview_ctl > 0){
		fprintf(fp, "!\n");
		write_modeview_ctl_c(fp, level, label_LIC_pvr_ctl[10], lic_pvr_c->mat_c);
	};
	
	if(lic_pvr_c->iflag_colormap_ctl > 0){
		fprintf(fp, "!\n");
		write_colormap_ctl_c(fp, level, label_LIC_pvr_ctl[11], lic_pvr_c->color_c);
	};
	
	if(lic_pvr_c->iflag_lighting_ctl > 0){
		fprintf(fp, "!\n");
		write_lighting_ctl_c(fp, level, label_LIC_pvr_ctl[12], lic_pvr_c->light_c);
	};
	
	if(lic_pvr_c->iflag_pvr_colorbar_ctl > 0){
		fprintf(fp, "!\n");
		write_pvr_colorbar_ctl_c(fp, level, label_LIC_pvr_ctl[13], lic_pvr_c->cbar_c);
	};
	if(lic_pvr_c->iflag_pvr_movie_ctl > 0){
		fprintf(fp, "!\n");
		write_pvr_movie_ctl_c(fp, level, label_LIC_pvr_ctl[14], lic_pvr_c->movie_c);
	};
	
	
	if(lic_pvr_c->num_pvr_sect_ctl > 0){
		fprintf(fp, "!\n");
		level = write_lic_pvr_sections_ctl_c(fp, level, label_LIC_pvr_ctl[15], lic_pvr_c);
	};
	if(lic_pvr_c->num_pvr_iso_ctl > 0){
		fprintf(fp, "!\n");
		level = write_lic_pvr_isosurfs_ctl_c(fp, level, label_LIC_pvr_ctl[16], lic_pvr_c);
	};
	
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
