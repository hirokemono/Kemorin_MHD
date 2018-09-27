/*
//  t_control_data_4_pvr_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_pvr_c.h"

FILE *FP_PVR;

const char label_pvr_plot_area_ctl[NLBL_PVR_PLOT_AREA_CTL][KCHARA_C] = {
	/*[ 0]*/	{"chosen_ele_grp_ctl"},
	/*[ 1]*/	{"surface_enhanse_ctl"},
};

const char label_pvr_movie_ctl[NLBL_PVR_MOVIE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"rotation_axis_ctl"},
	/*[ 1]*/	{"rotation_axis_ctl"},
};

const char label_pvr_ctl[NLBL_PVR_CTL][KCHARA_C] = {
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
	
	/*[17]*/	{"colormap_ctl"}
};

const char label_pvr_head[KCHARA_C] = "volume_rendering";


void get_label_pvr_plot_area_ctl(int index, char *label){
    if(index < NLBL_PVR_PLOT_AREA_CTL) strngcopy(label, label_pvr_plot_area_ctl[index]);
    return;
};
void get_label_pvr_movie_ctl(int index, char *label){
    if(index < NLBL_PVR_MOVIE_CTL) strngcopy(label, label_pvr_movie_ctl[index]);
    return;
};
void get_label_pvr_ctl(int index, char *label){
    if(index < NLBL_PVR_CTL) strngcopy(label, label_pvr_ctl[index]);
    return;
};


void alloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c){
	int i;
	
	area_c->maxlen = 0;
	for (i=0;i<NLBL_PVR_PLOT_AREA_CTL;i++){
		if(strlen(label_pvr_plot_area_ctl[i]) > area_c->maxlen){
			area_c->maxlen = (int) strlen(label_pvr_plot_area_ctl[i]);
		};
	};
	
    area_c->pvr_area_list = (struct chara_clist *) malloc(sizeof(struct chara_clist));
	init_chara_clist(area_c->pvr_area_list);
    area_c->surf_enhanse_ctl = (struct chara2_real_clist *) malloc(sizeof(struct chara2_real_clist));
	init_chara2_real_clist(area_c->surf_enhanse_ctl);
    sprintf(area_c->surf_enhanse_ctl->c1_name, "Group_name");
    sprintf(area_c->surf_enhanse_ctl->c2_name, "Direction");
    sprintf(area_c->surf_enhanse_ctl->r1_name, "Opacity");
	
	return;
};

void dealloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c){
	
	clear_chara_clist(area_c->pvr_area_list);
    free(area_c->pvr_area_list);
	clear_chara2_real_clist(area_c->surf_enhanse_ctl);
    free(area_c->surf_enhanse_ctl);
	
	return;
};

int read_pvr_plot_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_plot_area_ctl_c *area_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_clist(fp, buf, label_pvr_plot_area_ctl[0], area_c->pvr_area_list);
		read_chara2_real_clist(fp, buf, label_pvr_plot_area_ctl[1], area_c->surf_enhanse_ctl);
	};
	return 1;
};

int write_pvr_plot_area_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_plot_area_ctl_c *area_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_chara_clist(fp, level, label_pvr_plot_area_ctl[ 0], area_c->pvr_area_list);
	level = write_chara2_real_clist(fp, level, label_pvr_plot_area_ctl[ 1], area_c->surf_enhanse_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c){
	int i;
	
	movie_c->maxlen = 0;
	for (i=0;i<NLBL_PVR_MOVIE_CTL;i++){
		if(strlen(label_pvr_movie_ctl[i]) > movie_c->maxlen){
			movie_c->maxlen = (int) strlen(label_pvr_movie_ctl[i]);
		};
	};
	
	movie_c->rotation_axis_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	movie_c->num_frames_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	alloc_chara_ctl_item_c(movie_c->rotation_axis_ctl);
	init_int_ctl_item_c(movie_c->num_frames_ctl);
	
	return;
};

void dealloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c){
	
	dealloc_chara_ctl_item_c(movie_c->rotation_axis_ctl);
	free(movie_c->rotation_axis_ctl);
	free(movie_c->num_frames_ctl);
	
	return;
};

int read_pvr_movie_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_movie_ctl_c *movie_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_pvr_movie_ctl[ 0], movie_c->rotation_axis_ctl);
		read_integer_ctl_item_c(buf, label_pvr_movie_ctl[ 1], movie_c->num_frames_ctl);
	};
	return 1;
};

int write_pvr_movie_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_movie_ctl_c *movie_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, movie_c->maxlen, label_pvr_movie_ctl[ 0], movie_c->rotation_axis_ctl);
	write_integer_ctl_item_c(fp, level, movie_c->maxlen, label_pvr_movie_ctl[ 1], movie_c->num_frames_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c){
	int i;
	
	pvr_c->maxlen = 0;
	for (i=0;i<NLBL_PVR_CTL;i++){
		if(strlen(label_pvr_ctl[i]) > pvr_c->maxlen){
			pvr_c->maxlen = (int) strlen(label_pvr_ctl[i]);
		};
	};
	
	pvr_c->updated_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(pvr_c->updated_ctl);
	
	pvr_c->file_head_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	pvr_c->file_fmt_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	pvr_c->monitoring_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	pvr_c->transparent_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(pvr_c->file_head_ctl);
	alloc_chara_ctl_item_c(pvr_c->file_fmt_ctl);
	alloc_chara_ctl_item_c(pvr_c->monitoring_ctl);
	alloc_chara_ctl_item_c(pvr_c->transparent_ctl);
	
	pvr_c->streo_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	pvr_c->anaglyph_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(pvr_c->streo_ctl);
	alloc_chara_ctl_item_c(pvr_c->anaglyph_ctl);
	
	pvr_c->pvr_field_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	pvr_c->pvr_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(pvr_c->pvr_field_ctl);
	alloc_chara_ctl_item_c(pvr_c->pvr_comp_ctl);
	
	pvr_c->iflag_plot_area_ctl =    0;
	pvr_c->iflag_modeview_ctl =     0;
	pvr_c->iflag_lighting_ctl =     0;
	pvr_c->iflag_cmap_cbar_ctl =    0;
	pvr_c->iflag_pvr_movie_ctl =    0;
	
	pvr_c->area_c = (struct pvr_plot_area_ctl_c *) malloc(sizeof(struct pvr_plot_area_ctl_c));
	alloc_pvr_plot_area_ctl_c(pvr_c->area_c);
	
    pvr_c->pvr_modelview_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	pvr_c->mat_c = (struct modeview_ctl_c *) malloc(sizeof(struct modeview_ctl_c));
	alloc_modeview_ctl_c(pvr_c->mat_c);
	
	pvr_c->light_c = (struct lighting_ctl_c *) malloc(sizeof(struct lighting_ctl_c));
	alloc_lighting_ctl_c(pvr_c->light_c);
	
    pvr_c->pvr_colormap_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	pvr_c->cmap_cbar_c = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c));
	alloc_colormap_colorbar_ctl_c(pvr_c->cmap_cbar_c);
	
	pvr_c->movie_c = (struct pvr_movie_ctl_c *) malloc(sizeof(struct pvr_movie_ctl_c));
	alloc_pvr_movie_ctl_c(pvr_c->movie_c);
	
	init_pvr_section_ctl_list(&pvr_c->pvr_sect_c_list);
	init_pvr_iso_ctl_list(&pvr_c->pvr_iso_c_list);
	
	return;
};

void dealloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c){
    dealloc_chara_ctl_item_c(pvr_c->updated_ctl);
	free(pvr_c->updated_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->file_head_ctl);
	dealloc_chara_ctl_item_c(pvr_c->file_fmt_ctl);
	dealloc_chara_ctl_item_c(pvr_c->monitoring_ctl);
	dealloc_chara_ctl_item_c(pvr_c->transparent_ctl);
	free(pvr_c->file_head_ctl);
	free(pvr_c->file_fmt_ctl);
	free(pvr_c->monitoring_ctl);
	free(pvr_c->transparent_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->streo_ctl);
	dealloc_chara_ctl_item_c(pvr_c->anaglyph_ctl);
	free(pvr_c->streo_ctl);
	free(pvr_c->anaglyph_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->pvr_field_ctl);
	dealloc_chara_ctl_item_c(pvr_c->pvr_comp_ctl);
	free(pvr_c->pvr_field_ctl);
	free(pvr_c->pvr_comp_ctl);
	
	dealloc_pvr_plot_area_ctl_c(pvr_c->area_c);
	free(pvr_c->area_c);
	
	dealloc_modeview_ctl_c(pvr_c->mat_c);
	free(pvr_c->mat_c);
    free(pvr_c->pvr_modelview_file_name);
	
	dealloc_lighting_ctl_c(pvr_c->light_c);
	free(pvr_c->light_c);
	
	dealloc_colormap_colorbar_ctl_c(pvr_c->cmap_cbar_c);
	free(pvr_c->cmap_cbar_c);
    free(pvr_c->pvr_colormap_file_name);
	
	dealloc_pvr_movie_ctl_c(pvr_c->movie_c);
	free(pvr_c->movie_c);
	
	clear_pvr_section_ctl_list(&pvr_c->pvr_sect_c_list);
	clear_pvr_iso_ctl_list(&pvr_c->pvr_iso_c_list);
	
	pvr_c->iflag_plot_area_ctl =    0;
	pvr_c->iflag_modeview_ctl =     0;
	pvr_c->iflag_lighting_ctl =     0;
	pvr_c->iflag_cmap_cbar_ctl =    0;
	pvr_c->iflag_pvr_movie_ctl =    0;
	
	return;
};


void read_pvr_ctl_items(FILE *fp, char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c){
	int iflag;
	
	read_chara_ctl_item_c(buf, label_pvr_ctl[ 0], pvr_c->updated_ctl);
	
	read_chara_ctl_item_c(buf, label_pvr_ctl[ 3], pvr_c->monitoring_ctl);
	read_chara_ctl_item_c(buf, label_pvr_ctl[ 4], pvr_c->transparent_ctl);
	read_chara_ctl_item_c(buf, label_pvr_ctl[ 5], pvr_c->streo_ctl);
	read_chara_ctl_item_c(buf, label_pvr_ctl[ 6], pvr_c->anaglyph_ctl);
	
	read_chara_ctl_item_c(buf, label_pvr_ctl[ 7], pvr_c->pvr_field_ctl);
	read_chara_ctl_item_c(buf, label_pvr_ctl[ 8], pvr_c->pvr_comp_ctl);
	if(right_begin_flag_c(buf, label_pvr_ctl[ 9]) > 0){
		pvr_c->iflag_plot_area_ctl = read_pvr_plot_area_ctl_c(fp, buf, 
					label_pvr_ctl[ 9], pvr_c->area_c);
	};
	if(right_begin_flag_c(buf, label_pvr_ctl[10]) > 0){
		pvr_c->iflag_modeview_ctl = read_modeview_ctl_c(fp, buf, 
					label_pvr_ctl[10], pvr_c->mat_c);
	} else if(right_file_flag_c(buf, label_pvr_ctl[10])){
		pvr_c->iflag_modeview_ctl = read_file_flag_c(buf, pvr_c->pvr_modelview_file_name);
	};
	
	if(right_begin_flag_c(buf, label_pvr_ctl[12]) > 0){
		pvr_c->iflag_lighting_ctl = read_lighting_ctl_c(fp, buf, 
					label_pvr_ctl[12], pvr_c->light_c);
	};
	
	if(right_file_flag_c(buf, label_pvr_ctl[11])){
		pvr_c->iflag_cmap_cbar_ctl = read_file_flag_c(buf, pvr_c->pvr_colormap_file_name);
	}else{
		pvr_c->cmap_cbar_c->iflag_colormap_ctl = read_colormap_ctl_c(fp, buf, 
			label_pvr_ctl[11], pvr_c->cmap_cbar_c->cmap_c);
		pvr_c->cmap_cbar_c->iflag_colormap_ctl = read_colormap_ctl_c(fp, buf, 
			label_pvr_ctl[17], pvr_c->cmap_cbar_c->cmap_c);
		
		pvr_c->cmap_cbar_c->iflag_colorbar_ctl = read_colorbar_ctl_c(fp, buf, 
			label_pvr_ctl[13], pvr_c->cmap_cbar_c->cbar_c);
	};
	
	
	if(right_begin_flag_c(buf, label_pvr_ctl[14]) > 0){
		pvr_c->iflag_pvr_movie_ctl = read_pvr_movie_ctl_c(fp, buf, 
					label_pvr_ctl[14], pvr_c->movie_c);
	};
	
	iflag = read_pvr_section_ctl_list(fp, buf, label_pvr_ctl[15], &pvr_c->pvr_sect_c_list);
	iflag = read_pvr_iso_ctl_list(fp, buf, label_pvr_ctl[16], &pvr_c->pvr_iso_c_list);
	
	return;
};

int write_pvr_ctl_items(FILE *fp, int level, struct pvr_ctl_c *pvr_c){
	level = write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 0], pvr_c->updated_ctl);
	
	level = write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 3], pvr_c->monitoring_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 4], pvr_c->transparent_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 5], pvr_c->streo_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 6], pvr_c->anaglyph_ctl);
	
	level = write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 7], pvr_c->pvr_field_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 8], pvr_c->pvr_comp_ctl);
    
	if(pvr_c->iflag_plot_area_ctl > 0){
		fprintf(fp, "!\n");
		level = write_pvr_plot_area_ctl_c(fp, level, label_pvr_ctl[ 9], pvr_c->area_c);
	};
	
	if(pvr_c->iflag_modeview_ctl > 0){
		fprintf(fp, "!\n");
		level = write_modeview_ctl_c(fp, level, label_pvr_ctl[10], pvr_c->mat_c);
	} else if(pvr_c->iflag_modeview_ctl == -1){
		fprintf(fp, "!\n");
		write_file_flag_for_ctl_c(fp, level, label_pvr_ctl[10], pvr_c->pvr_modelview_file_name);
	};
	
	if(pvr_c->iflag_lighting_ctl > 0){
		fprintf(fp, "!\n");
		level = write_lighting_ctl_c(fp, level, label_pvr_ctl[12], pvr_c->light_c);
	};
	
	if(pvr_c->iflag_cmap_cbar_ctl == -1){
		fprintf(fp, "!\n");
		write_file_flag_for_ctl_c(fp, level, label_pvr_ctl[11], pvr_c->pvr_colormap_file_name);
	}else{
		if(pvr_c->cmap_cbar_c->iflag_colormap_ctl == 1){
			fprintf(fp, "!\n");
			level = write_colormap_ctl_c(fp, level, label_pvr_ctl[17], pvr_c->cmap_cbar_c->cmap_c);
		};
		if(pvr_c->cmap_cbar_c->iflag_colorbar_ctl == 1){
			fprintf(fp, "!\n");
			level = write_colorbar_ctl_c(fp, level, label_pvr_ctl[13], pvr_c->cmap_cbar_c->cbar_c);
		};
	};
	
	if(pvr_c->iflag_pvr_movie_ctl > 0){
		fprintf(fp, "!\n");
		level = write_pvr_movie_ctl_c(fp, level, label_pvr_ctl[14], pvr_c->movie_c);
	};
	
	level = write_pvr_section_ctl_list(fp, level, label_pvr_ctl[15], &pvr_c->pvr_sect_c_list);
	level = write_pvr_iso_ctl_list(fp, level, label_pvr_ctl[16], &pvr_c->pvr_iso_c_list);
	
	return level;
};

int read_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_ctl_c *pvr_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
        read_chara_ctl_item_c(buf, label_pvr_ctl[ 1], pvr_c->file_head_ctl);
        read_chara_ctl_item_c(buf, label_pvr_ctl[ 2], pvr_c->file_fmt_ctl);
        
		read_pvr_ctl_items(fp, buf, pvr_c);
	};
	return 1;
};

int write_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_ctl_c *pvr_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
    write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 1], pvr_c->file_head_ctl);
    write_chara_ctl_item_c(fp, level, pvr_c->maxlen, label_pvr_ctl[ 2], pvr_c->file_fmt_ctl);
    
	write_pvr_ctl_items(fp, level, pvr_c);
    
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void rename_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c){
    if(pvr_c->iflag_modeview_ctl ==-1){
        strcat(pvr_c->pvr_modelview_file_name, "_2");
    }
    if(pvr_c->iflag_cmap_cbar_ctl ==-1){
        strcat(pvr_c->pvr_colormap_file_name, "_2");
    }
	
	rename_pvr_section_subfile_list(&pvr_c->pvr_sect_c_list);
    return;
}

int read_pvr_ctl_subfiles(char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c){
    int iflag;
	if(pvr_c->iflag_modeview_ctl ==-1){
		iflag = read_modeview_file_c(pvr_c->pvr_modelview_file_name, buf,
							 pvr_c->mat_c);
	};
	if(pvr_c->iflag_cmap_cbar_ctl ==-1){
		iflag = read_colormap_file_c(pvr_c->pvr_colormap_file_name, buf,
							   pvr_c->cmap_cbar_c);
	};
	
	read_pvr_section_subfile_list(buf, &pvr_c->pvr_sect_c_list);
	
	return 0;
};

void write_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c){
    int iflag;
	if(pvr_c->iflag_modeview_ctl ==-1){
		iflag = write_modeview_file_c(pvr_c->pvr_modelview_file_name, pvr_c->mat_c);
	};
	if(pvr_c->iflag_cmap_cbar_ctl ==-1){
		iflag = write_colormap_file_c(pvr_c->pvr_colormap_file_name, pvr_c->cmap_cbar_c);
	};
	
	write_pvr_section_subfile_list(&pvr_c->pvr_sect_c_list);
	return;
};

int read_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
						struct pvr_ctl_c *pvr_c){
	int iflag = 0;
	
	if ((FP_PVR = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);					/* terminate with error message */
	};
	
	skip_comment_read_line(FP_PVR, buf);
	if(right_begin_flag_c(buf, label_pvr_head) > 0){
		iflag = read_pvr_ctl_c(FP_PVR, buf, label_pvr_head, pvr_c);
	};
	fclose(FP_PVR);
	
	read_pvr_ctl_subfiles(buf, pvr_c);
	
	return iflag;
};

int write_pvr_ctl_file_c(const char *file_name, struct pvr_ctl_c *pvr_c){
	int level;
	
	write_pvr_ctl_subfiles(pvr_c);
	
	if ((FP_PVR = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);					/* terminate with error message */
	};
	
	level = write_pvr_ctl_c(FP_PVR, 0, label_pvr_head, pvr_c);
	fclose(FP_PVR);
	
	return level;
};

