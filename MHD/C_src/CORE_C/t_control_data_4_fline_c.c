/*
//  t_control_data_4_fline_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_fline_c.h"

#define NLBL_FLINE_CTL   14

const char label_fline_ctl[NLBL_FLINE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"fline_file_head"},
	/*[ 1]*/	{"fline_output_type"},
	
	/*[ 2]*/	{"chosen_ele_grp_ctl"},
	/*[ 3]*/	{"field_line_field_ctl"},
	
	/*[ 4]*/	{"coloring_field_ctl"},
	/*[ 5]*/	{"coloring_comp_ctl"},
	
	/*[ 6]*/	{"starting_type_ctl"},
	/*[ 7]*/	{"selection_type_ctl"},
	/*[ 8]*/	{"line_direction_ctl"},
	/*[ 9]*/	{"num_fieldline_ctl"},
	/*[10]*/	{"max_line_stepping_ctl"},
	
	/*[11]*/	{"start_surf_grp_ctl"},
	/*[12]*/	{"starting_point_ctl"},
	/*[13]*/	{"starting_gl_surface_id"}
};

void alloc_fline_ctl_c(struct fline_ctl_c *fline_c){
	int i;
	
	fline_c->maxlen = 0;
	for (i=0;i<NLBL_FLINE_CTL;i++){
		if(strlen(label_fline_ctl[i]) > fline_c->maxlen){
			fline_c->maxlen = strlen(label_fline_ctl[i]);
		};
	};
	
	fline_c->fline_file_head_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	fline_c->fline_output_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(fline_c->fline_file_head_ctl);
	alloc_ctl_chara_item(fline_c->fline_output_type_ctl);
	
	fline_c->fline_field_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	fline_c->fline_color_field_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	fline_c->fline_color_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(fline_c->fline_field_ctl);
	alloc_ctl_chara_item(fline_c->fline_color_field_ctl);
	alloc_ctl_chara_item(fline_c->fline_color_comp_ctl);
	
	fline_c->fline_area_grp_ctl = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	
	fline_c->starting_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	fline_c->selection_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	fline_c->line_direction_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(fline_c->starting_type_ctl);
	alloc_ctl_chara_item(fline_c->selection_type_ctl);
	alloc_ctl_chara_item(fline_c->line_direction_ctl);
	
	fline_c->start_surf_grp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	fline_c->num_fieldline_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	fline_c->max_line_stepping_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	alloc_ctl_chara_item(fline_c->start_surf_grp_ctl);
	init_ctl_int_item(fline_c->num_fieldline_ctl);
	init_ctl_int_item(fline_c->max_line_stepping_ctl);
	
	fline_c->seed_point_ctl = (struct real3_ctl_array *) malloc(sizeof(struct real3_ctl_array));
	fline_c->seed_surface_ctl = (struct int2_ctl_array *) malloc(sizeof(struct int2_ctl_array));
	
	return;
};

void dealloc_fline_ctl_c(struct fline_ctl_c *fline_c){
	
	dealloc_ctl_chara_item(fline_c->fline_file_head_ctl);
	dealloc_ctl_chara_item(fline_c->fline_output_type_ctl);
	free(fline_c->fline_file_head_ctl);
	free(fline_c->fline_output_type_ctl);
	
	dealloc_ctl_chara_item(fline_c->fline_field_ctl);
	dealloc_ctl_chara_item(fline_c->fline_color_field_ctl);
	dealloc_ctl_chara_item(fline_c->fline_color_comp_ctl);
	free(fline_c->fline_field_ctl);
	free(fline_c->fline_color_field_ctl);
	free(fline_c->fline_color_comp_ctl);
	
	dealloc_ctl_chara_array(fline_c->fline_area_grp_ctl);
	free(fline_c->fline_area_grp_ctl);
	
	dealloc_ctl_chara_item(fline_c->starting_type_ctl);
	dealloc_ctl_chara_item(fline_c->selection_type_ctl);
	dealloc_ctl_chara_item(fline_c->line_direction_ctl);
	free(fline_c->starting_type_ctl);
	free(fline_c->selection_type_ctl);
	free(fline_c->line_direction_ctl);
	
	dealloc_ctl_chara_item(fline_c->start_surf_grp_ctl);
	free(fline_c->start_surf_grp_ctl);
	free(fline_c->num_fieldline_ctl);
	free(fline_c->max_line_stepping_ctl);
	
	dealloc_ctl_real3_array(fline_c->seed_point_ctl);
	dealloc_ctl_int2_array(fline_c->seed_surface_ctl);
	free(fline_c->seed_point_ctl);
	free(fline_c->seed_surface_ctl);
	
	return;
};

int read_psf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct fline_ctl_c *fline_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_fline_ctl[ 0], fline_c->fline_file_head_ctl);
		read_character_ctl_item_c(buf, label_fline_ctl[ 1], fline_c->fline_output_type_ctl);
		
		read_character_ctl_array_c(fp, buf, label_fline_ctl[ 2], fline_c->fline_area_grp_ctl);
		
		read_character_ctl_item_c(buf, label_fline_ctl[ 3], fline_c->fline_field_ctl);
		read_character_ctl_item_c(buf, label_fline_ctl[ 4], fline_c->fline_color_field_ctl);
		read_character_ctl_item_c(buf, label_fline_ctl[ 5], fline_c->fline_color_comp_ctl);
		
		read_character_ctl_item_c(buf, label_fline_ctl[ 6], fline_c->starting_type_ctl);
		read_character_ctl_item_c(buf, label_fline_ctl[ 7], fline_c->selection_type_ctl);
		read_character_ctl_item_c(buf, label_fline_ctl[ 8], fline_c->line_direction_ctl);
		
		read_integer_ctl_item_c(buf, label_fline_ctl[ 9], fline_c->num_fieldline_ctl);
		read_integer_ctl_item_c(buf, label_fline_ctl[10], fline_c->max_line_stepping_ctl);
		
		read_character_ctl_item_c(buf, label_fline_ctl[11], fline_c->start_surf_grp_ctl);
		
		read_real3_ctl_array_c(fp, buf, label_fline_ctl[12], fline_c->seed_point_ctl);
		read_int2_ctl_array_c(fp, buf, label_fline_ctl[13], fline_c->seed_surface_ctl);
	};
	return 1;
};

int write_fline_ctl_c(FILE *fp, int level, const char *label, 
			struct fline_ctl_c *fline_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 0], fline_c->fline_file_head_ctl);
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 1], fline_c->fline_output_type_ctl);
	
	if(fline_c->fline_area_grp_ctl->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, fline_c->maxlen, label_fline_ctl[ 2], fline_c->fline_area_grp_ctl);
	
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 3], fline_c->fline_field_ctl);
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 4], fline_c->fline_color_field_ctl);
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 5], fline_c->fline_color_comp_ctl);
	
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 6], fline_c->starting_type_ctl);
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 7], fline_c->selection_type_ctl);
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 8], fline_c->line_direction_ctl);
	
	write_integer_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 9], fline_c->num_fieldline_ctl);
	write_integer_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[10], fline_c->max_line_stepping_ctl);
	
	write_character_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[11], fline_c->start_surf_grp_ctl);
	
	if(fline_c->seed_point_ctl->num > 0) fprintf(fp, "!\n");
	write_real3_ctl_array_c(fp, level, fline_c->maxlen, label_fline_ctl[12], fline_c->seed_point_ctl);
	if(fline_c->seed_surface_ctl->num > 0) fprintf(fp, "!\n");
	write_int2_ctl_array_c(fp, level, fline_c->maxlen, label_fline_ctl[13], fline_c->seed_surface_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
