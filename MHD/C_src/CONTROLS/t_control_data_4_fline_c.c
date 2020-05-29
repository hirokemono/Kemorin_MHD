/*
//  t_control_data_4_fline_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_fline_c.h"

#define NLBL_FLINE_CTL   14

FILE *FP_fline;

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

const char label_fline_head[KCHARA_C] = "fieldline";

void get_label_fline_ctl(int index, char *label){
    if(index < NLBL_FLINE_CTL) strngcopy(label, label_fline_ctl[index]);
    return;
};

struct fline_ctl_c * init_fline_ctl_c(){
	int i;
    struct fline_ctl_c *fline_c;
    if((fline_c = (struct fline_ctl_c *) malloc(sizeof(struct fline_ctl_c))) == NULL) {
        printf("malloc error for fline_ctl_c \n");
        exit(0);
    }
	
	fline_c->maxlen = 0;
	for (i=0;i<NLBL_FLINE_CTL;i++){
		if((int) strlen(label_fline_ctl[i]) > fline_c->maxlen){
			fline_c->maxlen = (int) strlen(label_fline_ctl[i]);
		};
	};
	
	fline_c->fline_file_head_ctl = init_chara_ctl_item_c();
	fline_c->fline_output_type_ctl = init_chara_ctl_item_c();
	
    fline_c->fline_field_ctl = init_chara_ctl_item_c();
	fline_c->fline_color_field_ctl = init_chara_ctl_item_c();
	fline_c->fline_color_comp_ctl = init_chara_ctl_item_c();
	
    fline_c->fline_area_grp_list = (struct chara_clist *) malloc(sizeof(struct chara_clist));
	init_chara_clist(fline_c->fline_area_grp_list);
	
	fline_c->starting_type_ctl = init_chara_ctl_item_c();
	fline_c->selection_type_ctl = init_chara_ctl_item_c();
	fline_c->line_direction_ctl = init_chara_ctl_item_c();
	
	fline_c->start_surf_grp_ctl = init_chara_ctl_item_c();
	fline_c->num_fieldline_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	fline_c->max_line_stepping_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_int_ctl_item_c(fline_c->num_fieldline_ctl);
	init_int_ctl_item_c(fline_c->max_line_stepping_ctl);
	
    fline_c->seed_point_list = (struct real3_clist *) malloc(sizeof(struct real3_clist));
	init_real3_clist(fline_c->seed_point_list);
    sprintf(fline_c->seed_point_list->r1_name, "x");
    sprintf(fline_c->seed_point_list->r2_name, "y");
    sprintf(fline_c->seed_point_list->r3_name, "z");

    fline_c->seed_surface_list = (struct int2_clist *) malloc(sizeof(struct int2_clist));
	init_int2_clist(fline_c->seed_surface_list);
    sprintf(fline_c->seed_surface_list->i1_name, "Element_ID");
    sprintf(fline_c->seed_surface_list->i2_name, "Surface_ID");
	
	return fline_c;
};

void dealloc_fline_ctl_c(struct fline_ctl_c *fline_c){
	
	dealloc_chara_ctl_item_c(fline_c->fline_file_head_ctl);
	dealloc_chara_ctl_item_c(fline_c->fline_output_type_ctl);
	
	dealloc_chara_ctl_item_c(fline_c->fline_field_ctl);
	dealloc_chara_ctl_item_c(fline_c->fline_color_field_ctl);
	dealloc_chara_ctl_item_c(fline_c->fline_color_comp_ctl);
	
	clear_chara_clist(fline_c->fline_area_grp_list);
    free(fline_c->fline_area_grp_list);
	
	dealloc_chara_ctl_item_c(fline_c->starting_type_ctl);
	dealloc_chara_ctl_item_c(fline_c->selection_type_ctl);
	dealloc_chara_ctl_item_c(fline_c->line_direction_ctl);
	
	dealloc_chara_ctl_item_c(fline_c->start_surf_grp_ctl);
	free(fline_c->num_fieldline_ctl);
	free(fline_c->max_line_stepping_ctl);
	
	clear_real3_clist(fline_c->seed_point_list);
    free(fline_c->seed_point_list);
	clear_int2_clist(fline_c->seed_surface_list);
    free(fline_c->seed_surface_list);
	
	free(fline_c);
	return;
};

int read_fline_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct fline_ctl_c *fline_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_fline_ctl[ 0], fline_c->fline_file_head_ctl);
		read_chara_ctl_item_c(buf, label_fline_ctl[ 1], fline_c->fline_output_type_ctl);
		
		read_chara_clist(fp, buf, label_fline_ctl[ 2], fline_c->fline_area_grp_list);
		
		read_chara_ctl_item_c(buf, label_fline_ctl[ 3], fline_c->fline_field_ctl);
		read_chara_ctl_item_c(buf, label_fline_ctl[ 4], fline_c->fline_color_field_ctl);
		read_chara_ctl_item_c(buf, label_fline_ctl[ 5], fline_c->fline_color_comp_ctl);
		
		read_chara_ctl_item_c(buf, label_fline_ctl[ 6], fline_c->starting_type_ctl);
		read_chara_ctl_item_c(buf, label_fline_ctl[ 7], fline_c->selection_type_ctl);
		read_chara_ctl_item_c(buf, label_fline_ctl[ 8], fline_c->line_direction_ctl);
		
		read_integer_ctl_item_c(buf, label_fline_ctl[ 9], fline_c->num_fieldline_ctl);
		read_integer_ctl_item_c(buf, label_fline_ctl[10], fline_c->max_line_stepping_ctl);
		
		read_chara_ctl_item_c(buf, label_fline_ctl[11], fline_c->start_surf_grp_ctl);
		
		read_real3_clist(fp, buf, label_fline_ctl[12], fline_c->seed_point_list);
		read_int2_clist(fp, buf, label_fline_ctl[13], fline_c->seed_surface_list);
	};
	return 1;
};

int write_fline_ctl_c(FILE *fp, int level, const char *label, 
			struct fline_ctl_c *fline_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 0], fline_c->fline_file_head_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 1], fline_c->fline_output_type_ctl);
	
	write_chara_clist(fp, level, label_fline_ctl[ 2], fline_c->fline_area_grp_list);
	
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 3], fline_c->fline_field_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 4], fline_c->fline_color_field_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 5], fline_c->fline_color_comp_ctl);
	
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 6], fline_c->starting_type_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 7], fline_c->selection_type_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 8], fline_c->line_direction_ctl);
	
	write_integer_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[ 9], fline_c->num_fieldline_ctl);
	write_integer_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[10], fline_c->max_line_stepping_ctl);
	
	write_chara_ctl_item_c(fp, level, fline_c->maxlen, label_fline_ctl[11], fline_c->start_surf_grp_ctl);
	
	write_real3_clist(fp, level, label_fline_ctl[12], fline_c->seed_point_list);
	write_int2_clist(fp, level, label_fline_ctl[13], fline_c->seed_surface_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};



int read_fline_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
			struct fline_ctl_c *fline_c){
	int iflag = 0;
	
	if ((FP_fline = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	skip_comment_read_line(FP_fline, buf);
	if(right_begin_flag_c(buf, label_fline_head) > 0){
		iflag = read_fline_ctl_c(FP_fline, buf, label_fline_head, fline_c);
	};
	fclose(FP_fline);
	
	return iflag;
};

int write_fline_ctl_file_c(const char *file_name, struct fline_ctl_c *fline_c){
	int level;
	
	if ((FP_fline = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	level = write_fline_ctl_c(FP_fline, 0, label_fline_head, fline_c);
	fclose(FP_fline);
	
	return level;
};
