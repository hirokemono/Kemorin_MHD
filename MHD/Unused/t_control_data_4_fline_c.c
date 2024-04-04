/*
//  t_control_data_4_fline_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_fline_c.h"

#define NLBL_FLINE_CTL   14

FILE *FP_fline;

const char label_fline_head[KCHARA_C] = "fieldline";

struct fline_ctl_c * init_fline_ctl_c(){
    struct fline_ctl_c *fline_c;
    if((fline_c = (struct fline_ctl_c *) malloc(sizeof(struct fline_ctl_c))) == NULL) {
        printf("malloc error for fline_ctl_c \n");
        exit(0);
    }
	
	fline_c->fline_ctl_lbls = init_fline_control_labels();
	fline_c->fline_file_head_ctl = init_chara_ctl_item_c();
	fline_c->fline_output_type_ctl = init_chara_ctl_item_c();
	
    fline_c->fline_field_ctl = init_chara_ctl_item_c();
	fline_c->fline_color_field_ctl = init_chara_ctl_item_c();
	fline_c->fline_color_comp_ctl = init_chara_ctl_item_c();
	
    fline_c->fline_area_grp_list = init_chara_clist();
	
	fline_c->starting_type_ctl = init_chara_ctl_item_c();
	fline_c->selection_type_ctl = init_chara_ctl_item_c();
	fline_c->line_direction_ctl = init_chara_ctl_item_c();
	
	fline_c->start_surf_grp_ctl =    init_chara_ctl_item_c();
    fline_c->num_fieldline_ctl =     init_int_ctl_item_c();
    fline_c->max_line_stepping_ctl = init_int_ctl_item_c();
	
    fline_c->seed_point_list = init_real3_clist();
    sprintf(fline_c->seed_point_list->r1_name, "x");
    sprintf(fline_c->seed_point_list->r2_name, "y");
    sprintf(fline_c->seed_point_list->r3_name, "z");

    fline_c->seed_surface_list = init_int2_clist();
    sprintf(fline_c->seed_surface_list->i1_name, "Element_ID");
    sprintf(fline_c->seed_surface_list->i2_name, "Surface_ID");
	
	return fline_c;
};

void dealloc_fline_ctl_c(struct fline_ctl_c *fline_c){
	dealloc_fline_control_labels(fline_c->fline_ctl_lbls);
	
	dealloc_chara_ctl_item_c(fline_c->fline_file_head_ctl);
	dealloc_chara_ctl_item_c(fline_c->fline_output_type_ctl);
	
	dealloc_chara_ctl_item_c(fline_c->fline_field_ctl);
	dealloc_chara_ctl_item_c(fline_c->fline_color_field_ctl);
	dealloc_chara_ctl_item_c(fline_c->fline_color_comp_ctl);
	
	dealloc_chara_clist(fline_c->fline_area_grp_list);
	
	dealloc_chara_ctl_item_c(fline_c->starting_type_ctl);
	dealloc_chara_ctl_item_c(fline_c->selection_type_ctl);
	dealloc_chara_ctl_item_c(fline_c->line_direction_ctl);
	
	dealloc_chara_ctl_item_c(fline_c->start_surf_grp_ctl);
	free(fline_c->num_fieldline_ctl);
	free(fline_c->max_line_stepping_ctl);
	
	dealloc_real3_clist(fline_c->seed_point_list);
	dealloc_int2_clist(fline_c->seed_surface_list);
	
	free(fline_c);
	return;
};

int read_fline_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct fline_ctl_c *fline_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 0],
							  fline_c->fline_file_head_ctl);
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 1],
							  fline_c->fline_output_type_ctl);
		
		read_chara_clist(fp, buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 2],
						 fline_c->fline_area_grp_list);
		
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 3],
							  fline_c->fline_field_ctl);
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 4],
							  fline_c->fline_color_field_ctl);
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 5],
							  fline_c->fline_color_comp_ctl);
		
		read_integer_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 6],
								fline_c->num_fieldline_ctl);
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 7],
							  fline_c->line_direction_ctl);
		read_integer_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 8],
								fline_c->max_line_stepping_ctl);
        
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 9],
							  fline_c->starting_type_ctl);
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[10],
							  fline_c->selection_type_ctl);
		
		read_chara_ctl_item_c(buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[11],
							  fline_c->start_surf_grp_ctl);
		
		read_real3_clist(fp, buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[12],
						 fline_c->seed_point_list);
		read_int2_clist(fp, buf, fline_c->fline_ctl_lbls->label_fline_ctl->label[13],
						fline_c->seed_surface_list);
	};
	return 1;
};

int write_fline_ctl_c(FILE *fp, int level, const char *label, 
			struct fline_ctl_c *fline_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[ 0],
						   fline_c->fline_file_head_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[ 1],
						   fline_c->fline_output_type_ctl);
	
	write_chara_clist(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->label[ 2],
					  fline_c->fline_area_grp_list);
	
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[ 3],
						   fline_c->fline_field_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[ 4],
						   fline_c->fline_color_field_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[ 5],
						   fline_c->fline_color_comp_ctl);
	
	write_integer_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
							 fline_c->fline_ctl_lbls->label_fline_ctl->label[ 6],
							 fline_c->num_fieldline_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[ 7], 
						   fline_c->line_direction_ctl);
	write_integer_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
							 fline_c->fline_ctl_lbls->label_fline_ctl->label[ 8],
							 fline_c->max_line_stepping_ctl);
    
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[ 9],
						   fline_c->starting_type_ctl);
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[10], 
						   fline_c->selection_type_ctl);
	
	write_chara_ctl_item_c(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->maxlen,
						   fline_c->fline_ctl_lbls->label_fline_ctl->label[11], 
						   fline_c->start_surf_grp_ctl);
	
	write_real3_clist(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->label[12],
					  fline_c->seed_point_list);
	write_int2_clist(fp, level, fline_c->fline_ctl_lbls->label_fline_ctl->label[13],
					 fline_c->seed_surface_list);
	
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
