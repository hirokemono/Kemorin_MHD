/*
//  t_control_data_4_pvr_movie_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/03.
*/

#include "t_control_data_4_pvr_movie_c.h"

struct pvr_movie_ctl_c * init_pvr_movie_ctl_c(){
	int i;
    struct pvr_movie_ctl_c *movie_c;
    if((movie_c = (struct pvr_movie_ctl_c *) malloc(sizeof(struct pvr_movie_ctl_c))) == NULL) {
        printf("malloc error for pvr_movie_ctl_c \n");
        exit(0);
    }
	
	movie_c->iflag_use = 0;
	movie_c->label_lic_movie_ctl = init_label_lic_movie();
	
	movie_c->rotation_axis_ctl = init_chara_ctl_item_c();
    movie_c->num_frames_ctl =    init_int_ctl_item_c();
	
	return movie_c;
};

void dealloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c){
	dealloc_control_labels_f(movie_c->label_lic_movie_ctl);
	
	dealloc_chara_ctl_item_c(movie_c->movie_mode_ctl);
	dealloc_chara_ctl_item_c(movie_c->rotation_axis_ctl);
	
	dealloc_modelview_ctl_c(movie_c->view_start_mat_c);
	dealloc_modelview_ctl_c(movie_c->view_end_mat_c);
	free(movie_c->start_view_file_name);
	free(movie_c->end_view_file_name);
	free(movie_c);
	return;
};

void read_pvr_movie_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_movie_ctl_c *movie_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 0],
							  movie_c->movie_mode_ctl);
		read_integer_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 1],
								movie_c->num_frames_ctl);
		
		read_chara_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 2],
							  movie_c->rotation_axis_ctl);
		
		read_real2_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 5], 
							  movie_c->apature_range_ctl);
		
		read_real2_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 6], 
							  movie_c->LIC_kernel_peak_range_ctl);
		
		if(right_begin_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 3]) > 0){
			read_modelview_ctl_c(fp, buf, movie_c->label_lic_movie_ctl->label[ 3],
								 movie_c->view_start_mat_c);
		} else if(right_file_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 3])){
			movie_c->view_start_mat_c->iflag_use
					= read_file_flag_c(buf, movie_c->start_view_file_name);
		};
		
		if(right_begin_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 4]) > 0){
			read_modelview_ctl_c(fp, buf, movie_c->label_lic_movie_ctl->label[ 4],
								 movie_c->view_end_mat_c);
		} else if(right_file_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 4])){
			movie_c->view_end_mat_c->iflag_use
					= read_file_flag_c(buf, movie_c->end_view_file_name);
		};
	};
	movie_c->iflag_use = 1;
    return;
};

static int write_pvr_movie_ctl_items_c(FILE *fp, int level, 
									   struct pvr_movie_ctl_c *movie_c){
	write_chara_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen,
						   movie_c->label_lic_movie_ctl->label[ 0],
						   movie_c->movie_mode_ctl);
	write_integer_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen,
							 movie_c->label_lic_movie_ctl->label[ 1],
							 movie_c->num_frames_ctl);
	
	write_chara_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen,
						   movie_c->label_lic_movie_ctl->label[ 2],
						   movie_c->rotation_axis_ctl);
	
	write_real2_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen, 
						   movie_c->label_lic_movie_ctl->label[ 5], 
						   movie_c->apature_range_ctl);
	
	if(movie_c->view_start_mat_c->iflag_use > 0){
		level = write_modelview_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 3], 
									  movie_c->view_start_mat_c);
	} else if(movie_c->view_start_mat_c->iflag_use == -1){
		write_file_flag_for_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 3], 
								  movie_c->start_view_file_name);
	};
	
	if(movie_c->view_end_mat_c->iflag_use > 0){
		level = write_modelview_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 4], 
									  movie_c->view_end_mat_c);
	} else if(movie_c->view_end_mat_c->iflag_use == -1){
		write_file_flag_for_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 4], 
								  movie_c->end_view_file_name);
	};
	return level;
};

int write_pvr_movie_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_movie_ctl_c *movie_c){
    if(movie_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	level = write_pvr_movie_ctl_items_c(fp, level, movie_c);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


int write_lic_movie_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_movie_ctl_c *movie_c){
    if(movie_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	level = write_pvr_movie_ctl_items_c(fp, level, movie_c);
	write_real2_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen, 
						   movie_c->label_lic_movie_ctl->label[ 6], 
						   movie_c->LIC_kernel_peak_range_ctl);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
