/*
//  t_control_data_4_pvr_movie_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/03.
*/

#include "t_control_data_4_pvr_movie_c.h"

struct pvr_movie_ctl_c * init_pvr_movie_ctl_c(){
    struct pvr_movie_ctl_c *movie_c;
    if((movie_c = (struct pvr_movie_ctl_c *) malloc(sizeof(struct pvr_movie_ctl_c))) == NULL) {
        printf("malloc error for pvr_movie_ctl_c \n");
        exit(0);
    }
    if((movie_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for movie_c->f_iflag\n");
        exit(0);
    }
    movie_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
    

	movie_c->label_lic_movie_ctl = init_label_lic_movie();
	
	movie_c->f_rotation_axis_ctl = init_chara_ctl_item_c();
    movie_c->f_num_frames_ctl =    init_int_ctl_item_c();
    return movie_c;
};



void dealloc_f_PVR_mul_vmats_ctls(struct void_clist *f_vmat_ctls){
    int i;
    for(i=0;i<count_void_clist(f_vmat_ctls);i++){
        struct modelview_ctl_c *f_mat = (struct modelview_ctl_c *) void_clist_at_index(i,f_vmat_ctls);
        dealloc_modelview_ctl_c(f_mat);
    }
    dealloc_void_clist(f_vmat_ctls);
    return;
};


void dealloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c){
	dealloc_control_labels_f(movie_c->label_lic_movie_ctl);
	
    dealloc_real2_ctl_item_c(movie_c->f_LIC_kernel_peak_range_ctl);
    dealloc_real2_ctl_item_c(movie_c->f_apature_range_ctl);
    dealloc_real2_ctl_item_c(movie_c->f_angle_range_ctl);

    dealloc_chara_ctl_item_c(movie_c->f_movie_mode_ctl);
	dealloc_chara_ctl_item_c(movie_c->f_rotation_axis_ctl);
	
    dealloc_f_PVR_mul_vmats_ctls(movie_c->f_mul_mmats_c);
	dealloc_modelview_ctl_c(movie_c->f_view_start_ctl);
	dealloc_modelview_ctl_c(movie_c->f_view_end_ctl);
    
    free(movie_c->c_block_name);
    movie_c->f_iflag = NULL;
    movie_c->f_self =  NULL;
    free(movie_c);
	return;
};

void read_pvr_movie_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_movie_ctl_c *movie_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 0],
							  movie_c->f_movie_mode_ctl);
		read_integer_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 1],
								movie_c->f_num_frames_ctl);
		
		read_chara_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 2],
							  movie_c->f_rotation_axis_ctl);
		
		read_real2_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 5], 
							  movie_c->f_apature_range_ctl);
		
		read_real2_ctl_item_c(buf, movie_c->label_lic_movie_ctl->label[ 6], 
							  movie_c->f_LIC_kernel_peak_range_ctl);
		
		if(right_begin_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 3]) > 0){
			read_modelview_ctl_c(fp, buf, movie_c->label_lic_movie_ctl->label[ 3],
								 movie_c->f_view_start_ctl);
		} else if(right_file_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 3])){
			movie_c->f_view_start_ctl->f_iflag[0]
					= read_file_flag_c(buf, movie_c->f_view_start_ctl->mat_ctl_file_name);
		};
		
		if(right_begin_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 4]) > 0){
			read_modelview_ctl_c(fp, buf, movie_c->label_lic_movie_ctl->label[ 4],
								 movie_c->f_view_end_ctl);
		} else if(right_file_flag_c(buf, movie_c->label_lic_movie_ctl->label[ 4])){
			movie_c->f_view_end_ctl->f_iflag[0]
					= read_file_flag_c(buf, movie_c->f_view_end_ctl->mat_ctl_file_name);
		};
	};
	movie_c->f_iflag[0] = 1;
    return;
};

static int write_pvr_movie_ctl_items_c(FILE *fp, int level, 
									   struct pvr_movie_ctl_c *movie_c){
	write_chara_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen,
						   movie_c->label_lic_movie_ctl->label[ 0],
						   movie_c->f_movie_mode_ctl);
	write_integer_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen,
							 movie_c->label_lic_movie_ctl->label[ 1],
							 movie_c->f_num_frames_ctl);
	
	write_chara_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen,
						   movie_c->label_lic_movie_ctl->label[ 2],
						   movie_c->f_rotation_axis_ctl);
	
	write_real2_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen, 
						   movie_c->label_lic_movie_ctl->label[ 5], 
						   movie_c->f_apature_range_ctl);
	
	if(movie_c->f_view_start_ctl->f_iflag[0] > 0){
		level = write_modelview_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 3], 
									  movie_c->f_view_start_ctl);
	} else if(movie_c->f_view_start_ctl->f_iflag[0] == -1){
		write_file_flag_for_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 3], 
								  movie_c->f_view_start_ctl->mat_ctl_file_name);
	};
	
	if(movie_c->f_view_end_ctl->f_iflag[0] > 0){
		level = write_modelview_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 4], 
									  movie_c->f_view_end_ctl);
	} else if(movie_c->f_view_end_ctl->f_iflag[0] == -1){
		write_file_flag_for_ctl_c(fp, level, movie_c->label_lic_movie_ctl->label[ 4], 
								  movie_c->f_view_end_ctl->mat_ctl_file_name);
	};
	return level;
};

int write_pvr_movie_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_movie_ctl_c *movie_c){
    if(movie_c->f_iflag[0] == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	level = write_pvr_movie_ctl_items_c(fp, level, movie_c);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


int write_lic_movie_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_movie_ctl_c *movie_c){
    if(movie_c->f_iflag[0] == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	level = write_pvr_movie_ctl_items_c(fp, level, movie_c);
	write_real2_ctl_item_c(fp, level, movie_c->label_lic_movie_ctl->maxlen, 
						   movie_c->label_lic_movie_ctl->label[ 6], 
						   movie_c->f_LIC_kernel_peak_range_ctl);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
