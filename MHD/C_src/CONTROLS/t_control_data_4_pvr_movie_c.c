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


