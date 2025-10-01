/*
 *  write_modelview_matrix.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/11.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "write_modelview_matrix.h"

struct modelview_ctl_c *mat_c0;

void copy_mat44_from_ctl(struct chara2_real_ctl_list *head, double mat44[16]) {
	int i, j;
	
    head = head->_next;
    while (head != NULL){
		i = find_direction_from_ctl(head->c2r_item->c1_tbl);
		j = find_direction_from_ctl(head->c2r_item->c2_tbl);
		if(i>-1 && i<4 && j>-1 && j<4){mat44[i+4*j] = head->c2r_item->r_data;};
		
        head = head->_next;
    };
	return;
};
void copy_mat44_to_ctl(double mat44[16], struct chara2_real_clist *mat_clist){
    int i, j;
    char dir1[3], dir2[3];
	
	for(j=0;j<4;j++){
		for(i=0;i<4;i++){
			set_direction_from_ctl(i, dir1);
			set_direction_from_ctl(j, dir2);
            append_chara2_real_clist(dir1, dir2, mat44[i+4*j], mat_clist);
		};
	};
	
	
	return;
};

void copy_vector_from_ctl(struct chara_real_ctl_list *head, double vector[3]) {
	int i;
	
    head = head->_next;
    while (head != NULL){
		i = find_direction_from_ctl(head->cr_item->c_tbl);
		if(i>-1 && i<3) vector[i] = head->cr_item->r_data;
		
		head = head->_next;
	};
	
	return;
};
void copy_vector_to_ctl(double *vector, struct chara_real_clist *vec_clist) {
	int i;
    char dir1[3];
	
	for(i=0;i<3;i++){
		set_direction_from_ctl(i,dir1);
        append_chara_real_clist(dir1, vector[i], vec_clist);
	};
	return;
};


void copy_GL_stereo_params_to_ctl(struct view_element *view, struct streo_view_ctl_c *f_streo) {
	update_real_ctl_item_c(view->focal_length, f_streo->f_focalpoint_ctl);
	update_real_ctl_item_c(view->eye_separation, f_streo->f_eye_separation_ctl);
}


void copy_GL_modelview_params_to_ctl(struct view_element *view, struct modelview_ctl_c *mat_c) {
	int i;
	double viewpt_in_view[3];
	double lookat_in_view[3];
    double drotation[3];
	
	mat_c->f_iflag[0] = 1;
	
	for (i = 0; i < 3; i++) viewpt_in_view[i] = -view->shift[i];
	for (i = 0; i < 3; i++) lookat_in_view[i] = -view->shift[i];
    for (i = 0; i < 3; i++) drotation[i] = view->rotation[i+1];
	lookat_in_view[2] = view->x_lookat[2];
	
	mat_c->f_pixel->f_iflag[0] = 1;
	update_int_ctl_item_c(view->nx_frame, mat_c->f_pixel->f_num_xpixel_ctl);
	update_int_ctl_item_c(view->ny_frame, mat_c->f_pixel->f_num_ypixel_ctl);
	
	copy_vector_to_ctl(viewpt_in_view, mat_c->f_viewpt_in_viewer_ctl);
	
	update_real_ctl_item_c(view->iso_scale, mat_c->f_scale_factor_ctl);
	
	copy_vector_to_ctl(lookat_in_view, mat_c->f_lookpoint_ctl);
	
	copy_vector_to_ctl(drotation, mat_c->f_view_rot_vec_ctl);
	update_real_ctl_item_c(view->rotation[0], mat_c->f_view_rotation_deg_ctl);
	
    mat_c->f_proj->f_iflag[0] = 1;
	update_real_ctl_item_c(view->aperture, mat_c->f_proj->f_perspective_angle_ctl);
	update_real_ctl_item_c(view->aspect, mat_c->f_proj->f_perspective_xy_ratio_ctl);
	update_real_ctl_item_c(view->near, mat_c->f_proj->f_perspective_near_ctl);
	update_real_ctl_item_c(view->far, mat_c->f_proj->f_perspective_far_ctl);
	
	return;
}


void copy_GL_stereo_params_from_ctl(struct streo_view_ctl_c *f_streo, struct view_element *view) {
    set_from_real_ctl_item_c(f_streo->f_focalpoint_ctl, &view->focal_length);
    set_from_real_ctl_item_c(f_streo->f_eye_separation_ctl, &view->eye_separation);
	return;
}

void copy_GL_modelview_params_from_ctl(struct modelview_ctl_c *mat_c, struct view_element *view) {
	int i;
	double viewpt_in_view[3];
	double lookat_in_view[3];
    double drotation[3];
	
	if(mat_c->f_pixel->f_iflag[0] > 0){
		view->nx_frame = set_from_int_ctl_item_c(mat_c->f_pixel->f_num_xpixel_ctl);
		view->ny_frame = set_from_int_ctl_item_c(mat_c->f_pixel->f_num_ypixel_ctl);

        if(view->iflag_retina > 0){
            view->nx_window = view->nx_frame / 2;
            view->ny_window = view->ny_frame / 2;
        } else {
            view->nx_window = view->nx_frame;
            view->ny_window = view->ny_frame;
        }
	};
	
	copy_vector_from_ctl(&mat_c->f_viewpt_in_viewer_ctl->cr_item_head, viewpt_in_view);
    
    set_from_real_ctl_item_c(mat_c->f_scale_factor_ctl, &view->iso_scale);
    
	copy_vector_from_ctl(&mat_c->f_lookpoint_ctl->cr_item_head, lookat_in_view);
	
	copy_vector_from_ctl(&mat_c->f_view_rot_vec_ctl->cr_item_head, drotation);
    set_from_real_ctl_item_c(mat_c->f_view_rotation_deg_ctl, &view->rotation[0]);
	
    if(mat_c->f_proj->f_iflag[0] > 0){
        set_from_real_ctl_item_c(mat_c->f_proj->f_perspective_angle_ctl, &view->aperture);
        set_from_real_ctl_item_c(mat_c->f_proj->f_perspective_xy_ratio_ctl, &view->aspect);
        set_from_real_ctl_item_c(mat_c->f_proj->f_perspective_near_ctl, &view->near);
        set_from_real_ctl_item_c(mat_c->f_proj->f_perspective_far_ctl, &view->far);
    };
	
    for (i = 0; i < 3; i++) view->rotation[i+1] = drotation[i];
	for (i = 0; i < 3; i++) view->shift[i] = -viewpt_in_view[i];
	view->x_lookat[2] = lookat_in_view[2];
	return;
}


void write_GL_modelview_file(struct kv_string *filename, struct view_element *view){
	
	mat_c0 = init_modelview_ctl_c();
	
	copy_GL_modelview_params_to_ctl(view, mat_c0);
	if(view->iflag_view_type == VIEW_STEREO){
		mat_c0->f_streo->f_iflag[0] = 1;
		copy_GL_stereo_params_to_ctl(view, mat_c0->f_streo);
	};
    
	write_modelview_file_c(filename->string, mat_c0);
	free(mat_c0);
	return;
}


void read_GL_modelview_file(struct kv_string *filename, struct view_element *view){
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
	mat_c0 = init_modelview_ctl_c();
	
	read_modelview_file_c(filename->string, buf, mat_c0);
    
	copy_GL_modelview_params_from_ctl(mat_c0, view);
	if(mat_c0->f_streo->f_iflag[0] > 0){
		copy_GL_stereo_params_from_ctl(mat_c0->f_streo, view);
		view->iflag_view_type = VIEW_STEREO;
	};
	
	/*
	copy_mat44_from_ctl(view->mat_object_2_eye, &mat_c0->f_modelview_mat_ctl);
	if(mat_c0->f_proj->f_iflag[0] > 0){
		copy_mat44_from_ctl(view->mat_eye_2_clip, &mat_c0->projection_mat_ctl);
	}
	*/
	free(mat_c0);
	return;
}
