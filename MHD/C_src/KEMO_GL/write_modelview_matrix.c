/*
 *  write_modelview_matrix.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/11.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "write_modelview_matrix.h"

struct modeview_ctl_c *mat_c0;

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


void copy_GL_stereo_params_to_ctl(struct view_element *view, struct streo_view_ctl_c *streo_view_c) {
	update_real_ctl_item_c(view->focal_length, streo_view_c->focalpoint_ctl);
	update_real_ctl_item_c(view->eye_separation, streo_view_c->eye_separation_ctl);
}


void copy_GL_modelview_params_to_ctl(struct view_element *view, struct modeview_ctl_c *mat_c) {
	int i;
	double viewpt_in_view[3];
	double lookat_in_view[3];
    double drotation[3];
	
	for (i = 0; i < 3; i++) viewpt_in_view[i] = -view->shift[i];
	for (i = 0; i < 3; i++) lookat_in_view[i] = -view->shift[i];
    for (i = 0; i < 3; i++) drotation[i] = view->rotation[i+1];
	lookat_in_view[2] = view->x_lookat[2];
	
	mat_c->iflag_image_size_ctl = 1;
	update_int_ctl_item_c(view->nx_window, mat_c->img_size_c->num_xpixel_ctl);
	update_int_ctl_item_c(view->ny_window, mat_c->img_size_c->num_ypixel_ctl);
	
	copy_vector_to_ctl(viewpt_in_view, mat_c->viewpt_in_viewer_list);
	
	update_real_ctl_item_c(view->iso_scale, mat_c->scale_factor_ctl);
	
	copy_vector_to_ctl(lookat_in_view, mat_c->lookpoint_list);
	
	copy_vector_to_ctl(drotation, mat_c->view_rot_vec_list);
	update_real_ctl_item_c(view->rotation[0], mat_c->view_rotation_deg_ctl);
	
    mat_c->iflag_projection_mat_ctl = 1;
	update_real_ctl_item_c(view->aperture, mat_c->projection_c->perspective_angle_ctl);
	update_real_ctl_item_c(view->aspect, mat_c->projection_c->perspective_xy_ratio_ctl);
	update_real_ctl_item_c(view->near, mat_c->projection_c->perspective_near_ctl);
	update_real_ctl_item_c(view->far, mat_c->projection_c->perspective_far_ctl);
	
	return;
}


void copy_GL_stereo_params_from_ctl(struct streo_view_ctl_c *streo_view_c, struct view_element *view) {
    set_from_real_ctl_item_c(streo_view_c->focalpoint_ctl, &view->focal_length);
    set_from_real_ctl_item_c(streo_view_c->eye_separation_ctl, &view->eye_separation);
	return;
}

void copy_GL_modelview_params_from_ctl(struct modeview_ctl_c *mat_c, struct view_element *view) {
	int i;
	double viewpt_in_view[3];
	double lookat_in_view[3];
    double drotation[3];
	
	if(mat_c->iflag_image_size_ctl > 0){
		view->nx_window = set_from_int_ctl_item_c(mat_c->img_size_c->num_xpixel_ctl);
		view->ny_window = set_from_int_ctl_item_c(mat_c->img_size_c->num_ypixel_ctl);
	};
	
	copy_vector_from_ctl(&mat_c->viewpt_in_viewer_list->cr_item_head, viewpt_in_view);
    
    set_from_real_ctl_item_c(mat_c->scale_factor_ctl, &view->iso_scale);
    
	copy_vector_from_ctl(&mat_c->lookpoint_list->cr_item_head, lookat_in_view);
	
	copy_vector_from_ctl(&mat_c->view_rot_vec_list->cr_item_head, drotation);
    set_from_real_ctl_item_c(mat_c->view_rotation_deg_ctl, &view->rotation[0]);
	
    if(mat_c->iflag_projection_mat_ctl > 0){
        set_from_real_ctl_item_c(mat_c->projection_c->perspective_angle_ctl, &view->aperture);
        set_from_real_ctl_item_c(mat_c->projection_c->perspective_xy_ratio_ctl, &view->aspect);
        set_from_real_ctl_item_c(mat_c->projection_c->perspective_near_ctl, &view->near);
        set_from_real_ctl_item_c(mat_c->projection_c->perspective_far_ctl, &view->far);
    };
	
    for (i = 0; i < 3; i++) view->rotation[i+1] = drotation[i];
	for (i = 0; i < 3; i++) view->shift[i] = -viewpt_in_view[i];
	view->x_lookat[2] = lookat_in_view[2];
	return;
}


void write_GL_modelview_file(const char *file_name, int iflag_view, struct view_element *view){
	
	mat_c0 = (struct modeview_ctl_c *) malloc(sizeof(struct modeview_ctl_c));
	alloc_modeview_ctl_c(mat_c0);
	
	copy_GL_modelview_params_to_ctl(view, mat_c0);
	if(iflag_view == VIEW_STEREO){
		mat_c0->iflag_streo_view_ctl = 1;
		copy_GL_stereo_params_to_ctl(view, mat_c0->streo_view_c);
	};
    /*
	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
    copy_mat44_to_ctl(view->mat_object_2_eye, mat_c0->modelview_mat_ctl);
    copy_mat44_to_ctl(view->mat_eye_2_clip, mat_c0->projection_mat_ctl);
    */
    
	write_modeview_file_c(file_name, mat_c0);
	free(mat_c0);
	return;
}


void read_GL_modelview_file(const char *file_name, int iflag_view, struct view_element *view){
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
	mat_c0 = (struct modeview_ctl_c *) malloc(sizeof(struct modeview_ctl_c));
	alloc_modeview_ctl_c(mat_c0);
	
	read_modeview_file_c(file_name, buf, mat_c0);
    
	copy_GL_modelview_params_from_ctl(mat_c0, view);
	if(mat_c0->iflag_streo_view_ctl > 0){
		copy_GL_stereo_params_from_ctl(mat_c0->streo_view_c, view);
		iflag_view = VIEW_STEREO;
	};
	
	/*
	copy_mat44_from_ctl(view->mat_object_2_eye, &mat_c0->modelview_mat_ctl);
	if(mat_c0->iflag_projection_mat_ctl > 0){
		copy_mat44_from_ctl(view->mat_eye_2_clip, &mat_c0->projection_mat_ctl);
	}
	*/
	free(mat_c0);
	return;
}
