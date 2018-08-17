/*
//  t_ctl_data_4_view_transfer_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_ctl_data_4_view_transfer_c.h"

#define NLBL_IMAGE_SIZE_CTL       2
#define NLBL_STEREO_VIEW_CTL      2
#define NLBL_PROJECTION_MAT_CTL   4
#define NLBL_MODELVIEW_CTL       12

FILE *FP_View;

const char label_image_size_ctl[NLBL_IMAGE_SIZE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"x_pixel_ctl"},
	/*[ 1]*/	{"y_pixel_ctl"}
};

const char label_streo_view_ctl[NLBL_STEREO_VIEW_CTL][KCHARA_C] = {
	/*[ 0]*/	{"focal_point_ctl"},
	/*[ 1]*/	{"eye_separation_ctl"}
};

const char label_projection_mat_ctl[NLBL_PROJECTION_MAT_CTL][KCHARA_C] = {
	/*[ 0]*/	{"perspective_angle_ctl"},
	/*[ 1]*/	{"perspective_xy_ratio_ctl"},
	/*[ 2]*/	{"perspective_near_ctl"},
	/*[ 3]*/	{"perspective_far_ctl"}
};

const char label_modeview_ctl[NLBL_MODELVIEW_CTL][KCHARA_C] = {
	/*[ 0]*/	{"look_at_point_ctl"},
	/*[ 1]*/	{"viewpoint_ctl"},
	/*[ 2]*/	{"up_direction_ctl"},
	/*[ 3]*/	{"view_rotation_vec_ctl"},
	/*[ 4]*/	{"view_rotation_deg_ctl"},
	/*[ 5]*/	{"scale_factor_ctl"},
	/*[ 6]*/	{"scale_factor_vec_ctl"},
	/*[ 7]*/	{"viewpoint_in_viewer_ctl"},
	
	/*[ 8]*/	{"modelview_matrix_ctl"},
	
	/*[ 9]*/	{"image_size_ctl"},
	/*[10]*/	{"projection_matrix_ctl"},
	/*[11]*/	{"streo_view_parameter_ctl"}
};

const char label_modeview_head[KCHARA_C] = "view_transform_ctl";

void get_label_image_size_ctl(int index, char *label){
    if(index < NLBL_IMAGE_SIZE_CTL) strngcopy(label, label_image_size_ctl[index]);
    return;
};
void get_label_streo_view_ctl(int index, char *label){
    if(index < NLBL_STEREO_VIEW_CTL) strngcopy(label, label_streo_view_ctl[index]);
    return;
};
void get_label_projection_mat_ctl(int index, char *label){
    if(index < NLBL_PROJECTION_MAT_CTL) strngcopy(label, label_projection_mat_ctl[index]);
    return;
};
void get_label_modeview_ctl(int index, char *label){
    if(index < NLBL_MODELVIEW_CTL) strngcopy(label, label_modeview_ctl[index]);
    return;
};


void alloc_image_size_ctl_c(struct image_size_ctl_c *img_size_c){
	int i;
	
	img_size_c->maxlen = 0;
	for (i=0;i<NLBL_IMAGE_SIZE_CTL;i++){
		if(strlen(label_image_size_ctl[i]) > img_size_c->maxlen){
			img_size_c->maxlen = (int) strlen(label_image_size_ctl[i]);
		};
	};
	
	img_size_c->num_xpixel_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	img_size_c->num_ypixel_ctl = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_int_ctl_item_c(img_size_c->num_xpixel_ctl);
	init_int_ctl_item_c(img_size_c->num_ypixel_ctl);
	
	return;
};

void dealloc_image_size_ctl_c(struct image_size_ctl_c *img_size_c){
	
	free(img_size_c->num_xpixel_ctl);
	free(img_size_c->num_ypixel_ctl);
	
	return;
};

int read_image_size_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct image_size_ctl_c *img_size_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		read_integer_ctl_item_c(buf, label_image_size_ctl[ 0], img_size_c->num_xpixel_ctl);
		read_integer_ctl_item_c(buf, label_image_size_ctl[ 1], img_size_c->num_ypixel_ctl);
	};
	return 1;
};

int write_image_size_ctl_c(FILE *fp, int level, const char *label, 
			struct image_size_ctl_c *img_size_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_integer_ctl_item_c(fp, level, img_size_c->maxlen,
				label_image_size_ctl[ 0], img_size_c->num_xpixel_ctl);
	write_integer_ctl_item_c(fp, level, img_size_c->maxlen,
				label_image_size_ctl[ 1], img_size_c->num_ypixel_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_streo_view_ctl_c(struct streo_view_ctl_c *streo_view_c){
	int i;
	
	streo_view_c->maxlen = 0;
	for (i=0;i<NLBL_STEREO_VIEW_CTL;i++){
		if(strlen(label_streo_view_ctl[i]) > streo_view_c->maxlen){
			streo_view_c->maxlen = (int) strlen(label_streo_view_ctl[i]);
		};
	};
	
	streo_view_c->focalpoint_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	streo_view_c->eye_separation_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(streo_view_c->focalpoint_ctl);
	init_real_ctl_item_c(streo_view_c->eye_separation_ctl);
	
	return;
};

void dealloc_streo_view_ctl_c(struct streo_view_ctl_c *streo_view_c){
	
	free(streo_view_c->focalpoint_ctl);
	free(streo_view_c->eye_separation_ctl);
	
	return;
};

int read_streo_view_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct streo_view_ctl_c *streo_view_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real_ctl_item_c(buf, label_streo_view_ctl[ 0], streo_view_c->focalpoint_ctl);
		read_real_ctl_item_c(buf, label_streo_view_ctl[ 1], streo_view_c->eye_separation_ctl);
	};
	return 1;
};

int write_streo_view_ctl_c(FILE *fp, int level, const char *label, 
			struct streo_view_ctl_c *streo_view_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, streo_view_c->maxlen, 
				label_streo_view_ctl[ 0], streo_view_c->focalpoint_ctl);
	write_real_ctl_item_c(fp, level, streo_view_c->maxlen, 
				label_streo_view_ctl[ 1], streo_view_c->eye_separation_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_projection_mat_ctl_c(struct projection_mat_ctl_c *projection_c){
	int i;
	
	projection_c->maxlen = 0;
	for (i=0;i<NLBL_PROJECTION_MAT_CTL;i++){
		if(strlen(label_projection_mat_ctl[i]) > projection_c->maxlen){
			projection_c->maxlen = (int) strlen(label_projection_mat_ctl[i]);
		};
	};
	
	projection_c->perspective_angle_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	projection_c->perspective_xy_ratio_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	projection_c->perspective_near_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	projection_c->perspective_far_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(projection_c->perspective_angle_ctl);
	init_real_ctl_item_c(projection_c->perspective_xy_ratio_ctl);
	init_real_ctl_item_c(projection_c->perspective_near_ctl);
	init_real_ctl_item_c(projection_c->perspective_far_ctl);
	
	return;
};

void dealloc_projection_mat_ctl_c(struct projection_mat_ctl_c *projection_c){
	
	free(projection_c->perspective_angle_ctl);
	free(projection_c->perspective_xy_ratio_ctl);
	free(projection_c->perspective_near_ctl);
	free(projection_c->perspective_far_ctl);
	
	return;
};

int read_projection_mat_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct projection_mat_ctl_c *projection_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 0], projection_c->perspective_angle_ctl);
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 1], projection_c->perspective_xy_ratio_ctl);
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 2], projection_c->perspective_near_ctl);
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 3], projection_c->perspective_far_ctl);
	};
	return 1;
};

int write_projection_mat_ctl_c(FILE *fp, int level, const char *label, 
			struct projection_mat_ctl_c *projection_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, projection_c->maxlen, 
				label_projection_mat_ctl[ 0], projection_c->perspective_angle_ctl);
	write_real_ctl_item_c(fp, level, projection_c->maxlen, 
				label_projection_mat_ctl[ 1], projection_c->perspective_xy_ratio_ctl);
	write_real_ctl_item_c(fp, level, projection_c->maxlen, 
				label_projection_mat_ctl[ 2], projection_c->perspective_near_ctl);
	write_real_ctl_item_c(fp, level, projection_c->maxlen, 
				label_projection_mat_ctl[ 3], projection_c->perspective_far_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_modeview_ctl_c(struct modeview_ctl_c *mat_c){
	int i;
	
	mat_c->maxlen = 0;
	for (i=0;i<NLBL_MODELVIEW_CTL;i++){
		if(strlen(label_modeview_ctl[i]) > mat_c->maxlen){
			mat_c->maxlen = (int) strlen(label_modeview_ctl[i]);
		};
	};
	
    mat_c->modelview_mat_ctl = (struct chara2_real_clist *) malloc(sizeof(struct chara2_real_clist));
	init_c2r_clist(mat_c->modelview_mat_ctl);
	
	init_chara_real_ctl_list(&mat_c->lookpoint_list);
	init_chara_real_ctl_list(&mat_c->viewpoint_list);
	init_chara_real_ctl_list(&mat_c->up_dir_list);
	init_chara_real_ctl_list(&mat_c->view_rot_vec_list);
	
	mat_c->view_rotation_deg_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	mat_c->scale_factor_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(mat_c->view_rotation_deg_ctl);
	init_real_ctl_item_c(mat_c->scale_factor_ctl);
	
	init_chara_real_ctl_list(&mat_c->scale_vector_list);
	init_chara_real_ctl_list(&mat_c->viewpt_in_viewer_list);
	
	mat_c->iflag_image_size_ctl = 0;
	mat_c->img_size_c = (struct image_size_ctl_c *) malloc(sizeof(struct image_size_ctl_c));
	alloc_image_size_ctl_c(mat_c->img_size_c);
	
	mat_c->iflag_streo_view_ctl = 0;
	mat_c->streo_view_c = (struct streo_view_ctl_c *) malloc(sizeof(struct streo_view_ctl_c));
	alloc_streo_view_ctl_c(mat_c->streo_view_c);
	
	mat_c->iflag_projection_mat_ctl = 0;
	mat_c->projection_c = (struct projection_mat_ctl_c *) malloc(sizeof(struct projection_mat_ctl_c));
	alloc_projection_mat_ctl_c(mat_c->projection_c);
	
	return;
};

void dealloc_modeview_ctl_c(struct modeview_ctl_c *mat_c){
	
	clear_c2r_clist(mat_c->modelview_mat_ctl);
    free(mat_c->modelview_mat_ctl);
	
	clear_chara_real_ctl_list(&mat_c->lookpoint_list);
	clear_chara_real_ctl_list(&mat_c->viewpoint_list);
	clear_chara_real_ctl_list(&mat_c->up_dir_list);
	clear_chara_real_ctl_list(&mat_c->view_rot_vec_list);
	
	free(mat_c->view_rotation_deg_ctl);
	free(mat_c->scale_factor_ctl);
	
	clear_chara_real_ctl_list(&mat_c->scale_vector_list);
	clear_chara_real_ctl_list(&mat_c->viewpt_in_viewer_list);
	
	dealloc_image_size_ctl_c(mat_c->img_size_c);
	dealloc_streo_view_ctl_c(mat_c->streo_view_c);
	dealloc_projection_mat_ctl_c(mat_c->projection_c);
	free(mat_c->img_size_c);
	free(mat_c->streo_view_c);
	free(mat_c->projection_c);
	
	mat_c->iflag_image_size_ctl = 0;
	mat_c->iflag_streo_view_ctl = 0;
	mat_c->iflag_projection_mat_ctl = 0;
	
	return;
};

int read_modeview_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct modeview_ctl_c *mat_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_ctl_list(fp, buf, label_modeview_ctl[ 0], &mat_c->lookpoint_list);
		read_chara_real_ctl_list(fp, buf, label_modeview_ctl[ 1], &mat_c->viewpoint_list);
		read_chara_real_ctl_list(fp, buf, label_modeview_ctl[ 2], &mat_c->up_dir_list);
		read_chara_real_ctl_list(fp, buf, label_modeview_ctl[ 3], &mat_c->view_rot_vec_list);
		
		read_real_ctl_item_c(buf, label_modeview_ctl[ 4], mat_c->view_rotation_deg_ctl);
		read_real_ctl_item_c(buf, label_modeview_ctl[ 5], mat_c->scale_factor_ctl);
		
		read_chara_real_ctl_list(fp, buf, label_modeview_ctl[ 6], &mat_c->scale_vector_list);
		read_chara_real_ctl_list(fp, buf, label_modeview_ctl[ 7], &mat_c->viewpt_in_viewer_list);
		
		read_c2r_clist(fp, buf, label_modeview_ctl[ 8], mat_c->modelview_mat_ctl);
		
		if(right_begin_flag_c(buf, label_modeview_ctl[ 9]) > 0){
			mat_c->iflag_image_size_ctl = read_image_size_ctl_c(fp, buf, 
						label_modeview_ctl[ 9], mat_c->img_size_c);
		};
        if(right_begin_flag_c(buf, label_modeview_ctl[10]) > 0){
            mat_c->iflag_projection_mat_ctl = read_projection_mat_ctl_c(fp, buf, 
						label_modeview_ctl[10], mat_c->projection_c);
        };
		if(right_begin_flag_c(buf, label_modeview_ctl[11]) > 0){
			mat_c->iflag_streo_view_ctl = read_streo_view_ctl_c(fp, buf, 
						label_modeview_ctl[11], mat_c->streo_view_c);
		};
	};
	return 1;
};

int write_modeview_ctl_c(FILE *fp, int level, const char *label, 
			struct modeview_ctl_c *mat_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	if(mat_c->iflag_image_size_ctl > 0){
		level = write_image_size_ctl_c(fp, level, label_modeview_ctl[ 9], mat_c->img_size_c);
	};
	
	write_chara_real_ctl_list(fp, level, label_modeview_ctl[ 7], &mat_c->viewpt_in_viewer_list);
	
	write_real_ctl_item_c(fp, level, mat_c->maxlen, 
				label_modeview_ctl[ 5], mat_c->scale_factor_ctl);
	
	write_chara_real_ctl_list(fp, level, label_modeview_ctl[ 0], &mat_c->lookpoint_list);
	write_chara_real_ctl_list(fp, level, label_modeview_ctl[ 1], &mat_c->viewpoint_list);
	write_chara_real_ctl_list(fp, level, label_modeview_ctl[ 2], &mat_c->up_dir_list);
    write_chara_real_ctl_list(fp, level, label_modeview_ctl[ 3], &mat_c->view_rot_vec_list);
	
	write_real_ctl_item_c(fp, level, mat_c->maxlen, 
				label_modeview_ctl[ 4], mat_c->view_rotation_deg_ctl);
	
	write_chara_real_ctl_list(fp, level, label_modeview_ctl[ 6], &mat_c->scale_vector_list);
	
	write_c2r_clist(fp, level, label_modeview_ctl[ 8], mat_c->modelview_mat_ctl);
	
	if(mat_c->iflag_projection_mat_ctl > 0){
		fprintf(fp, "!\n");
		level = write_projection_mat_ctl_c(fp, level, label_modeview_ctl[10], mat_c->projection_c);
	};
	if(mat_c->iflag_streo_view_ctl > 0){
		fprintf(fp, "!\n");
		level = write_streo_view_ctl_c(fp, level, label_modeview_ctl[11], mat_c->streo_view_c);
	};
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


int read_modeview_file_c(const char *file_name, char buf[LENGTHBUF],
			struct modeview_ctl_c *mat_c){
	int iflag = 0;
	
    printf("Read PVR modelview file name: %s\n", file_name);
	if ((FP_View = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	skip_comment_read_line(FP_View, buf);
	if(right_begin_flag_c(buf, label_modeview_head) > 0){
		iflag = read_modeview_ctl_c(FP_View, buf, label_modeview_head, mat_c);
	};
	fclose(FP_View);
	
	return iflag;
};

int write_modeview_file_c(const char *file_name, struct modeview_ctl_c *mat_c){
	int level;
	
    printf("Write PVR modelview file name: %s\n", file_name);
	if ((FP_View = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	level = write_modeview_ctl_c(FP_View, 0, label_modeview_head, mat_c);
	fclose(FP_View);
	
	return level;
};
