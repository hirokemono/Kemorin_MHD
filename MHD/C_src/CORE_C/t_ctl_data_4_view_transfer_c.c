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
	init_ctl_int_item(img_size_c->num_xpixel_ctl);
	init_ctl_int_item(img_size_c->num_ypixel_ctl);
	
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
		fgets(buf, LENGTHBUF, fp);
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
	init_ctl_real_item(streo_view_c->focalpoint_ctl);
	init_ctl_real_item(streo_view_c->eye_separation_ctl);
	
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
		fgets(buf, LENGTHBUF, fp);
		
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
	init_ctl_real_item(projection_c->perspective_angle_ctl);
	init_ctl_real_item(projection_c->perspective_xy_ratio_ctl);
	init_ctl_real_item(projection_c->perspective_near_ctl);
	init_ctl_real_item(projection_c->perspective_far_ctl);
	
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
		fgets(buf, LENGTHBUF, fp);
		
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
	
	mat_c->modelview_mat_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	init_ctl_c2r_array(mat_c->modelview_mat_ctl);
	
	mat_c->lookpoint_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	mat_c->viewpoint_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	mat_c->up_dir_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	mat_c->view_rot_vec_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	init_ctl_cr_array(mat_c->lookpoint_ctl);
	init_ctl_cr_array(mat_c->viewpoint_ctl);
	init_ctl_cr_array(mat_c->up_dir_ctl);
	init_ctl_cr_array(mat_c->view_rot_vec_ctl);
	
	mat_c->view_rotation_deg_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	mat_c->scale_factor_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_ctl_real_item(mat_c->view_rotation_deg_ctl);
	init_ctl_real_item(mat_c->scale_factor_ctl);
	
	mat_c->scale_vector_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	mat_c->viewpt_in_viewer_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	init_ctl_cr_array(mat_c->scale_vector_ctl);
	init_ctl_cr_array(mat_c->viewpt_in_viewer_ctl);
	
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
	
	dealloc_ctl_c2r_array(mat_c->modelview_mat_ctl);
	free(mat_c->modelview_mat_ctl);
	
	dealloc_ctl_cr_array(mat_c->lookpoint_ctl);
	dealloc_ctl_cr_array(mat_c->viewpoint_ctl);
	dealloc_ctl_cr_array(mat_c->up_dir_ctl);
	dealloc_ctl_cr_array(mat_c->view_rot_vec_ctl);
	free(mat_c->lookpoint_ctl);
	free(mat_c->viewpoint_ctl);
	free(mat_c->up_dir_ctl);
	free(mat_c->view_rot_vec_ctl);
	
	free(mat_c->view_rotation_deg_ctl);
	free(mat_c->scale_factor_ctl);
	
	dealloc_ctl_cr_array(mat_c->scale_vector_ctl);
	dealloc_ctl_cr_array(mat_c->viewpt_in_viewer_ctl);
	free(mat_c->scale_vector_ctl);
	free(mat_c->viewpt_in_viewer_ctl);
	
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
		fgets(buf, LENGTHBUF, fp);
		
		read_cr_ctl_array_c(fp, buf, label_modeview_ctl[ 0], mat_c->lookpoint_ctl);
		read_cr_ctl_array_c(fp, buf, label_modeview_ctl[ 1], mat_c->viewpoint_ctl);
		read_cr_ctl_array_c(fp, buf, label_modeview_ctl[ 2], mat_c->up_dir_ctl);
		read_cr_ctl_array_c(fp, buf, label_modeview_ctl[ 3], mat_c->view_rot_vec_ctl);
		
		read_real_ctl_item_c(buf, label_modeview_ctl[ 4], mat_c->view_rotation_deg_ctl);
		read_real_ctl_item_c(buf, label_modeview_ctl[ 5], mat_c->scale_factor_ctl);
		
		read_cr_ctl_array_c(fp, buf, label_modeview_ctl[ 6], mat_c->scale_vector_ctl);
		read_cr_ctl_array_c(fp, buf, label_modeview_ctl[ 7], mat_c->viewpt_in_viewer_ctl);
		
		read_c2r_ctl_array_c(fp, buf, label_modeview_ctl[ 8],mat_c->modelview_mat_ctl);
		
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
	printf("mat_c->viewpoint_ctl->num read %d\n", mat_c->viewpoint_ctl->num);
	return 1;
};

int write_modeview_ctl_c(FILE *fp, int level, const char *label, 
			struct modeview_ctl_c *mat_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	if(mat_c->iflag_image_size_ctl > 0){
		level = write_image_size_ctl_c(fp, level, label_modeview_ctl[ 9], mat_c->img_size_c);
	};
	
	if(mat_c->viewpt_in_viewer_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, mat_c->maxlen, label_modeview_ctl[ 7], mat_c->viewpt_in_viewer_ctl);
	
	write_real_ctl_item_c(fp, level, mat_c->maxlen, 
				label_modeview_ctl[ 5], mat_c->scale_factor_ctl);
	
	if(mat_c->lookpoint_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, mat_c->maxlen, label_modeview_ctl[ 0], mat_c->lookpoint_ctl);
	
	if(mat_c->viewpoint_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, mat_c->maxlen, label_modeview_ctl[ 1], mat_c->viewpoint_ctl);
	
	if(mat_c->up_dir_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, mat_c->maxlen, label_modeview_ctl[ 2], mat_c->up_dir_ctl);
	
	if(mat_c->view_rot_vec_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, mat_c->maxlen, label_modeview_ctl[ 3], mat_c->view_rot_vec_ctl);
	
	write_real_ctl_item_c(fp, level, mat_c->maxlen, 
				label_modeview_ctl[ 4], mat_c->view_rotation_deg_ctl);
	
	if(mat_c->scale_vector_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, mat_c->maxlen, label_modeview_ctl[ 6], mat_c->scale_vector_ctl);
	
	if(mat_c->modelview_mat_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, mat_c->maxlen, label_modeview_ctl[ 8], mat_c->modelview_mat_ctl);
	
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
	
	if ((FP_View = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	printf("alloc_modeview_ctl_c \n");
	
	fgets(buf, LENGTHBUF, FP_View);
	if(right_begin_flag_c(buf, label_modeview_head) > 0){
		printf("read_modeview_ctl_c \n");
		iflag = read_modeview_ctl_c(FP_View, buf, label_modeview_head, mat_c);
	};
	fclose(FP_View);
	
	return iflag;
};

int write_modeview_file_c(const char *file_name, struct modeview_ctl_c *mat_c){
	int level;
	
	if ((FP_View = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	level = write_modeview_ctl_c(FP_View, 0, label_modeview_head, mat_c);
	fclose(FP_View);
	
	return level;
};
