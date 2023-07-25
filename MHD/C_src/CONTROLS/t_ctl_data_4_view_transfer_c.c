/*
//  t_ctl_data_4_view_transfer_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_ctl_data_4_view_transfer_c.h"

#define NLBL_IMAGE_SIZE_CTL       2
#define NLBL_STEREO_VIEW_CTL      2
#define NLBL_PROJECTION_MAT_CTL   6
#define NLBL_MODELVIEW_CTL       13

FILE *FP_View;

const char label_image_size_ctl[NLBL_IMAGE_SIZE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"x_pixel_ctl"},
	/*[ 1]*/	{"y_pixel_ctl"}
};

const char label_streo_view_ctl[NLBL_STEREO_VIEW_CTL][KCHARA_C] = {
	/*[ 0]*/	{"focal_distance_ctl"},
	/*[ 1]*/	{"eye_separation_ctl"}
};

const char label_projection_mat_ctl[NLBL_PROJECTION_MAT_CTL][KCHARA_C] = {
	/*[ 0]*/	{"perspective_angle_ctl"},
	/*[ 1]*/	{"perspective_xy_ratio_ctl"},
	/*[ 2]*/	{"perspective_near_ctl"},
	/*[ 3]*/	{"perspective_far_ctl"},
    /*[ 4]*/    {"horizontal_range_ctl"},
    /*[ 5]*/    {"vertical_range_ctl"}
};

const char label_modeview_ctl[NLBL_MODELVIEW_CTL][KCHARA_C] = {
	/*[ 0]*/	{"image_size_ctl"},
	
	/*[ 1]*/	{"look_at_point_ctl"},
	/*[ 2]*/	{"eye_position_ctl"},
	/*[ 3]*/	{"up_direction_ctl"},
	/*[ 4]*/	{"view_rotation_vec_ctl"},
	/*[ 5]*/	{"view_rotation_deg_ctl"},
	/*[ 6]*/	{"scale_factor_ctl"},
	/*[ 7]*/	{"scale_factor_vec_ctl"},
	/*[ 8]*/	{"eye_position_in_viewer_ctl"},
	
	/*[ 9]*/	{"projection_matrix_ctl"},
	/*[10]*/	{"modelview_matrix_ctl"},
	/*[11]*/	{"stereo_view_parameter_ctl"},
    /*[12]*/    {"projection_type_ctl"}
};

const char label_modeview_head[KCHARA_C] = "view_transform_ctl";

struct image_size_ctl_c * init_image_size_ctl_c(void){
    struct image_size_ctl_c *f_pixel;
    if((f_pixel = (struct image_size_ctl_c *) malloc(sizeof(struct image_size_ctl_c))) == NULL) {
        printf("malloc error for image_size_ctl_c \n");
        exit(0);
    }
    if((f_pixel->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for f_pixel->f_iflag\n");
        exit(0);
    }
    f_pixel->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	
    f_pixel->f_num_xpixel_ctl = init_int_ctl_item_c();
    f_pixel->f_num_ypixel_ctl = init_int_ctl_item_c();
	
	return f_pixel;
};

void dealloc_image_size_ctl_c(struct image_size_ctl_c *f_pixel){
    dealloc_int_ctl_item_c(f_pixel->f_num_xpixel_ctl);
    dealloc_int_ctl_item_c(f_pixel->f_num_ypixel_ctl);
	
    free(f_pixel->c_block_name);
    f_pixel->f_iflag = NULL;
    free(f_pixel);
	return;
};

void read_image_size_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct image_size_ctl_c *f_pixel){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		read_integer_ctl_item_c(buf, label_image_size_ctl[ 0], f_pixel->f_num_xpixel_ctl);
		read_integer_ctl_item_c(buf, label_image_size_ctl[ 1], f_pixel->f_num_ypixel_ctl);
	};
    f_pixel->f_iflag[0] = 1;
    return;
};

int write_image_size_ctl_c(FILE *fp, int level, const char *label, 
			struct image_size_ctl_c *f_pixel){
    if(f_pixel->f_iflag[0] == 0) return level;
    
    int maxlen;
    int i;
    for (i=0;i<NLBL_IMAGE_SIZE_CTL;i++){
        if(strlen(label_image_size_ctl[i]) > maxlen){
            maxlen = (int) strlen(label_image_size_ctl[i]);
        };
    };

    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_integer_ctl_item_c(fp, level, maxlen,
				label_image_size_ctl[ 0], f_pixel->f_num_xpixel_ctl);
	write_integer_ctl_item_c(fp, level, maxlen,
				label_image_size_ctl[ 1], f_pixel->f_num_ypixel_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct streo_view_ctl_c * init_streo_view_ctl_c(void){
    struct streo_view_ctl_c *f_streo;
    if((f_streo = (struct streo_view_ctl_c *) malloc(sizeof(struct streo_view_ctl_c))) == NULL) {
        printf("malloc error for streo_view_ctl_c \n");
        exit(0);
    }
    if((f_streo->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for f_streo->f_iflag\n");
        exit(0);
    }
    f_streo->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
    
    f_streo->f_focalpoint_ctl =         init_real_ctl_item_c();
    f_streo->f_eye_separation_ctl =     init_real_ctl_item_c();
    f_streo->f_eye_sep_angle_ctl =      init_real_ctl_item_c();
    f_streo->f_step_eye_sep_angle_ctl = init_chara_ctl_item_c();
	return f_streo;
};

void dealloc_streo_view_ctl_c(struct streo_view_ctl_c *f_streo){
    dealloc_real_ctl_item_c(f_streo->f_focalpoint_ctl);
    dealloc_real_ctl_item_c(f_streo->f_eye_separation_ctl);
    dealloc_real_ctl_item_c(f_streo->f_eye_sep_angle_ctl);
    dealloc_chara_ctl_item_c(f_streo->f_step_eye_sep_angle_ctl);

    free(f_streo->c_block_name);
    f_streo->f_iflag = NULL;
	return;
};

void read_streo_view_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct streo_view_ctl_c *f_streo){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real_ctl_item_c(buf, label_streo_view_ctl[ 0], f_streo->f_focalpoint_ctl);
		read_real_ctl_item_c(buf, label_streo_view_ctl[ 1], f_streo->f_eye_separation_ctl);
	};
    f_streo->f_iflag[0] = 1;
    return;
};

int write_streo_view_ctl_c(FILE *fp, int level, const char *label, 
			struct streo_view_ctl_c *f_streo){
    if(f_streo->f_iflag[0] == 0) return level;
    
    int maxlen;
    int i;
    for (i=0;i<NLBL_STEREO_VIEW_CTL;i++){
        if(strlen(label_streo_view_ctl[i]) > maxlen){
            maxlen = (int) strlen(label_streo_view_ctl[i]);
        };
    };
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, maxlen,
				label_streo_view_ctl[ 0], f_streo->f_focalpoint_ctl);
	write_real_ctl_item_c(fp, level, maxlen,
				label_streo_view_ctl[ 1], f_streo->f_eye_separation_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct projection_mat_ctl_c * init_projection_mat_ctl_c(void){
    struct projection_mat_ctl_c *f_proj;
    if((f_proj = (struct projection_mat_ctl_c *) malloc(sizeof(struct projection_mat_ctl_c))) == NULL) {
        printf("malloc error for projection_mat_ctl_c \n");
        exit(0);
    }
    if((f_proj->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for f_proj->f_iflag\n");
        exit(0);
    }
    f_proj->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	
    f_proj->f_perspective_angle_ctl =    init_real_ctl_item_c();
    f_proj->f_perspective_xy_ratio_ctl = init_real_ctl_item_c();
    f_proj->f_perspective_near_ctl =     init_real_ctl_item_c();
    f_proj->f_perspective_far_ctl =      init_real_ctl_item_c();
    f_proj->f_horizontal_range_ctl =     init_real2_ctl_item_c();
    f_proj->f_vertical_range_ctl =       init_real2_ctl_item_c();
	return f_proj;
};

void dealloc_projection_mat_ctl_c(struct projection_mat_ctl_c *f_proj){
    dealloc_real_ctl_item_c(f_proj->f_perspective_angle_ctl);
    dealloc_real_ctl_item_c(f_proj->f_perspective_xy_ratio_ctl);
    dealloc_real_ctl_item_c(f_proj->f_perspective_near_ctl);
    dealloc_real_ctl_item_c(f_proj->f_perspective_far_ctl);
    
    dealloc_real2_ctl_item_c(f_proj->f_horizontal_range_ctl);
    dealloc_real2_ctl_item_c(f_proj->f_vertical_range_ctl);
	
    free(f_proj->c_block_name);
    f_proj->f_iflag = NULL;
    free(f_proj);
	return;
};

void read_projection_mat_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct projection_mat_ctl_c *f_proj){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 0], f_proj->f_perspective_angle_ctl);
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 1], f_proj->f_perspective_xy_ratio_ctl);
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 2], f_proj->f_perspective_near_ctl);
		read_real_ctl_item_c(buf, label_projection_mat_ctl[ 3], f_proj->f_perspective_far_ctl);
        read_real2_ctl_item_c(buf, label_projection_mat_ctl[ 4], f_proj->f_horizontal_range_ctl);
        read_real2_ctl_item_c(buf, label_projection_mat_ctl[ 5], f_proj->f_vertical_range_ctl);
	};
    f_proj->f_iflag[0] = 1;
    return;
};

int write_projection_mat_ctl_c(FILE *fp, int level, const char *label, 
			struct projection_mat_ctl_c *f_proj){
    if(f_proj->f_iflag[0] == 0) return level;
    
    int i;
    int maxlen = 0;
    for (i=0;i<NLBL_PROJECTION_MAT_CTL;i++){
        if(strlen(label_projection_mat_ctl[i]) > maxlen){
            maxlen = (int) strlen(label_projection_mat_ctl[i]);
        };
    };
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, maxlen,
				label_projection_mat_ctl[ 0], f_proj->f_perspective_angle_ctl);
	write_real_ctl_item_c(fp, level, maxlen,
				label_projection_mat_ctl[ 1], f_proj->f_perspective_xy_ratio_ctl);
	write_real_ctl_item_c(fp, level, maxlen,
				label_projection_mat_ctl[ 2], f_proj->f_perspective_near_ctl);
	write_real_ctl_item_c(fp, level, maxlen,
				label_projection_mat_ctl[ 3], f_proj->f_perspective_far_ctl);
    write_real2_ctl_item_c(fp, level, maxlen,
                label_projection_mat_ctl[ 4], f_proj->f_horizontal_range_ctl);
    write_real2_ctl_item_c(fp, level, maxlen,
                label_projection_mat_ctl[ 5], f_proj->f_vertical_range_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct modelview_ctl_c * init_modelview_ctl_c(void){
    struct modelview_ctl_c *mat_c;
    if((mat_c = (struct modelview_ctl_c *) malloc(sizeof(struct modelview_ctl_c))) == NULL) {
        printf("malloc error for modelview_ctl_c \n");
        exit(0);
    }
	
    if((mat_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for mat_c->f_iflag\n");
        exit(0);
    }
    mat_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
    mat_c->mat_ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));

    mat_c->f_modelview_mat_ctl = init_chara2_real_clist();
	
    mat_c->f_lookpoint_ctl =    init_chara_real_clist();
    mat_c->f_viewpoint_ctl =    init_chara_real_clist();
    mat_c->f_up_dir_ctl =       init_chara_real_clist();
    mat_c->f_view_rot_vec_ctl = init_chara_real_clist();

    sprintf(mat_c->f_lookpoint_ctl->c1_name, "Direction");
    sprintf(mat_c->f_viewpoint_ctl->c1_name, "Direction");
    sprintf(mat_c->f_up_dir_ctl->c1_name, "Direction");
    sprintf(mat_c->f_view_rot_vec_ctl->c1_name, "Direction");
    sprintf(mat_c->f_lookpoint_ctl->r1_name, "Value");
    sprintf(mat_c->f_viewpoint_ctl->r1_name, "Value");
    sprintf(mat_c->f_up_dir_ctl->r1_name, "Value");
    sprintf(mat_c->f_view_rot_vec_ctl->r1_name, "Value");
	
    mat_c->f_view_rotation_deg_ctl = init_real_ctl_item_c();
    mat_c->f_scale_factor_ctl =      init_real_ctl_item_c();

    mat_c->f_projection_type_ctl =      init_chara_ctl_item_c();
    
    mat_c->f_scale_vector_ctl =     init_chara_real_clist();
    mat_c->f_viewpt_in_viewer_ctl = init_chara_real_clist();

    sprintf(mat_c->f_scale_vector_ctl->c1_name, "Direction");
    sprintf(mat_c->f_viewpt_in_viewer_ctl->r1_name, "Value");
    sprintf(mat_c->f_scale_vector_ctl->c1_name, "Direction");
    sprintf(mat_c->f_viewpt_in_viewer_ctl->r1_name, "Value");
	
	mat_c->f_pixel = init_image_size_ctl_c();
	mat_c->f_streo = init_streo_view_ctl_c();
	mat_c->f_proj = init_projection_mat_ctl_c();
	
	return mat_c;
};

void dealloc_modelview_ctl_c(struct modelview_ctl_c *mat_c){
	
	dealloc_chara2_real_clist(mat_c->f_modelview_mat_ctl);
	
	dealloc_chara_real_clist(mat_c->f_lookpoint_ctl);
	dealloc_chara_real_clist(mat_c->f_viewpoint_ctl);
	dealloc_chara_real_clist(mat_c->f_up_dir_ctl);
	dealloc_chara_real_clist(mat_c->f_view_rot_vec_ctl);
	
    dealloc_real_ctl_item_c(mat_c->f_view_rotation_deg_ctl);
    dealloc_real_ctl_item_c(mat_c->f_scale_factor_ctl);
    dealloc_chara_ctl_item_c(mat_c->f_projection_type_ctl);
	
	dealloc_chara_real_clist(mat_c->f_scale_vector_ctl);
	dealloc_chara_real_clist(mat_c->f_viewpt_in_viewer_ctl);
	
	dealloc_image_size_ctl_c(mat_c->f_pixel);
	dealloc_streo_view_ctl_c(mat_c->f_streo);
	dealloc_projection_mat_ctl_c(mat_c->f_proj);

    free(mat_c->mat_ctl_file_name);
    free(mat_c->c_block_name);
    mat_c->f_iflag = NULL;
    free(mat_c);
    return;
};

void read_modelview_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct modelview_ctl_c *mat_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_clist(fp, buf, label_modeview_ctl[ 1], mat_c->f_lookpoint_ctl);
		read_chara_real_clist(fp, buf, label_modeview_ctl[ 2], mat_c->f_viewpoint_ctl);
		read_chara_real_clist(fp, buf, label_modeview_ctl[ 3], mat_c->f_up_dir_ctl);
		read_chara_real_clist(fp, buf, label_modeview_ctl[ 4], mat_c->f_view_rot_vec_ctl);
		
		read_real_ctl_item_c(buf, label_modeview_ctl[ 5], mat_c->f_view_rotation_deg_ctl);
		read_real_ctl_item_c(buf, label_modeview_ctl[ 6], mat_c->f_scale_factor_ctl);
        read_chara_ctl_item_c(buf, label_modeview_ctl[12], mat_c->f_projection_type_ctl);
		
		read_chara_real_clist(fp, buf, label_modeview_ctl[ 7],
							  mat_c->f_scale_vector_ctl);
		read_chara_real_clist(fp, buf, label_modeview_ctl[ 8],
							  mat_c->f_viewpt_in_viewer_ctl);
		
		read_chara2_real_clist(fp, buf, label_modeview_ctl[10],
							   mat_c->f_modelview_mat_ctl);
		
		if(right_begin_flag_c(buf, label_modeview_ctl[ 0]) > 0){
			read_image_size_ctl_c(fp, buf, label_modeview_ctl[ 0], mat_c->f_pixel);
		};
        if(right_begin_flag_c(buf, label_modeview_ctl[ 9]) > 0){
			read_projection_mat_ctl_c(fp, buf, label_modeview_ctl[ 9],
									  mat_c->f_proj);
        };
		if(right_begin_flag_c(buf, label_modeview_ctl[11]) > 0){
			read_streo_view_ctl_c(fp, buf, label_modeview_ctl[11],
								  mat_c->f_streo);
		};
	};
	mat_c->f_iflag[0] = 1;
    return;
};

int write_modelview_ctl_c(FILE *fp, int level, const char *label, 
			struct modelview_ctl_c *mat_c){
    if(mat_c->f_iflag[0] == 0) return level;
    
    int i;
    int maxlen = 0;
    for (i=0;i<NLBL_MODELVIEW_CTL;i++){
        if(strlen(label_modeview_ctl[i]) > maxlen){
            maxlen = (int) strlen(label_modeview_ctl[i]);
        };
    };

    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_image_size_ctl_c(fp, level, label_modeview_ctl[ 0],
								   mat_c->f_pixel);
	
	write_chara_real_clist(fp, level, label_modeview_ctl[ 8],
						   mat_c->f_viewpt_in_viewer_ctl);
	
	write_real_ctl_item_c(fp, level, maxlen,
				label_modeview_ctl[ 6], mat_c->f_scale_factor_ctl);
    write_chara_ctl_item_c(fp, level, maxlen,
                label_modeview_ctl[12], mat_c->f_projection_type_ctl);
	
	write_chara_real_clist(fp, level, label_modeview_ctl[ 1], mat_c->f_lookpoint_ctl);
	write_chara_real_clist(fp, level, label_modeview_ctl[ 2], mat_c->f_viewpoint_ctl);
	write_chara_real_clist(fp, level, label_modeview_ctl[ 3], mat_c->f_up_dir_ctl);
    write_chara_real_clist(fp, level, label_modeview_ctl[ 4], mat_c->f_view_rot_vec_ctl);
	
	write_real_ctl_item_c(fp, level, maxlen,
				label_modeview_ctl[ 5], mat_c->f_view_rotation_deg_ctl);
	
	write_chara_real_clist(fp, level, label_modeview_ctl[ 7],
						   mat_c->f_scale_vector_ctl);
	
	write_chara2_real_clist(fp, level, label_modeview_ctl[10],
							mat_c->f_modelview_mat_ctl);
	
	level = write_projection_mat_ctl_c(fp, level, label_modeview_ctl[ 9],
									   mat_c->f_proj);
	level = write_streo_view_ctl_c(fp, level, label_modeview_ctl[11],
								   mat_c->f_streo);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void read_modelview_file_c(const char *file_name, char buf[LENGTHBUF],
			struct modelview_ctl_c *mat_c){
	
    printf("Read PVR modelview file name: %s\n", file_name);
	if ((FP_View = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	skip_comment_read_line(FP_View, buf);
	if(right_begin_flag_c(buf, label_modeview_head) > 0){
		read_modelview_ctl_c(FP_View, buf, label_modeview_head, mat_c);
	};
	fclose(FP_View);
	
    mat_c->f_iflag[0] = -1;
	return;
};

void write_modelview_file_c(const char *file_name, struct modelview_ctl_c *mat_c){
	int level = 0;
	
    printf("Write PVR modelview file name: %s\n", file_name);
	if ((FP_View = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	level = write_modelview_ctl_c(FP_View, 0, label_modeview_head, mat_c);
	fclose(FP_View);
	
	return;
};
