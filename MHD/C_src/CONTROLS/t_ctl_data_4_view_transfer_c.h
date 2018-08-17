/*
//  t_ctl_data_4_view_transfer_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_ctl_data_4_view_transfer_c_h_
#define t_ctl_data_4_view_transfer_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_control_chara2_real_IO.h"

struct image_size_ctl_c{
	int maxlen;
	
	struct int_ctl_item *num_xpixel_ctl;
	struct int_ctl_item *num_ypixel_ctl;
};

struct streo_view_ctl_c{
	int maxlen;
	
    struct real_ctl_item *focalpoint_ctl;
    struct real_ctl_item *eye_separation_ctl;
};

struct projection_mat_ctl_c{
	int maxlen;
	
    struct real_ctl_item *perspective_angle_ctl;
    struct real_ctl_item *perspective_xy_ratio_ctl;
    struct real_ctl_item *perspective_near_ctl;
    struct real_ctl_item *perspective_far_ctl;
};

struct modeview_ctl_c{
	int maxlen;
	
	struct chara2_real_clist *modelview_mat_ctl;
	
	struct chara_real_clist *lookpoint_list;
	struct chara_real_clist *viewpoint_list;
	struct chara_real_clist *up_dir_list;
	struct chara_real_clist *view_rot_vec_list;
	
    struct real_ctl_item *view_rotation_deg_ctl;
    struct real_ctl_item *scale_factor_ctl;
	
	struct chara_real_clist *scale_vector_list;
	struct chara_real_clist *viewpt_in_viewer_list;
	
	int iflag_image_size_ctl;
	struct image_size_ctl_c *img_size_c;
	int iflag_streo_view_ctl;
	struct streo_view_ctl_c *streo_view_c;
	int iflag_projection_mat_ctl;
	struct projection_mat_ctl_c *projection_c;
};

/* prototypes */
void get_label_image_size_ctl(int index, char *label);
void get_label_streo_view_ctl(int index, char *label);
void get_label_projection_mat_ctl(int index, char *label);
void get_label_modeview_ctl(int index, char *label);

void alloc_image_size_ctl_c(struct image_size_ctl_c *img_size_c);
void dealloc_image_size_ctl_c(struct image_size_ctl_c *img_size_c);
int read_image_size_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct image_size_ctl_c *img_size_c);
int write_image_size_ctl_c(FILE *fp, int level, const char *label, 
			struct image_size_ctl_c *img_size_c);

void alloc_streo_view_ctl_c(struct streo_view_ctl_c *streo_view_c);
void dealloc_streo_view_ctl_c(struct streo_view_ctl_c *streo_view_c);
int read_streo_view_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct streo_view_ctl_c *streo_view_c);
int write_streo_view_ctl_c(FILE *fp, int level, const char *label, 
			struct streo_view_ctl_c *streo_view_c);

void alloc_projection_mat_ctl_c(struct projection_mat_ctl_c *projection_c);
void dealloc_projection_mat_ctl_c(struct projection_mat_ctl_c *projection_c);
int read_projection_mat_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct projection_mat_ctl_c *projection_c);
int write_projection_mat_ctl_c(FILE *fp, int level, const char *label, 
			struct projection_mat_ctl_c *projection_c);

void alloc_modeview_ctl_c(struct modeview_ctl_c *mat_c);
void dealloc_modeview_ctl_c(struct modeview_ctl_c *mat_c);
int read_modeview_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct modeview_ctl_c *mat_c);
int write_modeview_ctl_c(FILE *fp, int level, const char *label, 
			struct modeview_ctl_c *mat_c);

int read_modeview_file_c(const char *file_name, char buf[LENGTHBUF],
			struct modeview_ctl_c *mat_c);
int write_modeview_file_c(const char *file_name, struct modeview_ctl_c *mat_c);

#endif /* t_ctl_data_4_view_transfer_c_h_ */
