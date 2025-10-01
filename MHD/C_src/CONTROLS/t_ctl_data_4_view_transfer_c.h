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
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_real2_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_control_chara2_real_IO.h"

struct image_size_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    
	struct int_ctl_item *f_num_xpixel_ctl;
	struct int_ctl_item *f_num_ypixel_ctl;
};

struct streo_view_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    
    struct real_ctl_item *f_focalpoint_ctl;
    struct real_ctl_item *f_eye_separation_ctl;
    struct real_ctl_item *f_eye_sep_angle_ctl;
    struct chara_ctl_item *f_step_eye_sep_angle_ctl;
};

struct projection_mat_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    
    struct real_ctl_item *f_perspective_angle_ctl;
    struct real_ctl_item *f_perspective_xy_ratio_ctl;
    struct real_ctl_item *f_perspective_near_ctl;
    struct real_ctl_item *f_perspective_far_ctl;

    struct real2_ctl_item *f_horizontal_range_ctl;
    struct real2_ctl_item *f_vertical_range_ctl;
};

struct modelview_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *mat_ctl_file_name;

	struct chara2_real_clist *f_modelview_mat_ctl;
	
	struct chara_real_clist *f_lookpoint_ctl;
	struct chara_real_clist *f_viewpoint_ctl;
	struct chara_real_clist *f_up_dir_ctl;
	struct chara_real_clist *f_view_rot_vec_ctl;
	
    struct real_ctl_item *f_view_rotation_deg_ctl;
    struct real_ctl_item *f_scale_factor_ctl;
    struct chara_ctl_item *f_projection_type_ctl;
	
	struct chara_real_clist *f_scale_vector_ctl;
	struct chara_real_clist *f_viewpt_in_viewer_ctl;
	
	struct image_size_ctl_c *f_pixel;
	struct streo_view_ctl_c *f_streo;
	struct projection_mat_ctl_c *f_proj;
 
    void *void_panel;
};

/* prototypes */
struct image_size_ctl_c * init_image_size_ctl_c(void);
void dealloc_image_size_ctl_c(struct image_size_ctl_c *f_pixel);
void read_image_size_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct image_size_ctl_c *f_pixel);
int write_image_size_ctl_c(FILE *fp, int level, const char *label, 
			struct image_size_ctl_c *f_pixel);

struct streo_view_ctl_c * init_streo_view_ctl_c(void);
void dealloc_streo_view_ctl_c(struct streo_view_ctl_c *f_streo);
void read_streo_view_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct streo_view_ctl_c *f_streo);
int write_streo_view_ctl_c(FILE *fp, int level, const char *label, 
			struct streo_view_ctl_c *f_streo);

struct projection_mat_ctl_c * init_projection_mat_ctl_c(void);
void dealloc_projection_mat_ctl_c(struct projection_mat_ctl_c *f_proj);
void read_projection_mat_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct projection_mat_ctl_c *f_proj);
int write_projection_mat_ctl_c(FILE *fp, int level, const char *label, 
			struct projection_mat_ctl_c *f_proj);

struct modelview_ctl_c * init_modelview_ctl_c(void);
void dealloc_modelview_ctl_c(struct modelview_ctl_c *mat_c);
void read_modelview_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct modelview_ctl_c *mat_c);
int write_modelview_ctl_c(FILE *fp, int level, const char *label, 
			struct modelview_ctl_c *mat_c);

void read_modelview_file_c(const char *file_name, char buf[LENGTHBUF],
			struct modelview_ctl_c *mat_c);
void write_modelview_file_c(const char *file_name, struct modelview_ctl_c *mat_c);

#endif /* t_ctl_data_4_view_transfer_c_h_ */
