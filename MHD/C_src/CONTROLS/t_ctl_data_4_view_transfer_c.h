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
#include "t_control_chara_real_IO.h"
#include "t_control_chara2_real_IO.h"

struct image_size_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct int_ctl_item *num_xpixel_ctl;
	struct int_ctl_item *num_ypixel_ctl;
};

struct streo_view_ctl_c{
    int iflag_use;
	int maxlen;
	
    struct real_ctl_item *focalpoint_ctl;
    struct real_ctl_item *eye_separation_ctl;
};

struct projection_mat_ctl_c{
    int iflag_use;
	int maxlen;
	
    struct real_ctl_item *perspective_angle_ctl;
    struct real_ctl_item *perspective_xy_ratio_ctl;
    struct real_ctl_item *perspective_near_ctl;
    struct real_ctl_item *perspective_far_ctl;
};

struct modelview_ctl_c{
    int iflag_use;
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
	
	struct image_size_ctl_c *img_size_c;
	struct streo_view_ctl_c *streo_view_c;
	struct projection_mat_ctl_c *projection_c;
};

/* prototypes */
struct image_size_ctl_c * init_image_size_ctl_c(void);
void dealloc_image_size_ctl_c(struct image_size_ctl_c *img_size_c);
void read_image_size_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct image_size_ctl_c *img_size_c);
int write_image_size_ctl_c(FILE *fp, int level, const char *label, 
			struct image_size_ctl_c *img_size_c);

struct streo_view_ctl_c * init_streo_view_ctl_c(void);
void dealloc_streo_view_ctl_c(struct streo_view_ctl_c *streo_view_c);
void read_streo_view_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct streo_view_ctl_c *streo_view_c);
int write_streo_view_ctl_c(FILE *fp, int level, const char *label, 
			struct streo_view_ctl_c *streo_view_c);

struct projection_mat_ctl_c * init_projection_mat_ctl_c(void);
void dealloc_projection_mat_ctl_c(struct projection_mat_ctl_c *projection_c);
void read_projection_mat_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct projection_mat_ctl_c *projection_c);
int write_projection_mat_ctl_c(FILE *fp, int level, const char *label, 
			struct projection_mat_ctl_c *projection_c);

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
