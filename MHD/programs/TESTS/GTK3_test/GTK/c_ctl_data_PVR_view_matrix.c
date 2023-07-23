/*
//  c_ctl_data_PVR_view_matrix.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_PVR_view_matrix.h"

extern void * c_modeview_ctl_block_name(void *f_mat);
extern void * c_modeview_ctl_iflag(void *f_mat);
extern void * c_modeview_ctl_pixel(void *f_mat);
extern void * c_modeview_ctl_proj(void *f_mat);
extern void * c_modeview_ctl_streo(void *f_mat);
extern void * c_modeview_modelview_mat_ctl(void *f_mat);
extern void * c_modeview_lookpoint_ctl(void *f_mat);
extern void * c_modeview_viewpoint_ctl(void *f_mat);
extern void * c_modeview_up_dir_ctl(void *f_mat);
extern void * c_modeview_view_rot_vec_ctl(void *f_mat);
extern void * c_modeview_rotation_deg_ctl(void *f_mat);
extern void * c_modeview_scale_factor_ctl(void *f_mat);
extern void * c_modeview_scale_vector_ctl(void *f_mat);
extern void * c_modeview_viewpt_in_view_ctl(void *f_mat);
extern void * c_modeview_projection_type_ctl(void *f_mat);


extern void * c_screen_pixel_ctl_block_name(void *f_pixel);
extern void * c_screen_pixel_ctl_iflag(void *f_pixel);
extern void * c_screen_num_xpixel_ctl(void *f_pixel);
extern void * c_screen_num_ypixel_ctl(void *f_pixel);

extern void * c_projection_ctl_block_name(void *f_proj);
extern void * c_projection_ctl_iflag(void *f_proj);
extern void * c_projection_perspect_agl_ctl(void *f_proj);
extern void * c_projection_xy_ratio_ctl(void *f_proj);
extern void * c_projection_near_ctl(void *f_proj);
extern void * c_projection_far_ctl(void *f_proj);
extern void * c_projection_horiz_range_ctl(void *f_proj);
extern void * c_projection_vert_range_ctl(void *f_proj);

extern void * c_streo_view_ctl_block_name(void *f_streo);
extern void * c_streo_view_ctl_iflag(void *f_streo);
extern void * c_streo_view_focalpoint_ctl(void *f_streo);
extern void * c_streo_view_eye_separate_ctl(void *f_streo);
extern void * c_streo_view_eye_sep_angle_ctl(void *f_streo);
extern void * c_streo_view_step_eye_sep_ctl(void *f_streo);

extern void * c_VIZ_mul_mdlvw_ctl_block_name(void *f_mul_mmats_c);
extern int    c_VIZ_mul_mdlvw_num_mat_c(void *f_mul_mmats_c);
extern void * c_VIZ_mul_mdlvw_fname_ctl(int idx, void *f_mul_mmats_c);
extern void * c_VIZ_mul_mdlvw_matrices(int idx, void *f_mul_mmats_c);


static struct image_size_ctl_c * init_f_VIZ_pixels_ctl(void *(*c_load_self)(void *f_parent),
                                                void *f_parent){
	struct image_size_ctl_c *f_pixel
			= (struct image_size_ctl_c *) malloc(sizeof(struct image_size_ctl_c));
	if(f_pixel == NULL){
		printf("malloc error for image_size_ctl_c\n");
		exit(0);
    };
	f_pixel->f_self =  c_load_self(f_parent);
	f_pixel->f_iflag =   (int *) c_screen_pixel_ctl_iflag(f_pixel->f_self);
	char *f_block_name = (char *) c_screen_pixel_ctl_block_name(f_pixel->f_self);
	f_pixel->c_block_name = strngcopy_from_f(f_block_name);
    
    f_pixel->f_num_xpixel_ctl = init_f_ctl_int_item(c_screen_num_xpixel_ctl, f_pixel->f_self);
    f_pixel->f_num_ypixel_ctl = init_f_ctl_int_item(c_screen_num_ypixel_ctl, f_pixel->f_self);
    return f_pixel;
};

static struct projection_mat_ctl_c * init_f_VIZ_projection_ctl(void *(*c_load_self)(void *f_parent),
                                                void *f_parent){
	struct projection_mat_ctl_c *f_proj 
			= (struct projection_mat_ctl_c *) malloc(sizeof(struct projection_mat_ctl_c));
	if(f_proj == NULL){
		printf("malloc error for projection_mat_ctl_c\n");
		exit(0);
    };
	f_proj->f_self =  c_load_self(f_parent);
	f_proj->f_iflag =   (int *) c_projection_ctl_iflag(f_proj->f_self);
	char *f_block_name = (char *) c_projection_ctl_block_name(f_proj->f_self);
	f_proj->c_block_name = strngcopy_from_f(f_block_name);
    
    f_proj->f_perspective_angle_ctl =    init_f_ctl_real_item(c_projection_perspect_agl_ctl, f_proj->f_self);
    f_proj->f_perspective_xy_ratio_ctl = init_f_ctl_real_item(c_projection_xy_ratio_ctl, f_proj->f_self);
    f_proj->f_perspective_near_ctl =     init_f_ctl_real_item(c_projection_near_ctl, f_proj->f_self);
    f_proj->f_perspective_far_ctl =      init_f_ctl_real_item(c_projection_far_ctl, f_proj->f_self);
    
    f_proj->f_horizontal_range_ctl = init_f_ctl_r2_item(c_projection_horiz_range_ctl, f_proj->f_self);
    f_proj->f_vertical_range_ctl =   init_f_ctl_r2_item(c_projection_vert_range_ctl, f_proj->f_self);
    return f_proj;
};

static struct streo_view_ctl_c * init_f_VIZ_stereo_view_ctl(void *(*c_load_self)(void *f_parent),
                                                          void *f_parent){
	struct streo_view_ctl_c *f_streo 
			= (struct streo_view_ctl_c *) malloc(sizeof(struct streo_view_ctl_c));
	if(f_streo == NULL){
		printf("malloc error for streo_view_ctl_c\n");
		exit(0);
    };
	f_streo->f_self =  c_load_self(f_parent);
	f_streo->f_iflag =   (int *) c_streo_view_ctl_iflag(f_streo->f_self);
	char *f_block_name = (char *) c_streo_view_ctl_block_name(f_streo->f_self);
	f_streo->c_block_name = strngcopy_from_f(f_block_name);
    
    f_streo->f_focalpoint_ctl =         init_f_ctl_real_item(c_streo_view_focalpoint_ctl, f_streo->f_self);
    f_streo->f_eye_separation_ctl =     init_f_ctl_real_item(c_streo_view_eye_separate_ctl, f_streo->f_self);
    f_streo->f_eye_sep_angle_ctl =      init_f_ctl_real_item(c_streo_view_eye_sep_angle_ctl, f_streo->f_self);
    f_streo->f_step_eye_sep_angle_ctl = init_f_ctl_chara_item(c_streo_view_step_eye_sep_ctl, f_streo->f_self);
    return f_streo;
};


static void init_f_VIZ_view_matrix_ctl_items(char *ctl_file_name, struct modelview_ctl_c *f_mat){
    f_mat->f_iflag =   (int *) c_modeview_ctl_iflag(f_mat->f_self);
    char *f_block_name = (char *) c_modeview_ctl_block_name(f_mat->f_self);
    f_mat->c_block_name = strngcopy_from_f(f_block_name);
    
    f_mat->mat_ctl_file_name = ctl_file_name;
    
    f_mat->f_pixel = init_f_VIZ_pixels_ctl(c_modeview_ctl_pixel, f_mat->f_self);
    f_mat->f_proj = init_f_VIZ_projection_ctl(c_modeview_ctl_proj, f_mat->f_self);
    f_mat->f_streo = init_f_VIZ_stereo_view_ctl(c_modeview_ctl_streo, f_mat->f_self);
    
    f_mat->f_modelview_mat_ctl =     init_f_ctl_c2r_array(c_modeview_modelview_mat_ctl, f_mat->f_self);
    f_mat->f_lookpoint_ctl =         init_f_ctl_cr_array(c_modeview_lookpoint_ctl, f_mat->f_self);
    f_mat->f_viewpoint_ctl =         init_f_ctl_cr_array(c_modeview_viewpoint_ctl, f_mat->f_self);
    f_mat->f_up_dir_ctl =            init_f_ctl_cr_array(c_modeview_up_dir_ctl, f_mat->f_self);
    f_mat->f_view_rot_vec_ctl =      init_f_ctl_cr_array(c_modeview_view_rot_vec_ctl, f_mat->f_self);
    f_mat->f_view_rotation_deg_ctl = init_f_ctl_real_item(c_modeview_rotation_deg_ctl, f_mat->f_self);
    f_mat->f_scale_factor_ctl =      init_f_ctl_real_item(c_modeview_scale_factor_ctl, f_mat->f_self);
    f_mat->f_scale_vector_ctl =      init_f_ctl_cr_array(c_modeview_scale_vector_ctl, f_mat->f_self);
    f_mat->f_viewpt_in_viewer_ctl =  init_f_ctl_cr_array(c_modeview_viewpt_in_view_ctl, f_mat->f_self);
    f_mat->f_projection_type_ctl =   init_f_ctl_chara_item(c_modeview_projection_type_ctl, f_mat->f_self);
    return;
}


struct modelview_ctl_c * init_f_VIZ_view_matrix_ctl(char *ctl_file_name,
                                                    void *(*c_load_self)(void *f_parent),
                                                    void *f_parent){
    struct modelview_ctl_c *f_mat
            = (struct modelview_ctl_c *) malloc(sizeof(struct modelview_ctl_c));
    if(f_mat == NULL){
        printf("malloc error for modelview_ctl_c\n");
        exit(0);
    };
    f_mat->f_self =  c_load_self(f_parent);
    
    init_f_VIZ_view_matrix_ctl_items(ctl_file_name, f_mat);
    return f_mat;
};

static struct modelview_ctl_c * init_f_VIZ_mul_view_matrix_ctl(char *ctl_file_name,
                                                        void *(*c_load_self)(int idx, void *f_parent),
                                                        int idx, void *f_parent){
    struct modelview_ctl_c *f_mat
            = (struct modelview_ctl_c *) malloc(sizeof(struct modelview_ctl_c));
    if(f_mat == NULL){
        printf("malloc error for modelview_ctl_c\n");
        exit(0);
    };
    f_mat->f_self =  c_load_self(idx, f_parent);
    
    init_f_VIZ_view_matrix_ctl_items(ctl_file_name, f_mat);
    return f_mat;
};

struct void_clist * init_f_PVR_mul_vmats_ctls(void *f_parent)
{
    char *f_block_name =   (char *) c_VIZ_mul_mdlvw_ctl_block_name(f_parent);
    struct void_clist *f_vmat_ctls = init_void_clist(strngcopy_from_f(f_block_name));
    f_vmat_ctls->f_parent = f_parent;

    int i;
    for(i=0;i<c_VIZ_mul_mdlvw_num_mat_c(f_vmat_ctls->f_parent);i++){
        f_block_name = c_VIZ_mul_mdlvw_fname_ctl(i, f_vmat_ctls->f_parent);
        struct modelview_ctl_c *f_ctl_tmp = init_f_VIZ_mul_view_matrix_ctl(strngcopy_from_f(f_block_name),
                                                                           c_VIZ_mul_mdlvw_matrices,
                                                                           i, f_vmat_ctls->f_parent);
        append_void_clist((void *) f_ctl_tmp, f_vmat_ctls);
    }
    return f_vmat_ctls;
}


void dealloc_f_PVR_mul_vmats_ctls(struct void_clist *f_vmat_ctls){
    int i;
    for(i=0;i<count_void_clist(f_vmat_ctls);i++){
        struct modelview_ctl_c *f_mat = (struct modelview_ctl_c *) void_clist_at_index(i,f_vmat_ctls);
        dealloc_modelview_ctl_c(f_mat);
    }
    dealloc_void_clist(f_vmat_ctls);
    return;
};

