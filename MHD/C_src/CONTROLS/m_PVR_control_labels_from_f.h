/*
//  m_PVR_control_labels_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/


/*
Labels for PVR module controls

Check pvr_ctl_labls->num_label_pvr_ctl_c 19
Check pvr_ctl_labls->label_pvr_ctl_w_dpl
ctl_list->num_labels 20 
label[0]   updated_sign 
label[1]   pvr_file_prefix 
label[2]   pvr_output_format 
label[3]   monitoring_mode 
label[4]   streo_imaging 
label[5]   anaglyph_switch 
label[6]   quilt_3d_imaging
label[7]   output_field 
label[8]   output_component 
label[9]   plot_area_ctl 
label[10]   view_transform_ctl 
label[11]   pvr_color_ctl 
label[12]   colormap_ctl 
label[13]   lighting_ctl 
label[14]   colorbar_ctl 
label[15]   section_ctl 
label[16]   isosurface_ctl 
label[17]   snapshot_movie_ctl 
label[18]   image_rotation_ctl 

Check pvr_ctl_labls->label_pvr_modelview
ctl_list->num_labels 12 
label[0]   image_size_ctl 
label[1]   look_at_point_ctl 
label[2]   eye_position_ctl
label[3]   up_direction_ctl 
label[4]   view_rotation_vec_ctl 
label[5]   view_rotation_deg_ctl 
label[6]   scale_factor_ctl 
label[7]   scale_factor_vec_ctl 
label[8]   eye_position_in_viewer_ctl
label[9]   projection_matrix_ctl 
label[10]   modelview_matrix_ctl 
label[11]   stereo_view_parameter_ctl 
label[12]   projection_type_ctl 

Check pvr_ctl_labls->label_pvr_pixels
ctl_list->num_labels 2 
label[0]   x_pixel_ctl 
label[1]   y_pixel_ctl 

Check pvr_ctl_labls->label_pvr_project
ctl_list->len_f 255 
ctl_list->num_labels 4 
label[0]   perspective_angle_ctl 
label[1]   perspective_xy_ratio_ctl 
label[2]   perspective_near_ctl 
label[3]   perspective_far_ctl 
label[4]   horizontal_range_ctl
label[5]   vertical_range_ctl 

Check pvr_ctl_labls->label_pvr_streo
ctl_list->num_labels 2 
label[0]   focal_distance_ctl 
label[1]   eye_separation_ctl 

 Check pvr_ctl_labls->label_pvr_light
ctl_list->num_labels 4 
label[0]   position_of_lights 
label[1]   ambient_coef_ctl 
label[2]   diffuse_coef_ctl 
label[3]   specular_coef_ctl 

Check pvr_ctl_labls->label_pvr_cmap
ctl_list->num_labels 9 
label[0]   colormap_mode_ctl 
label[1]   data_mapping_ctl 
label[2]   range_min_ctl 
label[3]   range_max_ctl 
label[4]   color_table_ctl 
label[5]   opacity_style_ctl 
label[6]   constant_opacity_ctl 
label[7]   linear_opacity_ctl 
label[8]   step_opacity_ctl 
label[9]   background_color_ctl 

Check pvr_ctl_labls->label_pvr_cbar
ctl_list->num_labels 7 
label[0]   colorbar_switch_ctl 
label[1]   colorbar_scale_ctl 
label[2]   font_size_ctl 
label[3]   num_grid_ctl 
label[4]   iflag_zeromarker 
label[5]   colorbar_range 
label[6]   axis_label_switch 

Check pvr_ctl_labls->label_pvr_cmap_bar
ctl_list->num_labels 2 
label[0]   colormap_ctl 
label[1]   colorbar_ctl 

Check pvr_ctl_labls->label_pvr_section
ctl_list->num_labels 
label[ 0]   surface_define 
label[ 1]   opacity_ctl 
label[ 2]   zeroline_switch_ctl 
label[ 3]   isoline_color_mode 
label[ 4]   isoline_number_ctl 
label[ 5]   isoline_range_ctl 
label[ 6]   isoline_width_ctl 
label[ 7]   grid_width_ctl 
label[ 8]   tangent_cylinder_switch_ctl 
label[ 9]   inner_radius_ctl 
label[10]   outer_radius_ctl 

Check pvr_ctl_labls->label_pvr_isosurf
ctl_list->num_labels 3 
label[0]   isosurf_value 
label[1]   opacity_ctl 
label[2]   surface_direction 

Check pvr_ctl_labls->num_label_pvr_movie_c 6
Check lic_ctl_labls->label_lic_movie
ctl_list->num_labels 7 
label[0]   movie_mode_ctl 
label[1]   num_frames_ctl 
label[2]   rotation_axis_ctl 
label[3]   start_view_control 
label[4]   end_view_control 
label[5]   angle_range
label[6]   apature_range
label[7]   LIC_kernel_peak_range

Check pvr_ctl_labls->label_pvr_dirs
ctl_list->num_labels 3 
label[0]   X 
label[1]   Y 
label[2]   Z 

Check pvr_ctl_labls->flag_pvr_movie_mode
ctl_list->num_labels 3 
label[0]   rotation 
label[1]   apature 
label[2]   view_matrices 

Check pvr_ctl_labls->flag_pvr_isosurf_dir
ctl_list->num_labels 3 
label[0]   forward_surface 
label[1]   reverse_surface 
label[2]   boarder 

Check lic_ctl_labls->label_lic_dirs
ctl_list->num_labels 4 
label[0]   rotation 
label[1]   apature 
label[2]   view_matrices 
label[3]   LIC_kernel 
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"
#include "t_control_label_from_f.h"

#ifndef M_PVR_CONTROL_LABELS_FROM_F_
#define M_PVR_CONTROL_LABELS_FROM_F_

struct pvr_control_labels{
	int num_label_pvr_ctl_c;
	struct control_labels_f *label_pvr_ctl_w_dpl;
	struct control_labels_f *label_pvr_modelview;
	struct control_labels_f *label_pvr_pixels;
	struct control_labels_f *label_pvr_project;
	struct control_labels_f *label_pvr_streo;
	struct control_labels_f *label_pvr_light;
	struct control_labels_f *label_pvr_cmap;
	struct control_labels_f *label_pvr_cbar;
	struct control_labels_f *label_pvr_cmap_bar;
	struct control_labels_f *label_pvr_section;
	struct control_labels_f *label_pvr_isosurf;
	
	int num_label_pvr_movie_c;
	struct control_labels_f *label_pvr_movie;
	
    int num_label_quilt_image_c;
    struct control_labels_f *label_quilt_image;

    struct control_labels_f *label_pvr_dirs;
	struct control_labels_f *flag_pvr_isosurf_dir;
	
	int num_flag_pvr_movie_mode_c;
	struct control_labels_f *flag_lic_movie_mode;
};

/*  prototype */

struct control_labels_f * init_label_pvr_ctl_w_dpl();

struct control_labels_f * init_label_pvr_pixels();
struct control_labels_f * init_label_pvr_project();
struct control_labels_f * init_label_pvr_streo();
struct control_labels_f * init_label_pvr_modelview();

struct control_labels_f * init_label_pvr_light();
struct control_labels_f * init_label_pvr_cmap();
struct control_labels_f * init_label_pvr_cbar();
struct control_labels_f * init_label_pvr_cmap_bar();

struct control_labels_f * init_label_pvr_section();

struct control_labels_f * init_label_pvr_isosurf();
struct control_labels_f * init_flag_pvr_isosurf_dir();

struct control_labels_f * init_label_lic_movie();
struct control_labels_f * init_flag_lic_movie_mode();

struct pvr_control_labels * init_pvr_control_labels();
void dealloc_pvr_control_labels(struct pvr_control_labels *pvr_ctl_labls);
void check_pvr_control_labels(struct pvr_control_labels *pvr_ctl_labls);

#endif    /* M_PVR_CONTROL_LABELS_FROM_F_ */
