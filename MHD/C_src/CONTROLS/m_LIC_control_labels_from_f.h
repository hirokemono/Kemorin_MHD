/*
//  m_LIC_control_labels_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/


/*
Labels for LIC module controls
Check lic_ctl_labls->label_LIC_pvr_ctl_f
ctl_list->num_labels 18 
label[0]   updated_sign 
label[1]   lic_file_prefix 
label[2]   lic_image_format 
label[3]   monitoring_mode 
label[4]   image_tranceparency 
label[5]   max_pe_4_composit 
label[6]   streo_imaging 
label[7]   anaglyph_image 
label[8]   LIC_ctl 
label[9]   plot_area_ctl 
label[10]   view_transform_ctl 
label[11]   LIC_color_ctl 
label[12]   colormap_ctl 
label[13]   lighting_ctl 
label[14]   colorbar_ctl 
label[15]   section_ctl 
label[16]   isosurface_ctl 
label[17]   movie_mode_ctl 

Check lic_ctl_labls->label_LIC_ctl_f
ctl_list->num_labels 12 
label[0]   LIC_field 
label[1]   color_field 
label[2]   color_component 
label[3]   opacity_field 
label[4]   opacity_component 
label[5]   masking_control 
label[6]   cube_noise_ctl 
label[7]   kernel_ctl 
label[8]   vr_sample_mode 
label[9]   step_size 
label[10]   normalization_type 
label[11]   normalization_value 

Check lic_ctl_labls->label_LIC_noise_f
ctl_list->num_labels 6 
label[0]   noise_type 
label[1]   noise_file_prefix 
label[2]   noise_file_format 
label[3]   noise_resolution 
label[4]   noise_step_size 
label[5]   noise_cube_size 

Check lic_ctl_labls->label_LIC_kernel_f
ctl_list->num_labels 7 
label[0]   kernel_type 
label[1]   kernel_resolution 
label[2]   gaussian_width_ctl 
label[3]   peak_position_ctl 
label[4]   trace_length_mode 
label[5]   half_length_ctl 
label[6]   max_trace_count 

Check lic_ctl_labls->label_lic_modelview
ctl_list->num_labels 12 
label[0]   image_size_ctl 
label[1]   look_at_point_ctl 
label[2]   viewpoint_ctl 
label[3]   up_direction_ctl 
label[4]   view_rotation_vec_ctl 
label[5]   view_rotation_deg_ctl 
label[6]   scale_factor_ctl 
label[7]   scale_factor_vec_ctl 
label[8]   viewpoint_in_viewer_ctl 
label[9]   projection_matrix_ctl 
label[10]   modelview_matrix_ctl 
label[11]   streo_view_parameter_ctl 

Check lic_ctl_labls->label_lic_pixels
ctl_list->num_labels 2 
label[0]   x_pixel_ctl 
label[1]   y_pixel_ctl 

Check lic_ctl_labls->label_lic_streo
ctl_list->num_labels 2 
label[0]   focal_point_ctl 
label[1]   eye_separation_ctl 

Check lic_ctl_labls->label_lic_area
ctl_list->num_labels 2 
label[0]   chosen_ele_grp_ctl 
label[1]   surface_enhanse_ctl 

Check lic_ctl_labls->label_lic_light
ctl_list->num_labels 4 
label[0]   position_of_lights 
label[1]   ambient_coef_ctl 
label[2]   diffuse_coef_ctl 
label[3]   specular_coef_ctl 

Check lic_ctl_labls->label_lic_cmap
ctl_list->num_labels 13 
label[0]   colormap_mode_ctl 
label[1]   LIC_color_field 
label[2]   LIC_color_componenet 
label[3]   LIC_transparent_field 
label[4]   LIC_transparent_componenet 
label[5]   data_mapping_ctl 
label[6]   range_min_ctl 
label[7]   range_max_ctl 
label[8]   color_table_ctl 
label[9]   opacity_style_ctl 
label[10]   constant_opacity_ctl 
label[11]   linear_opacity_ctl 
label[12]   step_opacity_ctl 

Check lic_ctl_labls->label_lic_cbar
ctl_list->num_labels 7 
label[0]   colorbar_switch_ctl 
label[1]   colorbar_scale_ctl 
label[2]   font_size_ctl 
label[3]   num_grid_ctl 
label[4]   iflag_zeromarker 
label[5]   colorbar_range 
label[6]   axis_label_switch 

Check lic_ctl_labls->label_lic_cmap_bar
ctl_list->num_labels 2 
label[0]   colormap_ctl 
label[1]   colorbar_ctl 

Check lic_ctl_labls->label_lic_section
ctl_list->num_labels 2 
label[0]   surface_define 
label[1]   opacity_ctl 

Check lic_ctl_labls->label_lic_isosurf
ctl_list->num_labels 3 
label[0]   isosurf_value 
label[1]   opacity_ctl 
label[2]   surface_direction 

Check lic_ctl_labls->label_lic_movie
ctl_list->num_labels 7 
label[0]   movie_mode_ctl 
label[1]   num_frames_ctl 
label[2]   rotation_axis_ctl 
label[3]   start_view_control 
label[4]   end_view_control 
label[5]   apature_range 
label[6]   LIC_kernel_peak_range 

Check lic_ctl_labls->label_lic_dirs
ctl_list->num_labels 3 
label[0]   X 
label[1]   Y 
label[2]   Z 

Check lic_ctl_labls->label_lic_dirs
ctl_list->num_labels 4 
label[0]   rotation 
label[1]   apature 
label[2]   view_matrices 
label[3]   LIC_kernel 

Check lic_ctl_labls->flag_lic_isosurf_dir
ctl_list->num_labels 3 
label[0]   forward_surface 
label[1]   reverse_surface 
label[2]   boarder 
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"
#include "t_control_label_from_f.h"

#ifndef M_LIC_CONTROL_LABELS_FROM_F_
#define M_LIC_CONTROL_LABELS_FROM_F_

struct lic_control_labels{
	struct control_labels_f *label_LIC_pvr_ctl_f;
	struct control_labels_f *label_LIC_ctl_f;
	struct control_labels_f *label_LIC_noise_f;
	struct control_labels_f *label_LIC_kernel_f;
	
	struct control_labels_f *label_lic_modelview;
	struct control_labels_f *label_lic_pixels;
	struct control_labels_f *label_lic_streo;
	struct control_labels_f *label_lic_area;
	struct control_labels_f *label_lic_light;
	struct control_labels_f *label_lic_cmap;
	struct control_labels_f *label_lic_cbar;
	struct control_labels_f *label_lic_cmap_bar;
	struct control_labels_f *label_lic_section;
	struct control_labels_f *label_lic_isosurf;
	struct control_labels_f *label_lic_movie;
	
	struct control_labels_f *label_lic_dirs;
	struct control_labels_f *flag_lic_movie_mode;
	struct control_labels_f *flag_lic_isosurf_dir;
};

/*  prototype */

struct lic_control_labels * init_lic_control_labels();
void dealloc_lic_control_labels(struct lic_control_labels *lic_ctl_labls);
void check_lic_control_labels(struct lic_control_labels *lic_ctl_labls);

#endif    /* M_LIC_CONTROL_LABELS_FROM_F_ */
