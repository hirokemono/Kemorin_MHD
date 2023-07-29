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
label[ 0]   updated_sign
label[ 1]   lic_file_prefix
label[ 2]   lic_output_format
label[ 3]   monitoring_mode
label[ 4]   streo_imaging
label[ 5]   anaglyph_switch
label[ 6]   quilt_3d_imaging
label[ 7]   LIC_ctl
label[ 8]   plot_area_ctl
label[ 9]   view_transform_ctl 
label[10]   LIC_color_ctl 
label[11]   colormap_ctl 
label[12]   lighting_ctl 
label[13]   colorbar_ctl 
label[14]   section_ctl 
label[15]   isosurface_ctl 
label[16]   snapshot_movie_ctl 

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
label[12]   LIC_repartition_ctl
label[13]   elapsed_time_monitor

Check lic_ctl_labls->label_LIC_noise_f
ctl_list->num_labels 7 
label[0]   noise_type 
label[1]   noise_file_prefix 
label[2]   noise_file_format 
label[3]   noise_resolution 
label[4]   noise_step_size 
label[5]   noise_cube_size 
label[6]   noise_delta_x 

Check lic_ctl_labls->label_LIC_kernel_f
ctl_list->num_labels 7 
label[0]   kernel_type 
label[1]   kernel_resolution 
label[2]   gaussian_width_ctl 
label[3]   peak_position_ctl 
label[4]   trace_length_mode 
label[5]   half_length_ctl 
label[6]   max_trace_count 

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
label[13]   background_color_ctl 

Check lic_ctl_labls->label_lic_dirs
ctl_list->num_labels 3 
label[0]   X 
label[1]   Y 
label[2]   Z 
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"
#include "t_control_label_from_f.h"
#include "m_PVR_control_labels_from_f.h"

#ifndef M_LIC_CONTROL_LABELS_FROM_F_
#define M_LIC_CONTROL_LABELS_FROM_F_

struct lic_control_labels{
	struct control_labels_f *label_LIC_pvr_ctl_f;
	struct control_labels_f *label_LIC_ctl_f;
	struct control_labels_f *label_LIC_noise_f;
	struct control_labels_f *label_LIC_kernel_f;
	
	struct control_labels_f *label_lic_modelview;
	struct control_labels_f *label_lic_pixels;
	struct control_labels_f *label_lic_project;
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

struct control_labels_f * init_label_LIC_pvr_ctl_f();
struct control_labels_f * init_label_LIC_ctl_f();
struct control_labels_f * init_label_LIC_noise_f();
struct control_labels_f * init_label_LIC_kernel_f();
struct control_labels_f * init_label_LIC_cmap();


struct lic_control_labels * init_lic_control_labels();
void dealloc_lic_control_labels(struct lic_control_labels *lic_ctl_labls);
void check_lic_control_labels(struct lic_control_labels *lic_ctl_labls);

#endif    /* M_LIC_CONTROL_LABELS_FROM_F_ */
