/*
//  m_PSF_control_labels_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/
/*
Labels for Sections module controls

Check psf_ctl_lbls->label_psf_def_ctl
ctl_list->num_labels 8 
label[0]   section_method 
label[1]   coefs_ctl 
label[2]   normal_vector 
label[3]   axial_length 
label[4]   center_position 
label[5]   radius 
label[6]   group_name 
label[7]   section_area_ctl 

Check psf_ctl_lbls->label_psf_ctl
ctl_list->num_labels 5 
label[0]   section_file_prefix 
label[1]   psf_output_type 
label[2]   surface_define 
label[3]   output_field_define 
label[4]   psf_file_head 

Check psf_ctl_lbls->label_fld_on_psf_ctl
ctl_list->num_labels 3 
label[0]   result_type 
label[1]   output_field 
label[2]   result_value 

Check psf_ctl_lbls->num_label_psf_def_type_c 6
Check psf_ctl_lbls->label_psf_define_ctl
ctl_list->num_labels 7 
label[0]   equation 
label[1]   plane 
label[2]   sphere 
label[3]   ellipsoid 
label[4]   hyperboloid 
label[5]   paraboloid 
label[6]   group 

Check psf_ctl_lbls->label_psf_format
ctl_list->num_labels 12 
label[0]   UDT 
label[1]   UCD 
label[2]   VTK 
label[3]   VTD 
label[4]   ISO 
label[5]   PSF 
label[6]   UDT_gzip 
label[7]   UCD_gzip 
label[8]   VTK_gzip 
label[9]   VTD_gzip 
label[10]   ISO_gzip 
label[11]   PSF_gzip 

Check psf_ctl_lbls->label_psf_dirs
ctl_list->num_labels 3 
label[0]   X 
label[1]   Y 
label[2]   Z 

Check psf_ctl_lbls->label_psf_coefs
ctl_list->num_labels 10 
label[0]   X^2 
label[1]   Y^2 
label[2]   Z^2 
label[3]   XY 
label[4]   YZ 
label[5]   ZX 
label[6]   X 
label[7]   Y 
label[8]   Z 
label[9]   Const 
*/

/*
Labels for Isosurface module controls

Check iso_ctl_lbls->num_label_iso_ctl_c 4
Check iso_ctl_lbls->label_iso_ctl_w_dpl
ctl_list->num_labels 6 
label[0]   isosurface_file_prefix 
label[1]   iso_output_type 
label[2]   isosurf_define 
label[3]   field_on_isosurf 
label[4]   iso_file_head 
label[5]   isosurf_result_define 

Check iso_ctl_lbls->label_iso_define_ctl
ctl_list->num_labels 4 
label[0]   isosurf_field 
label[1]   isosurf_component 
label[2]   isosurf_value 
label[3]   isosurf_area_ctl 

Check iso_ctl_lbls->label_fld_on_iso_ctl
ctl_list->num_labels 3 
label[0]   result_type 
label[1]   output_field 
label[2]   result_value 

Check iso_ctl_lbls->flag_iso_color
ctl_list->num_labels 2 
label[0]   const 
label[1]   field 

Check iso_ctl_lbls->flag_iso_format
ctl_list->num_labels 6 
label[0]   UCD 
label[1]   VTK 
label[2]   ISO 
label[3]   UCD_gzip 
label[4]   VTK_gzip 
label[5]   ISO_gzip 

*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"
#include "t_control_label_from_f.h"

#ifndef M_PSF_CONTROL_LABELS_FROM_F_
#define M_PSF_CONTROL_LABELS_FROM_F_


struct psf_control_labels{
	struct control_labels_f *label_psf_def_ctl;
	struct control_labels_f *label_fld_on_psf_ctl;
	struct control_labels_f *label_psf_ctl;
	
	int num_label_psf_def_type_c;
	struct control_labels_f *label_psf_define_ctl;
	struct control_labels_f *label_psf_format;
	struct control_labels_f *label_psf_dirs;
	struct control_labels_f *label_psf_coefs;
};

struct iso_control_labels{
	int num_label_iso_ctl_c;
	struct control_labels_f *label_iso_ctl_w_dpl;
	struct control_labels_f *label_iso_define_ctl;
	struct control_labels_f *label_fld_on_iso_ctl;
	
	struct control_labels_f *flag_iso_color;
	struct control_labels_f *flag_iso_format;
};

/*  prototype */

struct control_labels_f * init_label_psf_ctl();
struct control_labels_f * init_label_psf_def_ctl();
struct control_labels_f * init_label_fld_on_psf_ctl();

struct control_labels_f * init_label_iso_ctl_w_dpl();
struct control_labels_f * init_label_iso_define_ctl();
struct control_labels_f * init_flag_iso_format();
struct control_labels_f * init_flag_iso_color();

void set_primary_psf_format_flag_c(char *name);
void set_primary_iso_format_flag_c(char *name);

struct psf_control_labels * init_psf_control_labels();
void dealloc_psf_control_labels(struct psf_control_labels *psf_ctl_lbls);
void check_psf_control_labels(struct psf_control_labels *psf_ctl_lbls);

struct iso_control_labels * init_iso_control_labels();
void dealloc_iso_control_labels(struct iso_control_labels *iso_ctl_lbls);
void check_iso_control_labels(struct iso_control_labels *iso_ctl_lbls);

#endif    /* M_PSF_CONTROL_LABELS_FROM_F_ */
