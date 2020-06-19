/*
//  t_control_data_4_psf_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_control_data_4_psf_c_h_
#define t_control_data_4_psf_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_control_chara2_IO.h"
#include "m_PSF_control_labels_from_f.h"

struct psf_define_ctl_c{
	struct control_labels_f *label_psf_def_ctl;
	
	struct chara_ctl_item *section_method_ctl;
	
	struct chara_real_clist *psf_coefs_list;
	struct chara_real_clist *psf_normal_list;
	struct chara_real_clist *psf_center_list;
	struct chara_real_clist *psf_axis_list;
	
	struct real_ctl_item *radius_psf_ctl;
	struct chara_ctl_item *psf_group_name_ctl;
	
	struct chara_clist *psf_area_list;
};

struct psf_field_ctl_c{
	struct control_labels_f *label_fld_on_psf_ctl;
	
	struct chara2_clist *psf_out_field_list;
};

struct psf_ctl_c{
	struct control_labels_f *label_psf_ctl;
	
	struct chara_ctl_item *psf_file_head_ctl;
	struct chara_ctl_item *psf_output_type_ctl;
	
	int iflag_surface_define;
    char *psf_def_file_name;
	struct psf_define_ctl_c *psf_def_c;
	int iflag_output_field;
	struct psf_field_ctl_c *psf_fld_c;
};


/* prototypes */
struct psf_define_ctl_c * init_psf_define_ctl_c();
void dealloc_psf_define_ctl_c(struct psf_define_ctl_c *psf_def_c);
int read_psf_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_define_ctl_c *psf_def_c);
int write_psf_define_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_define_ctl_c *psf_def_c);

struct psf_field_ctl_c * init_psf_field_ctl_c();
void dealloc_psf_field_ctl_c(struct psf_field_ctl_c *psf_fld_c);
int read_psf_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_field_ctl_c *psf_fld_c);
int write_psf_field_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_field_ctl_c *psf_fld_c);

struct psf_ctl_c * init_psf_ctl_c();
void dealloc_psf_ctl_c(struct psf_ctl_c *psf_c);
int read_psf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_ctl_c *psf_c);
int write_psf_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_ctl_c *psf_c);


int read_psf_define_file_c(const char *file_name, char buf[LENGTHBUF],
                           struct control_labels_f *label_psf_ctl,
                           struct psf_define_ctl_c *psf_def_c);
int write_psf_define_file_c(const char *file_name, 
                            struct control_labels_f *label_psf_ctl,
                            struct psf_define_ctl_c *psf_def_c);

void rename_psf_define_file_c(struct psf_ctl_c *psf_c);
int read_psf_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
			struct psf_ctl_c *psf_c);
int write_psf_ctl_file_c(const char *file_name, struct psf_ctl_c *psf_c);

#endif /* t_control_data_4_psf_c_h_ */
