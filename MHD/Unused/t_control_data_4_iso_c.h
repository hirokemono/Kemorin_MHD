/*
//  t_control_data_4_iso_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_control_data_4_iso_c_h_
#define t_control_data_4_iso_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_IO.h"
#include "m_PSF_control_labels_from_f.h"

struct iso_define_ctl_c{
    int iflag_use;

	struct control_labels_f *label_iso_define_ctl;
	
	struct chara_ctl_item *isosurf_data_ctl;
	struct chara_ctl_item *isosurf_comp_ctl;
	
	struct real_ctl_item *isosurf_value_ctl;
	
	struct chara_clist *iso_area_list;
};

struct iso_field_ctl_c{
    int iflag_use;
    
	struct control_labels_f *label_fld_on_iso_ctl;
    struct control_labels_f *flag_iso_color;
	
	struct chara_ctl_item *output_type_ctl;
	struct real_ctl_item *output_value_ctl;
	
	struct chara2_clist *iso_out_field_list;
};

struct iso_ctl_c{
	struct control_labels_f *label_iso_ctl_w_dpl;
    struct control_labels_f *flag_iso_format;
	
	struct chara_ctl_item *iso_file_head_ctl;
	struct chara_ctl_item *iso_output_type_ctl;
	
	struct iso_define_ctl_c *iso_def_c;
	struct iso_field_ctl_c *iso_fld_c;
};

/* prototypes */
char * isosurface_control_head();

struct iso_define_ctl_c * init_iso_define_ctl_c();
void dealloc_iso_define_ctl_c(struct iso_define_ctl_c *iso_def_c);
void read_iso_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_define_ctl_c *iso_def_c);
int write_iso_define_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_define_ctl_c *iso_def_c);

struct iso_field_ctl_c * init_iso_field_ctl_c();
void dealloc_iso_field_ctl_c(struct iso_field_ctl_c *iso_fld_c);
void read_iso_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_field_ctl_c *iso_fld_c);
int write_iso_field_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_field_ctl_c *iso_fld_c);

struct iso_ctl_c * init_iso_ctl_c();
void dealloc_iso_ctl_c(struct iso_ctl_c *iso_c);
int read_iso_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_ctl_c *iso_c);
int write_iso_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_ctl_c *iso_c);

int read_iso_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct iso_ctl_c *iso_c);
int write_iso_ctl_file_c(const char *file_name, struct iso_ctl_c *iso_c);


#endif /* t_control_data_4_iso_c_h_ */
