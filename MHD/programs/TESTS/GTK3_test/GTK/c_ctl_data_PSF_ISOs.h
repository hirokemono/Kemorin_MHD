/*
//  c_ctl_data_PSF_ISOs.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_PSF_ISOS_H_
#define C_CTL_DATA_PSF_ISOS_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "t_ctl_array_chara2_items_c.h"


struct f_VIZ_PSF_def_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *psf_def_file_name;
    
    struct chara_ctl_item   *f_section_method_ctl;
    struct chara_real_clist *f_psf_coefs_ctl;
    struct chara_real_clist *f_psf_center_ctl;
    struct chara_real_clist *f_psf_normal_ctl;
    struct chara_real_clist *f_psf_axis_ctl;
    struct real_ctl_item    *f_radius_psf_ctl;
    struct chara_ctl_item   *f_psf_group_name_ctl;
    struct chara_clist      *f_psf_area_ctl;
};

struct f_VIZ_fld_on_PSF_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *fname_fld_on_psf;
    
    struct chara2_clist *f_field_output_ctl;
    struct real_ctl_item *f_output_value_ctl;
    struct chara_ctl_item *f_output_type_ctl;
    
};

struct f_VIZ_ISO_def_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    
    struct chara_ctl_item *f_isosurf_data_ctl;
    struct chara_ctl_item *f_isosurf_comp_ctl;
    struct real_ctl_item *f_isosurf_value_ctl;
    struct chara_clist *f_iso_area_ctl;
};

struct f_VIZ_PSF_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *psf_ctl_file_name;
    
    struct f_VIZ_PSF_def_ctl *f_psf_def_c;
    
    struct f_VIZ_fld_on_PSF_ctl *f_fld_on_psf_c;
    
    struct chara_ctl_item *f_psf_file_head_ctl;
    struct chara_ctl_item *f_psf_output_type_ctl;
    
    void *void_panel;
};

struct f_VIZ_ISO_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *iso_ctl_file_name;
    
    struct f_VIZ_fld_on_PSF_ctl *f_fld_on_iso_c;
    struct f_VIZ_ISO_def_ctl *f_iso_def_c;
    
    struct chara_ctl_item *f_iso_file_head_ctl;
    struct chara_ctl_item *f_iso_output_type_ctl;
    
    void *void_panel;
};

/* prototypes */
extern void * c_append_viz_section_ctls(int idx, char *block_name, void *f_psf_ctls);
extern void * c_delete_viz_section_ctls(int idx, void *f_psf_ctls);

extern void * c_append_viz_isosurf_ctls(int idx, char *block_name, void *f_iso_ctls);
extern void * c_delete_viz_isosurf_ctls(int idx, void *f_iso_ctls);

struct f_VIZ_PSF_ctl * init_f_VIZ_PSF_ctl(int idx, void *f_parent);
void dealloc_f_VIZ_PSF_ctl(struct f_VIZ_PSF_ctl *f_psf_ctl);
struct void_clist * init_f_VIZ_psf_ctls(void *f_parent, int *f_num_psf_ctl);

struct f_VIZ_ISO_ctl * init_f_VIZ_ISO_ctl(int idx, void *f_parent);
void dealloc_f_VIZ_ISO_ctl(struct f_VIZ_ISO_ctl *f_iso_ctl);
struct void_clist * init_f_VIZ_iso_ctls(void *f_parent, int *f_num_iso_ctl);

#endif /* C_CTL_DATA_PSF_ISOS_H_ */
