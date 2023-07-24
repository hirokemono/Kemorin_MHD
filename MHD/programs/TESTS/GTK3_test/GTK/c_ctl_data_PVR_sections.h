/*
//  c_ctl_data_PVR_sections.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_PVR_SECTIONS_H_
#define C_CTL_DATA_PVR_SECTIONS_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_int2_items_c.h"
#include "c_ctl_data_PSF_ISOs.h"


struct f_PVR_section_ctl{
     void * f_self;
     int * f_iflag;
     
    char *c_block_name;
    char *c_fname_sect_ctl;
    
    struct f_VIZ_PSF_def_ctl *f_psf_def_c;
    
    struct real_ctl_item *f_opacity_ctl;
    struct chara_ctl_item *f_zeroline_switch_ctl;
};

struct f_PVR_isosurface_ctl{
     void * f_self;
     int * f_iflag;
     
    char *c_block_name;
    
    struct chara_ctl_item *f_isosurf_type_ctl;
    struct real_ctl_item  *f_iso_value_ctl;
    struct real_ctl_item  *f_opacity_ctl;
};

/* prototypes */

struct void_clist * init_f_PVR_sections_ctl(void *f_parent);
void dealloc_f_PVR_sections_ctl(struct void_clist *f_pvr_scts_c);

struct void_clist * init_f_PVR_isosurfs_ctl(void *f_parent);
void dealloc_f_PVR_isosurfs_ctl(struct void_clist *f_pvr_isos_c);

#endif /* C_CTL_DATA_PVR_SECTIONS_H_ */
