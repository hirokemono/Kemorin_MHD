/*
//  c_ctl_data_MAP.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_MAP_H_
#define C_CTL_DATA_MAP_H_

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
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"
#include "c_ctl_data_PSF_ISOs.h"
#include "c_ctl_data_PVR_view_matrix.h"
#include "c_ctl_data_PVR_colormap.h"

struct f_MAP_section_ctl{
     void * f_self;
     int * f_iflag;
     
    char *c_block_name;
    
    struct f_VIZ_PSF_def_ctl *f_psf_def_c;
    
    struct chara_ctl_item *f_zeroline_switch_ctl;
    struct chara_ctl_item *f_isoline_color_mode;
    struct int_ctl_item   *f_isoline_number_ctl;
    struct real2_ctl_item *f_isoline_range_ctl;
    struct real_ctl_item  *f_isoline_width_ctl;
    struct real_ctl_item *f_grid_width_ctl;
    struct chara_ctl_item *f_tan_cyl_switch_ctl;
    struct real_ctl_item  *f_tangent_cylinder_inner_ctl;
    struct real_ctl_item  *f_tangent_cylinder_outer_ctl;
};


struct f_VIZ_MAP_ctl{
     void * f_self;
     int * f_iflag;
     
    char *c_block_name;
    char *map_ctl_file_name;
    
    struct modelview_ctl_c *f_mat;
    struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c;
    struct f_MAP_section_ctl *f_map_define_ctl;
    
    struct chara_ctl_item *f_map_image_prefix_ctl;
    struct chara_ctl_item *f_map_image_fmt_ctl;
    struct chara_ctl_item *f_map_field_ctl;
    struct chara_ctl_item *f_map_comp_ctl;
    struct chara_ctl_item *f_isoline_field_ctl;
    struct chara_ctl_item *f_isoline_comp_ctl;
    
    void *void_panel;
};


/* prototypes */
extern void * c_append_viz_map_render_ctls(int idx, char *block_name, void *f_map_ctls);
extern void * c_delete_viz_map_render_ctls(int idx, void *f_map_ctls);

struct f_VIZ_MAP_ctl * init_f_VIZ_MAP_ctl(int idx, void *f_parent);
void dealloc_f_VIZ_MAP_ctl(struct f_VIZ_MAP_ctl *f_map_ctl);
struct void_clist * init_f_VIZ_map_ctls(void *f_parent, int *f_num_map_ctl);


#endif /* C_CTL_DATA_MAP_H_ */
