/*
//  c_ctl_data_LIC.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_LIC_H_
#define C_CTL_DATA_LIC_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_c_lists.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "c_ctl_VIZ_repartition.h"
#include "c_control_data_pvrs.h"


struct f_LIC_noise_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	char * noise_ctl_file_name;
    
    struct chara_ctl_item *f_noise_type_ctl;
    struct chara_ctl_item *f_noise_file_name_ctl;
    struct chara_ctl_item *f_noise_file_format_ctl;
    struct int_ctl_item   *f_noise_resolution_ctl;
    struct int_ctl_item   *f_noise_stepping_ctl;
    struct real_ctl_item  *f_noise_cube_size_ctl;
    struct real_ctl_item  *f_noise_deltax_ctl;
};

struct f_LIC_kernel_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	char * kernel_ctl_file_name;
    
    struct chara_ctl_item *f_kernel_type_ctl;
    struct int_ctl_item   *f_kernel_resolution_ctl;
    struct real_ctl_item  *f_kernel_peak_ctl;
    struct real_ctl_item  *f_kernel_sigma_ctl;
    struct chara_ctl_item *f_trace_length_mode_ctl;
    struct real_ctl_item  *f_half_length_ctl;
    struct int_ctl_item   *f_max_trace_count_ctl;
};

struct f_VIZ_LIC_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    
	struct f_LIC_noise_ctl *f_noise_ctl;
	struct f_LIC_kernel_ctl *f_kernel_ctl;
	struct f_VIZ_repartition_ctl *f_repart_ctl;
    
	struct void_clist *f_mask_ctl;
    
    struct chara_ctl_item *f_LIC_field_ctl;
	struct chara_ctl_item *f_subdomain_elapsed_dump_ctl;
	struct chara_ctl_item *f_color_field_ctl;
	struct chara_ctl_item *f_color_component_ctl;
	struct chara_ctl_item *f_opacity_field_ctl;
	struct chara_ctl_item *f_opacity_component_ctl;
    
	struct chara_ctl_item *f_vr_sample_mode_ctl;
	struct real_ctl_item  *f_step_size_ctl;
	struct chara_ctl_item *f_normalization_type_ctl;
	struct real_ctl_item *f_normalization_value_ctl;
};

struct f_VIZ_LIC_PVR_ctl{
    int f_iflag[1];
    char *lic_ctl_file_name;
	struct f_VIZ_PVR_ctl *f_lic_pvr_ctl;
	struct f_VIZ_LIC_ctl *f_lic_lic_ctl;
    
    void *void_panel;
};


/* prototypes */

extern void * c_append_viz_lic_render_ctls(int idx, char *block_name, void *f_lic_ctls);
extern void * c_delete_viz_lic_render_ctls(int idx, void *f_lic_ctls);

struct f_LIC_noise_ctl * init_f_LIC_noise_ctl(char *ctl_file_name,
                                              void *(*c_load_self)(void *f_parent),
                                              void *f_parent);
void dealloc_f_LIC_noise_ctl(struct f_LIC_noise_ctl *f_noise_ctl);

struct f_LIC_kernel_ctl * init_f_LIC_kernel_ctl(char *ctl_file_name,
                                                void *(*c_load_self)(void *f_parent),
                                                void *f_parent);
void dealloc_f_LIC_kernel_ctl(struct f_LIC_kernel_ctl *f_kernel_ctl);

struct f_VIZ_LIC_ctl * init_f_VIZ_LIC_ctl(int idx, void *f_parent);
struct f_VIZ_LIC_ctl * dealloc_f_VIZ_LIC_ctl(void *void_in);

struct f_VIZ_LIC_PVR_ctl * init_f_VIZ_LIC_PVR_ctl(int idx, void *f_parent);
void dealloc_f_VIZ_LIC_PVR_ctl(struct f_VIZ_LIC_PVR_ctl *f_lic_ctl);

struct void_clist * init_f_VIZ_lic_ctls(void *f_parent, int *f_num_lic_ctl);


#endif /* C_CTL_DATA_LIC_H_ */
