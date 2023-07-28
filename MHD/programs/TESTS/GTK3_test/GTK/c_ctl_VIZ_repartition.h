/*
//  c_ctl_VIZ_repartition.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_VIZ_REPARTITION_H_
#define C_CTL_VIZ_REPARTITION_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_int_IO.h"
#include "t_control_real2_IO.h"
#include "c_ctl_data_platforms.h"

struct f_LIC_masking_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    
    struct chara_ctl_item *f_mask_type_ctl;
    struct chara_ctl_item *f_field_name_ctl;
    struct chara_ctl_item *f_component_ctl;
    struct real2_ctl_item *f_mask_range_ctl;
    
    void *void_panel;
};


struct f_FEM_sleeve_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    
    struct chara_ctl_item *f_sleeve_extension_mode_ctl;
    struct int_ctl_item   *f_sleeve_level_ctl;
    struct real_ctl_item  *f_sleeve_size_ctl;
    struct chara_ctl_item *f_ref_vector_ctl;
};


struct f_new_patition_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    
    struct chara_ctl_item *f_repart_table_head_ctl;
    struct chara_ctl_item *f_repart_table_fmt_ctl;
    struct chara_ctl_item *f_partition_reference_ctl;
    struct chara_ctl_item *f_trace_count_head_ctl;
    struct chara_ctl_item *f_trace_count_fmt_ctl;
    
    struct chara_int_clist *f_ndomain_section_ctl;
    
    struct int_ctl_item    *f_ratio_of_grouping_ctl;
    struct int_ctl_item    *f_sleeve_level_ctl;
    struct real_ctl_item   *f_weight_to_previous_ctl;
    
    struct chara_ctl_item *f_masking_switch_ctl;
    struct real_ctl_item  *f_masking_weight_ctl;
    struct real_ctl_item  *f_power_of_volume_ctl;
    
	struct void_clist *f_mask_ctl;
};

struct f_VIZ_repartition_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	char * repart_ctl_file_name;
    
    struct f_platform_control *f_viz_plt;
    struct f_FEM_mesh_FILE_ctl *f_Fmesh_ctl;
    struct f_new_patition_ctl *f_new_part_ctl;
    struct f_FEM_sleeve_control *f_Fsleeve_ctl;
};

/* prototypes */

extern void * c_VIZ_multi_masking_ctl_name(void *f_new_part_ctl);
extern int    c_VIZ_num_multi_masking_ctl(void *f_new_part_ctl);
extern void * c_VIZ_multi_mask_ctl(int idx, void *f_new_part_ctl);


struct f_LIC_masking_ctl * init_f_LIC_masking_ctl(int idx, void *f_parent);
void dealloc_f_LIC_masking_ctl(struct f_LIC_masking_ctl *f_mask_ctl);


struct f_FEM_sleeve_control * init_f_FEM_sleeve_control(void *(*c_load_self)(void *f_parent),
                                                        void *f_parent);
void dealloc_f_FEM_sleeve_control(struct f_FEM_sleeve_control *f_Fsleeve_ctl);

struct f_new_patition_ctl * init_f_new_patition_ctl(void *(*c_load_self)(void *f_parent),
                                                    void *f_parent);
void dealloc_f_new_patition_ctl(struct f_new_patition_ctl *f_new_part_ctl);


struct f_VIZ_repartition_ctl * init_f_VIZ_repartition_ctl(char *ctl_file_name,
                                                          void *(*c_load_self)(void *f_parent),
                                                          void *f_parent);
void dealloc_f_VIZ_repartition_ctl(struct f_VIZ_repartition_ctl *f_repart_ctl);

#endif /* C_CTL_VIZ_REPARTITION_H_ */
