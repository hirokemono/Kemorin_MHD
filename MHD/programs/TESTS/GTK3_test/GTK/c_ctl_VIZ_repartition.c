/*
//  c_ctl_VIZ_repartition.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_VIZ_repartition.h"

extern void * c_masking_fld_ctl_block_name(void *f_mask_ctl);
extern void * c_masking_fld_ctl_iflag(void *f_mask_ctl);
extern void * c_masking_fld_mask_type_ctl(void *f_mask_ctl);
extern void * c_masking_fld_field_name_ctl(void *f_mask_ctl);
extern void * c_masking_fld_component_ctl(void *f_mask_ctl);
extern void * c_masking_fld_mask_range_ctl(void *f_mask_ctl);


extern void * c_viz_repart_ctl_block_name(void *f_viz_repart_c);
extern void * c_viz_repart_ctl_iflag(void *f_viz_repart_c);
extern void * c_viz_repart_viz_plt_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_Fmesh_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_Fmesh_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_new_part_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_Fsleeve_ctl(void *f_viz_repart_c);

extern void * c_new_repart_ctl_block_name(void *f_new_part_ctl);
extern void * c_new_repart_ctl_iflag(void *f_new_part_ctl);
extern void * c_new_repart_table_head_ctl(void *f_new_part_ctl);
extern void * c_new_repart_table_fmt_ctl(void *f_new_part_ctl);
extern void * c_new_repart_partition_ref_ctl(void *f_new_part_ctl);
extern void * c_new_repart_trace_cnt_head_ctl(void *f_new_part_ctl);
extern void * c_new_repart_trace_cnt_fmt_ctl(void *f_new_part_ctl);
extern void * c_new_repart_ndomain_sect_ctl(void *f_new_part_ctl);
extern void * c_new_repart_ratio_group_ctl(void *f_new_part_ctl);
extern void * c_new_repart_sleeve_level_ctl(void *f_new_part_ctl);
extern void * c_new_repart_weight_to_prev_ctl(void *f_new_part_ctl);
extern void * c_new_repart_mask_switch_ctl(void *f_new_part_ctl);
extern void * c_new_repart_mask_weight_ctl(void *f_new_part_ctl);
extern void * c_new_repart_pwr_of_vol_ctl(void *f_new_part_ctl);
extern void * c_new_repart_mul_masking_ctl(void *f_new_part_ctl);

extern void * c_FEM_sleeve_ctl_block_name(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_ctl_iflag(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_extension_mode_ctl(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_sleeve_level_ctl(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_sleeve_size_ctl(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_ref_vector_ctl(void *f_Fsleeve_ctl);


struct f_LIC_masking_ctl * init_f_LIC_masking_ctl(int idx, void *f_parent)
{
    struct f_LIC_masking_ctl *f_mask_ctl
            = (struct f_LIC_masking_ctl *) malloc(sizeof(struct f_LIC_masking_ctl));
    if(f_mask_ctl == NULL){
        printf("malloc error for f_LIC_masking_ctl\n");
        exit(0);
    };
    f_mask_ctl->f_self =  c_VIZ_multi_mask_ctl(idx, f_parent);
    
    f_mask_ctl->f_iflag =   (int *) c_masking_fld_ctl_iflag(f_mask_ctl->f_self);
    char *f_block_name =   (char *) c_masking_fld_ctl_block_name(f_mask_ctl->f_self);
    f_mask_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_mask_ctl->f_mask_type_ctl =  init_f_ctl_chara_item(c_masking_fld_mask_type_ctl, f_mask_ctl->f_self);
    f_mask_ctl->f_field_name_ctl = init_f_ctl_chara_item(c_masking_fld_field_name_ctl, f_mask_ctl->f_self);
    f_mask_ctl->f_component_ctl =  init_f_ctl_chara_item(c_masking_fld_component_ctl, f_mask_ctl->f_self);
    f_mask_ctl->f_mask_range_ctl = init_f_ctl_r2_item(c_masking_fld_mask_range_ctl, f_mask_ctl->f_self);
    return f_mask_ctl;
}

void dealloc_f_LIC_masking_ctl(struct f_LIC_masking_ctl *f_mask_ctl){
    dealloc_chara_ctl_item_c(f_mask_ctl->f_mask_type_ctl);
    dealloc_chara_ctl_item_c(f_mask_ctl->f_field_name_ctl);
    dealloc_chara_ctl_item_c(f_mask_ctl->f_component_ctl);
    dealloc_real2_ctl_item_c(f_mask_ctl->f_mask_range_ctl);
    
    free(f_mask_ctl->c_block_name);
    f_mask_ctl->f_iflag = NULL;
    f_mask_ctl->f_self = NULL;
    free(f_mask_ctl);
    return;
}


struct f_FEM_sleeve_control * init_f_FEM_sleeve_control(void *(*c_load_self)(void *f_parent),
                                                        void *f_parent)
{
    struct f_FEM_sleeve_control *f_Fsleeve_ctl
            = (struct f_FEM_sleeve_control *) malloc(sizeof(struct f_FEM_sleeve_control));
    if(f_Fsleeve_ctl == NULL){
        printf("malloc error for f_FEM_sleeve_control\n");
        exit(0);
    };
    f_Fsleeve_ctl->f_self =  c_load_self(f_parent);
    
    f_Fsleeve_ctl->f_iflag =   (int *) c_FEM_sleeve_ctl_iflag(f_Fsleeve_ctl->f_self);
    char *f_block_name =   (char *) c_FEM_sleeve_ctl_block_name(f_Fsleeve_ctl->f_self);
    f_Fsleeve_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_Fsleeve_ctl->f_sleeve_extension_mode_ctl = init_f_ctl_chara_item(c_FEM_sleeve_extension_mode_ctl,
                                                                     f_Fsleeve_ctl->f_self);
    f_Fsleeve_ctl->f_sleeve_level_ctl =          init_f_ctl_int_item(c_FEM_sleeve_sleeve_level_ctl,
                                                                   f_Fsleeve_ctl->f_self);
    f_Fsleeve_ctl->f_sleeve_size_ctl =           init_f_ctl_real_item(c_FEM_sleeve_sleeve_size_ctl,
                                                                    f_Fsleeve_ctl->f_self);
    f_Fsleeve_ctl->f_ref_vector_ctl =            init_f_ctl_chara_item(c_FEM_sleeve_ref_vector_ctl,
                                                                     f_Fsleeve_ctl->f_self);
    return f_Fsleeve_ctl;
}

void dealloc_f_FEM_sleeve_control(struct f_FEM_sleeve_control *f_Fsleeve_ctl){
    dealloc_chara_ctl_item_c(f_Fsleeve_ctl->f_sleeve_extension_mode_ctl);
    dealloc_int_ctl_item_c(f_Fsleeve_ctl->f_sleeve_level_ctl);
    dealloc_real_ctl_item_c(f_Fsleeve_ctl->f_sleeve_size_ctl);
    dealloc_chara_ctl_item_c(f_Fsleeve_ctl->f_ref_vector_ctl);
    
    free(f_Fsleeve_ctl->c_block_name);
    f_Fsleeve_ctl->f_iflag = NULL;
    f_Fsleeve_ctl->f_self = NULL;
    free(f_Fsleeve_ctl);
    return;
}


struct f_new_patition_ctl * init_f_new_patition_ctl(void *(*c_load_self)(void *f_parent),
                                                    void *f_parent)
{
    struct f_new_patition_ctl *f_new_part_ctl
            = (struct f_new_patition_ctl *) malloc(sizeof(struct f_new_patition_ctl));
    if(f_new_part_ctl == NULL){
        printf("malloc error for f_new_patition_ctl\n");
        exit(0);
    };
    f_new_part_ctl->f_self =  c_load_self(f_parent);
    
    f_new_part_ctl->f_iflag =   (int *) c_new_repart_ctl_iflag(f_new_part_ctl->f_self);
    char *f_block_name =   (char *) c_new_repart_ctl_block_name(f_new_part_ctl->f_self);
    f_new_part_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_new_part_ctl->f_repart_table_head_ctl =   init_f_ctl_chara_item(c_new_repart_table_head_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_repart_table_fmt_ctl =    init_f_ctl_chara_item(c_new_repart_table_fmt_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_partition_reference_ctl = init_f_ctl_chara_item(c_new_repart_partition_ref_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_trace_count_head_ctl =    init_f_ctl_chara_item(c_new_repart_trace_cnt_head_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_trace_count_fmt_ctl =     init_f_ctl_chara_item(c_new_repart_trace_cnt_fmt_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_ndomain_section_ctl =     init_f_ctl_ci_array(c_new_repart_ndomain_sect_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_ratio_of_grouping_ctl =   init_f_ctl_int_item(c_new_repart_ratio_group_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_sleeve_level_ctl =        init_f_ctl_int_item(c_new_repart_sleeve_level_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_weight_to_previous_ctl =  init_f_ctl_real_item(c_new_repart_weight_to_prev_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_masking_switch_ctl =      init_f_ctl_chara_item(c_new_repart_mask_switch_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_masking_weight_ctl =      init_f_ctl_real_item(c_new_repart_mask_weight_ctl,
                                                                    f_new_part_ctl->f_self);
    f_new_part_ctl->f_power_of_volume_ctl =     init_f_ctl_real_item(c_new_repart_pwr_of_vol_ctl,
                                                                    f_new_part_ctl->f_self);
    
    void *f_mul_maksing = c_new_repart_mul_masking_ctl(f_new_part_ctl->f_self);
    f_block_name = c_VIZ_multi_masking_ctl_name(f_mul_maksing);
    f_new_part_ctl->f_mask_ctl = init_void_clist(strngcopy_from_f(f_block_name));
	f_new_part_ctl->f_mask_ctl->f_parent = f_mul_maksing;
    int i;
    for(i=0;i<c_VIZ_num_multi_masking_ctl(f_mul_maksing);i++){
        void *f_ctl_tmp = init_f_LIC_masking_ctl(i, f_mul_maksing);
		append_void_clist((void *) f_ctl_tmp, f_new_part_ctl->f_mask_ctl);
    };
    
    return f_new_part_ctl;
}

void dealloc_f_new_patition_ctl(struct f_new_patition_ctl *f_new_part_ctl){
    dealloc_chara_ctl_item_c(f_new_part_ctl->f_repart_table_head_ctl);
    dealloc_chara_ctl_item_c(f_new_part_ctl->f_repart_table_fmt_ctl);
    dealloc_chara_ctl_item_c(f_new_part_ctl->f_partition_reference_ctl);
    dealloc_chara_ctl_item_c(f_new_part_ctl->f_trace_count_head_ctl);
    dealloc_chara_ctl_item_c(f_new_part_ctl->f_trace_count_fmt_ctl);
    
    dealloc_chara_int_clist(f_new_part_ctl->f_ndomain_section_ctl);
    dealloc_int_ctl_item_c(f_new_part_ctl->f_ratio_of_grouping_ctl);
    dealloc_int_ctl_item_c(f_new_part_ctl->f_sleeve_level_ctl);
    dealloc_real_ctl_item_c(f_new_part_ctl->f_weight_to_previous_ctl);
    dealloc_chara_ctl_item_c(f_new_part_ctl->f_masking_switch_ctl);
    dealloc_real_ctl_item_c(f_new_part_ctl->f_masking_weight_ctl);
    dealloc_real_ctl_item_c(f_new_part_ctl->f_power_of_volume_ctl);
    
    int i;
    for(i=0;i<count_void_clist(f_new_part_ctl->f_mask_ctl);i++){
        void *f_ctl_tmp = void_clist_at_index(i, f_new_part_ctl->f_mask_ctl);
		dealloc_f_LIC_masking_ctl((struct f_LIC_masking_ctl *) f_ctl_tmp);
    };
    dealloc_void_clist(f_new_part_ctl->f_mask_ctl);
    
    free(f_new_part_ctl->c_block_name);
    f_new_part_ctl->f_iflag = NULL;
    f_new_part_ctl->f_self = NULL;
    free(f_new_part_ctl);
    return;
}

struct f_VIZ_repartition_ctl * init_f_VIZ_repartition_ctl(char *ctl_file_name,
                                                          void *(*c_load_self)(void *f_parent),
                                                          void *f_parent)
{
    struct f_VIZ_repartition_ctl *f_repart_ctl
            = (struct f_VIZ_repartition_ctl *) malloc(sizeof(struct f_VIZ_repartition_ctl));
    if(f_repart_ctl == NULL){
        printf("malloc error for f_VIZ_repartition_ctl\n");
        exit(0);
    };
    f_repart_ctl->f_self =  c_load_self(f_parent);
    
    f_repart_ctl->f_iflag =   (int *) c_viz_repart_ctl_iflag(f_repart_ctl->f_self);
    char *f_block_name =   (char *) c_viz_repart_ctl_block_name(f_repart_ctl->f_self);
    f_repart_ctl->c_block_name = strngcopy_from_f(f_block_name);
    f_repart_ctl->repart_ctl_file_name = ctl_file_name;
    
    f_repart_ctl->f_viz_plt =    init_f_platform_control(c_viz_repart_viz_plt_ctl,
                                                         f_repart_ctl->f_self);
    f_repart_ctl->f_Fmesh_ctl =   init_f_FEM_mesh_FILE_ctl(c_viz_repart_Fmesh_ctl,
                                                           f_repart_ctl->f_self);
    f_repart_ctl->f_new_part_ctl = init_f_new_patition_ctl(c_viz_repart_new_part_ctl,
                                                           f_repart_ctl->f_self);
    f_repart_ctl->f_Fsleeve_ctl =   init_f_FEM_sleeve_control(c_viz_repart_Fsleeve_ctl,
                                                              f_repart_ctl->f_self);
    return f_repart_ctl;
}


void dealloc_f_VIZ_repartition_ctl(struct f_VIZ_repartition_ctl *f_repart_ctl){
    dealloc_f_platform_control(f_repart_ctl->f_viz_plt);
    dealloc_f_FEM_mesh_FILE_ctl(f_repart_ctl->f_Fmesh_ctl);
    dealloc_f_new_patition_ctl(f_repart_ctl->f_new_part_ctl);
    dealloc_f_FEM_sleeve_control(f_repart_ctl->f_Fsleeve_ctl);
    
    free(f_repart_ctl->repart_ctl_file_name);
    free(f_repart_ctl->c_block_name);
    f_repart_ctl->f_iflag = NULL;
    f_repart_ctl->f_self = NULL;
    free(f_repart_ctl);
    return;
}

