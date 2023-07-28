/*
//  c_ctl_data_LIC.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_LIC.h"

extern void * c_LIC_kernel_ctl_block_name(void *f_kernel_ctl);
extern void * c_LIC_kernel_ctl_iflag(void *f_kernel_ctl);
extern void * c_LIC_kernel_type_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_resolution_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_peak_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_kernel_sigma_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_trace_len_mod_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_half_length_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_max_trace_cnt_ctl(void *f_kernel_ctl);

extern void * c_LIC_noise_ctl_block_name(void *f_noise_ctl);
extern void * c_LIC_noise_ctl_iflag(void *f_noise_ctl);
extern void * c_LIC_noise_type_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_file_name_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_file_format_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_resolution_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_stepping_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_cube_size_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_deltax_ctl(void *f_noise_ctl);

extern void * c_VIZ_LIC_ctl_block_name(void *f_lic_ctl);
extern void * c_VIZ_LIC_ctl_iflag(void *f_lic_ctl);
extern void * c_VIZ_LIC_LIC_field_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_pe_elapsed_dump_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_color_field_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_color_comp_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_opacity_field_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_opacity_comp_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_mul_masking_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_fname_noise_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_noise_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_fname_kernel_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_kernel_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_vr_sample_mode_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_step_size_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_normalize_type_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_normalize_value_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_fname_vol_repart_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_repartition_ctl(void *f_lic_ctl);

extern void * c_lic_render_ctls_block_name(void *f_lic_ctls);
extern int    c_lic_render_ctls_num_lic_ctl(void *f_lic_ctls);
extern char * c_lic_render_ctls_fname(int idx, void *f_lic_ctls);
extern void * c_lic_render_ctls_pvr_ctl(int idx, void *f_lic_ctls);
extern void * c_lic_render_ctls_lic_ctl(int idx, void *f_lic_ctls);



struct f_LIC_noise_ctl * init_f_LIC_noise_ctl(char *ctl_file_name,
                                               void *(*c_load_self)(void *f_parent),
                                               void *f_parent)
{
    struct f_LIC_noise_ctl *f_noise_ctl
            = (struct f_LIC_noise_ctl *) malloc(sizeof(struct f_LIC_noise_ctl));
    if(f_noise_ctl == NULL){
        printf("malloc error for f_LIC_noise_ctl\n");
        exit(0);
    };
    f_noise_ctl->f_self =  c_load_self(f_parent);
    
    f_noise_ctl->f_iflag =   (int *) c_LIC_noise_ctl_iflag(f_noise_ctl->f_self);
    char *f_block_name =   (char *) c_LIC_noise_ctl_block_name(f_noise_ctl->f_self);
    f_noise_ctl->c_block_name = strngcopy_from_f(f_block_name);
    f_noise_ctl->noise_ctl_file_name = ctl_file_name;
    
    f_noise_ctl->f_noise_type_ctl =        init_f_ctl_chara_item(c_LIC_noise_type_ctl,
                                                                 f_noise_ctl->f_self);
    f_noise_ctl->f_noise_file_name_ctl =   init_f_ctl_chara_item(c_LIC_noise_file_name_ctl,
                                                                f_noise_ctl->f_self);
    f_noise_ctl->f_noise_file_format_ctl = init_f_ctl_chara_item(c_LIC_noise_file_format_ctl,
                                                                 f_noise_ctl->f_self);
    f_noise_ctl->f_noise_resolution_ctl =  init_f_ctl_int_item(c_LIC_noise_resolution_ctl,
                                                               f_noise_ctl->f_self);
    f_noise_ctl->f_noise_stepping_ctl =    init_f_ctl_int_item(c_LIC_noise_stepping_ctl,
                                                               f_noise_ctl->f_self);
    f_noise_ctl->f_noise_cube_size_ctl =   init_f_ctl_real_item(c_LIC_noise_cube_size_ctl, 
                                                                f_noise_ctl->f_self);
    f_noise_ctl->f_noise_deltax_ctl =      init_f_ctl_real_item(c_LIC_noise_deltax_ctl, 
                                                                f_noise_ctl->f_self);
    return f_noise_ctl;
}


void dealloc_f_LIC_noise_ctl(struct f_LIC_noise_ctl *f_noise_ctl){
    dealloc_chara_ctl_item_c(f_noise_ctl->f_noise_type_ctl);
    dealloc_chara_ctl_item_c(f_noise_ctl->f_noise_file_name_ctl);
    dealloc_chara_ctl_item_c(f_noise_ctl->f_noise_file_format_ctl);
    dealloc_int_ctl_item_c(f_noise_ctl->f_noise_resolution_ctl);
    dealloc_int_ctl_item_c(f_noise_ctl->f_noise_stepping_ctl);
    dealloc_real_ctl_item_c(f_noise_ctl->f_noise_cube_size_ctl);
    dealloc_real_ctl_item_c(f_noise_ctl->f_noise_deltax_ctl);
    
    free(f_noise_ctl->noise_ctl_file_name);
    free(f_noise_ctl->c_block_name);
    f_noise_ctl->f_iflag = NULL;
    f_noise_ctl->f_self = NULL;
    free(f_noise_ctl);
    return;
}


struct f_LIC_kernel_ctl * init_f_LIC_kernel_ctl(char *ctl_file_name,
                                                void *(*c_load_self)(void *f_parent),
                                                void *f_parent)
{
    struct f_LIC_kernel_ctl *f_kernel_ctl
            = (struct f_LIC_kernel_ctl *) malloc(sizeof(struct f_LIC_kernel_ctl));
    if(f_kernel_ctl == NULL){
        printf("malloc error for f_LIC_kernel_ctl\n");
        exit(0);
    };
    f_kernel_ctl->f_self =  c_load_self(f_parent);
    
    f_kernel_ctl->f_iflag =   (int *) c_LIC_kernel_ctl_iflag(f_kernel_ctl->f_self);
    char *f_block_name =   (char *) c_LIC_kernel_ctl_block_name(f_kernel_ctl->f_self);
    f_kernel_ctl->c_block_name = strngcopy_from_f(f_block_name);
    f_kernel_ctl->kernel_ctl_file_name = ctl_file_name;
    
    f_kernel_ctl->f_kernel_type_ctl =       init_f_ctl_chara_item(c_LIC_kernel_type_ctl,
                                                                  f_kernel_ctl->f_self);
    f_kernel_ctl->f_kernel_resolution_ctl = init_f_ctl_int_item(c_LIC_kernel_resolution_ctl,
                                                                f_kernel_ctl->f_self);
    f_kernel_ctl->f_kernel_peak_ctl =       init_f_ctl_real_item(c_LIC_kernel_peak_ctl,
                                                                 f_kernel_ctl->f_self);
    f_kernel_ctl->f_kernel_sigma_ctl =      init_f_ctl_real_item(c_LIC_kernel_kernel_sigma_ctl,
                                                                 f_kernel_ctl->f_self);
    f_kernel_ctl->f_trace_length_mode_ctl = init_f_ctl_chara_item(c_LIC_kernel_trace_len_mod_ctl,
                                                                  f_kernel_ctl->f_self);
    f_kernel_ctl->f_half_length_ctl =       init_f_ctl_real_item(c_LIC_kernel_half_length_ctl, 
                                                                 f_kernel_ctl->f_self);
    f_kernel_ctl->f_max_trace_count_ctl =   init_f_ctl_int_item(c_LIC_kernel_max_trace_cnt_ctl, 
                                                                f_kernel_ctl->f_self);
    return f_kernel_ctl;
}

void dealloc_f_LIC_kernel_ctl(struct f_LIC_kernel_ctl *f_kernel_ctl){
    dealloc_chara_ctl_item_c(f_kernel_ctl->f_kernel_type_ctl);
    dealloc_int_ctl_item_c(f_kernel_ctl->f_kernel_resolution_ctl);
    dealloc_real_ctl_item_c(f_kernel_ctl->f_kernel_peak_ctl);
    dealloc_real_ctl_item_c(f_kernel_ctl->f_kernel_sigma_ctl);
    dealloc_chara_ctl_item_c(f_kernel_ctl->f_trace_length_mode_ctl);
    dealloc_real_ctl_item_c(f_kernel_ctl->f_half_length_ctl);
    dealloc_int_ctl_item_c(f_kernel_ctl->f_max_trace_count_ctl);
    
    free(f_kernel_ctl->kernel_ctl_file_name);
    free(f_kernel_ctl->c_block_name);
    f_kernel_ctl->f_iflag = NULL;
    f_kernel_ctl->f_self = NULL;
    free(f_kernel_ctl);
    return;
}

struct f_VIZ_LIC_ctl * init_f_VIZ_LIC_ctl(int idx, void *f_parent)
{
	struct f_VIZ_LIC_ctl *f_lic_lic_ctl 
			= (struct f_VIZ_LIC_ctl *) malloc(sizeof(struct f_VIZ_LIC_ctl));
	if(f_lic_lic_ctl == NULL){
		printf("malloc error for f_VIZ_LIC_ctl\n");
		exit(0);
	};
	
	f_lic_lic_ctl->f_self =  c_lic_render_ctls_lic_ctl(idx, f_parent);
	
	f_lic_lic_ctl->f_iflag =   (int *) c_VIZ_LIC_ctl_iflag(f_lic_lic_ctl->f_self);
	char *f_block_name =   (char *) c_VIZ_LIC_ctl_block_name(f_lic_lic_ctl->f_self);
	f_lic_lic_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_block_name = (char *) c_VIZ_LIC_fname_noise_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_noise_ctl = init_f_LIC_noise_ctl(strngcopy_from_f(f_block_name), 
                                                      c_VIZ_LIC_noise_ctl,
                                                      f_lic_lic_ctl->f_self);
    
    f_block_name = (char *) c_VIZ_LIC_fname_kernel_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_kernel_ctl = init_f_LIC_kernel_ctl(strngcopy_from_f(f_block_name), 
                                                        c_VIZ_LIC_kernel_ctl, 
                                                        f_lic_lic_ctl->f_self);
    
    f_block_name = (char *) c_VIZ_LIC_fname_vol_repart_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_repart_ctl = init_f_VIZ_repartition_ctl(strngcopy_from_f(f_block_name),
                                                             c_VIZ_LIC_repartition_ctl,
                                                             f_lic_lic_ctl->f_self);
    
    void *f_mul_maksing = c_VIZ_LIC_mul_masking_ctl(f_lic_lic_ctl->f_self);
    f_block_name = c_VIZ_multi_masking_ctl_name(f_mul_maksing);
    f_lic_lic_ctl->f_mask_ctl = init_void_clist(strngcopy_from_f(f_block_name));
	f_lic_lic_ctl->f_mask_ctl->f_parent = f_mul_maksing;
    int i;
    for(i=0;i<c_VIZ_num_multi_masking_ctl(f_mul_maksing);i++){
        void *f_ctl_tmp = init_f_LIC_masking_ctl(i, f_mul_maksing);
		append_void_clist((void *) f_ctl_tmp, f_lic_lic_ctl->f_mask_ctl);
    };
    
    f_lic_lic_ctl->f_LIC_field_ctl = init_f_ctl_chara_item(c_VIZ_LIC_LIC_field_ctl,
                                                           f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_subdomain_elapsed_dump_ctl = init_f_ctl_chara_item(c_VIZ_LIC_pe_elapsed_dump_ctl,
                                                                        f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_color_field_ctl =       init_f_ctl_chara_item(c_VIZ_LIC_color_field_ctl,
                                                                   f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_color_component_ctl =   init_f_ctl_chara_item(c_VIZ_LIC_color_comp_ctl,
                                                                   f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_opacity_field_ctl =     init_f_ctl_chara_item(c_VIZ_LIC_opacity_field_ctl,
                                                                   f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_opacity_component_ctl = init_f_ctl_chara_item(c_VIZ_LIC_opacity_comp_ctl,
                                                                   f_lic_lic_ctl->f_self);
    
    f_lic_lic_ctl->f_vr_sample_mode_ctl = init_f_ctl_chara_item(c_VIZ_LIC_vr_sample_mode_ctl,
                                                                f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_step_size_ctl =      init_f_ctl_real_item(c_VIZ_LIC_step_size_ctl,
                                                               f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_normalization_type_ctl =   init_f_ctl_chara_item(c_VIZ_LIC_normalize_type_ctl,
                                                                      f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_normalization_value_ctl = init_f_ctl_real_item(c_VIZ_LIC_normalize_value_ctl,
                                                                    f_lic_lic_ctl->f_self);
    return f_lic_lic_ctl;
};

struct f_VIZ_LIC_ctl * dealloc_f_VIZ_LIC_ctl(void *void_in)
{
    struct f_VIZ_LIC_ctl *f_lic_lic_ctl = (struct f_VIZ_LIC_ctl *) void_in;
    
    f_lic_lic_ctl->f_self = NULL;
	free(f_lic_lic_ctl->c_block_name);
    
    dealloc_f_LIC_noise_ctl(f_lic_lic_ctl->f_noise_ctl);
    dealloc_f_LIC_kernel_ctl(f_lic_lic_ctl->f_kernel_ctl);
    dealloc_f_VIZ_repartition_ctl(f_lic_lic_ctl->f_repart_ctl);
    
    int i;
    for(i=0;i<count_void_clist(f_lic_lic_ctl->f_mask_ctl);i++){
        void *f_ctl_tmp = void_clist_at_index(i, f_lic_lic_ctl->f_mask_ctl);
		dealloc_f_LIC_masking_ctl((struct f_LIC_masking_ctl *) f_ctl_tmp);
    };
    dealloc_void_clist(f_lic_lic_ctl->f_mask_ctl);
    
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_LIC_field_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_subdomain_elapsed_dump_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_color_field_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_color_component_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_opacity_field_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_opacity_component_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_vr_sample_mode_ctl);
    dealloc_real_ctl_item_c(f_lic_lic_ctl->f_step_size_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_normalization_type_ctl);
    dealloc_real_ctl_item_c(f_lic_lic_ctl->f_normalization_value_ctl);
    
    return f_lic_lic_ctl;
};

struct f_VIZ_LIC_PVR_ctl * init_f_VIZ_LIC_PVR_ctl(int idx, void *f_parent)
{
	struct f_VIZ_LIC_PVR_ctl *f_lic_ctl
			= (struct f_VIZ_LIC_PVR_ctl *) malloc(sizeof(struct f_VIZ_LIC_PVR_ctl));
	if(f_lic_ctl == NULL){
		printf("malloc error for f_VIZ_LIC_PVR_ctl\n");
		exit(0);
	};
    
    char *f_block_name = c_lic_render_ctls_fname(idx, f_parent);
    f_lic_ctl->lic_ctl_file_name =  strngcopy_from_f(f_block_name);
    f_lic_ctl->f_lic_pvr_ctl = init_f_VIZ_PVR_ctl(c_lic_render_ctls_pvr_ctl,
                                                  idx, f_parent);
    f_lic_ctl->f_lic_lic_ctl =  init_f_VIZ_LIC_ctl(idx, f_parent);
    return f_lic_ctl;
};

void dealloc_f_VIZ_LIC_PVR_ctl(struct f_VIZ_LIC_PVR_ctl *f_lic_ctl)
{
    free(f_lic_ctl->lic_ctl_file_name);
    f_lic_ctl->f_lic_pvr_ctl = dealloc_f_VIZ_PVR_ctl(f_lic_ctl->f_lic_pvr_ctl);
    f_lic_ctl->f_lic_lic_ctl =  dealloc_f_VIZ_LIC_ctl(f_lic_ctl->f_lic_lic_ctl);
    free(f_lic_ctl);
    return;
};

struct void_clist * init_f_VIZ_lic_ctls(void *f_parent, int *f_num_lic_ctl)
{
    char *f_block_name =   (char *) c_lic_render_ctls_block_name(f_parent);
	struct void_clist *f_lic_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_lic_ctls->f_parent = f_parent;
	*f_num_lic_ctl = c_lic_render_ctls_num_lic_ctl(f_lic_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_lic_ctl;i++){
        struct f_VIZ_LIC_PVR_ctl *f_ctl_tmp = init_f_VIZ_LIC_PVR_ctl(i, f_lic_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_lic_ctls);
	}
	return f_lic_ctls;
}

