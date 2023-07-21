/*
//  c_ctl_data_MAP.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_MAP.h"
extern void * c_VIZ_MAP_ctl_block_name(void *f_map_ctl);
extern void * c_VIZ_MAP_ctl_iflag(void *f_map_ctl);
extern void * c_VIZ_MAP_map_define_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_fname_mat_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_viewmat_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_fname_cmap_cbar_c(void *f_map_ctl);
extern void * c_VIZ_MAP_cmap_cbar_c(void *f_map_ctl);
extern void * c_VIZ_MAP_map_image_prefix_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_map_image_fmt_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_map_field_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_map_comp_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_isoline_field_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_isoline_comp_ctl(void *f_map_ctl);

extern void * c_MAP_section_ctl_block_name(void *f_map_define_ctl);
extern void * c_MAP_section_ctl_iflag(void *f_map_define_ctl);
extern void * c_MAP_section_fname_sect_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_psf_def_c(void *f_map_define_ctl);
extern void * c_MAP_section_zeroline_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_iso_color_mode(void *f_map_define_ctl);
extern void * c_MAP_section_iso_number_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_iso_range_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_iso_width_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_grid_width_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_tan_cyl_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_tan_cyl_in_ctl(void *f_map_define_ctl);
extern void * c_MAP_section_tan_cyl_out_ctl(void *f_map_define_ctl);

extern void * c_map_render_ctls_block_name(void *f_map_ctls);
extern int    c_map_render_ctls_num_map_ctl(void *f_map_ctls);
extern char * c_map_render_ctls_fname(int idx, void *f_map_ctls);
extern void * c_map_render_ctls_map_ctl(int idx, void *f_map_ctls);

static struct f_MAP_section_ctl * init_f_MAP_section_ctl(void *(*c_load_self)(void *f_parent), 
                                                  void *f_parent)
{
	struct f_MAP_section_ctl *f_map_define_ctl 
			= (struct f_MAP_section_ctl *) malloc(sizeof(struct f_MAP_section_ctl));
	if(f_map_define_ctl == NULL){
		printf("malloc error for f_MAP_section_ctl\n");
		exit(0);
	};
	f_map_define_ctl->f_self =  c_load_self(f_parent);
	
	f_map_define_ctl->f_iflag =   (int *) c_MAP_section_ctl_iflag(f_map_define_ctl->f_self);
	char *f_block_name =   (char *) c_MAP_section_ctl_block_name(f_map_define_ctl->f_self);
	f_map_define_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_block_name = c_MAP_section_fname_sect_ctl(f_map_define_ctl->f_self);
    f_map_define_ctl->f_psf_def_c =  init_f_VIZ_PSF_def_ctl(strngcopy_from_f(f_block_name), 
                                                            c_MAP_section_psf_def_c, 
                                                            f_map_define_ctl->f_self);
    
    f_map_define_ctl->f_zeroline_switch_ctl = init_f_ctl_chara_item(c_MAP_section_zeroline_ctl,
                                                                    f_map_define_ctl->f_self);
    f_map_define_ctl->f_isoline_color_mode = init_f_ctl_chara_item(c_MAP_section_iso_color_mode,
                                                                   f_map_define_ctl->f_self);
    f_map_define_ctl->f_isoline_number_ctl = init_f_ctl_int_item(c_MAP_section_iso_number_ctl,
                                                                 f_map_define_ctl->f_self);
    f_map_define_ctl->f_isoline_width_ctl =  init_f_ctl_real_item(c_MAP_section_iso_width_ctl,
                                                                  f_map_define_ctl->f_self);
    f_map_define_ctl->f_isoline_range_ctl =  init_f_ctl_r2_item(c_MAP_section_iso_range_ctl,
                                                                f_map_define_ctl->f_self);
    f_map_define_ctl->f_grid_width_ctl =     init_f_ctl_real_item(c_MAP_section_grid_width_ctl,
                                                                  f_map_define_ctl->f_self);
    f_map_define_ctl->f_tan_cyl_switch_ctl
            = init_f_ctl_chara_item(c_MAP_section_tan_cyl_ctl, f_map_define_ctl->f_self);
    f_map_define_ctl->f_tangent_cylinder_inner_ctl
            = init_f_ctl_real_item(c_MAP_section_tan_cyl_in_ctl, f_map_define_ctl->f_self);
    f_map_define_ctl->f_tangent_cylinder_outer_ctl
            = init_f_ctl_real_item(c_MAP_section_tan_cyl_out_ctl, f_map_define_ctl->f_self);
    return f_map_define_ctl;
};

static void dealloc_f_MAP_section_ctl(struct f_MAP_section_ctl *f_map_define_ctl){
    dealloc_f_VIZ_PSF_def_ctl(f_map_define_ctl->f_psf_def_c);
    
    dealloc_chara_ctl_item_c(f_map_define_ctl->f_zeroline_switch_ctl);
    dealloc_chara_ctl_item_c(f_map_define_ctl->f_isoline_color_mode);
    dealloc_int_ctl_item_c(f_map_define_ctl->f_isoline_number_ctl);
    dealloc_real2_ctl_item_c(f_map_define_ctl->f_isoline_range_ctl);
    dealloc_real_ctl_item_c(f_map_define_ctl->f_isoline_width_ctl);
    dealloc_real_ctl_item_c(f_map_define_ctl->f_grid_width_ctl);
    dealloc_chara_ctl_item_c(f_map_define_ctl->f_tan_cyl_switch_ctl);
    dealloc_real_ctl_item_c(f_map_define_ctl->f_tangent_cylinder_inner_ctl);
    dealloc_real_ctl_item_c(f_map_define_ctl->f_tangent_cylinder_outer_ctl);
    
    free(f_map_define_ctl->c_block_name);
    free(f_map_define_ctl->f_iflag);
    f_map_define_ctl->f_self = NULL;
    free(f_map_define_ctl);
    return;
}

struct f_VIZ_MAP_ctl * init_f_VIZ_MAP_ctl(int idx, void *f_parent)
{
	struct f_VIZ_MAP_ctl *f_map_ctl 
			= (struct f_VIZ_MAP_ctl *) malloc(sizeof(struct f_VIZ_MAP_ctl));
	if(f_map_ctl == NULL){
		printf("malloc error for f_VIZ_MAP_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_map_render_ctls_fname(idx, f_parent);
    f_map_ctl->map_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_map_ctl->f_self =  c_map_render_ctls_map_ctl(idx, f_parent);
	
	f_map_ctl->f_iflag =   (int *) c_VIZ_MAP_ctl_iflag(f_map_ctl->f_self);
	f_block_name =   (char *) c_VIZ_MAP_ctl_block_name(f_map_ctl->f_self);
	f_map_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_map_ctl->f_map_define_ctl = init_f_MAP_section_ctl(c_VIZ_MAP_map_define_ctl, f_map_ctl->f_self);
    
    f_block_name = c_VIZ_MAP_fname_mat_ctl(f_map_ctl->f_self);
    f_map_ctl->f_mat = init_f_VIZ_view_matrix_ctl(strngcopy_from_f(f_block_name), 
                                                  c_VIZ_MAP_viewmat_ctl, f_map_ctl->f_self);
    
    f_block_name = c_VIZ_MAP_fname_cmap_cbar_c(f_map_ctl->f_self);
    f_map_ctl->f_cmap_cbar_c =  init_f_PVR_colormap_bar_ctl(f_block_name, 
                                                            c_VIZ_MAP_cmap_cbar_c, 
                                                            f_map_ctl->f_self);
    
    f_map_ctl->f_map_image_prefix_ctl = init_f_ctl_chara_item(c_VIZ_MAP_map_image_prefix_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_map_image_fmt_ctl =    init_f_ctl_chara_item(c_VIZ_MAP_map_image_fmt_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_map_field_ctl =        init_f_ctl_chara_item(c_VIZ_MAP_map_field_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_map_comp_ctl =         init_f_ctl_chara_item(c_VIZ_MAP_map_comp_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_isoline_field_ctl =    init_f_ctl_chara_item(c_VIZ_MAP_isoline_field_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_isoline_comp_ctl =     init_f_ctl_chara_item(c_VIZ_MAP_isoline_comp_ctl,
                                                              f_map_ctl->f_self);
    return f_map_ctl;
};

void dealloc_f_VIZ_MAP_ctl(struct f_VIZ_MAP_ctl *f_map_ctl){
    
    free(f_map_ctl->c_block_name);
    free(f_map_ctl->map_ctl_file_name);
    f_map_ctl->f_self = NULL;
    
    dealloc_f_MAP_section_ctl(f_map_ctl->f_map_define_ctl);
    dealloc_modelview_ctl_c(f_map_ctl->f_mat);
    dealloc_colormap_colorbar_ctl_c(f_map_ctl->f_cmap_cbar_c);
    
    dealloc_chara_ctl_item_c(f_map_ctl->f_map_image_prefix_ctl);
    dealloc_chara_ctl_item_c(f_map_ctl->f_map_image_fmt_ctl);
    dealloc_chara_ctl_item_c(f_map_ctl->f_map_field_ctl);
    dealloc_chara_ctl_item_c(f_map_ctl->f_map_comp_ctl);
    dealloc_chara_ctl_item_c(f_map_ctl->f_isoline_field_ctl);
    dealloc_chara_ctl_item_c(f_map_ctl->f_isoline_comp_ctl);
    free(f_map_ctl);
    return;
};


struct void_clist * init_f_VIZ_map_ctls(void *f_parent, int *f_num_map_ctl)
{
    char *f_block_name =   (char *) c_map_render_ctls_block_name(f_parent);
	struct void_clist *f_map_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_map_ctls->f_parent = f_parent;
	*f_num_map_ctl = c_map_render_ctls_num_map_ctl(f_map_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_map_ctl;i++){
        struct f_VIZ_MAP_ctl *f_ctl_tmp = init_f_VIZ_MAP_ctl(i, f_map_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_map_ctls);
	}
	return f_map_ctls;
}

