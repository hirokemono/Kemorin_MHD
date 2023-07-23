/*
//  c_control_data_pvrs.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_control_data_pvrs.h"

extern char * c_pvr_render_ctls_fname(int idx, void *f_pvr_ctls);

extern void * c_VIZ_pvr_ctl_block_name(void *f_pvr_ctls);
extern void * c_VIZ_pvr_ctl_iflag(void *f_pvr_ctls);
extern void * c_VIZ_pvr_fname_mat_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_viewmat_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_fname_pvr_light_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_light_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_fname_cmap_cbar_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_cmap_cbar_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_movie_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_quilt_c(void *f_pvr_ctls);
extern void * c_VIZ_pvr_updated_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_file_head_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_file_fmt_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_monitoring_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_streo_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_anaglyph_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_quilt_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_render_area_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_field_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_comp_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_section_ctl(void *f_pvr_ctls);
extern void * c_VIZ_pvr_isosurf_ctl(void *f_pvr_ctls);

extern void * c_PVR_quilt_img_ctl_block_name(void *f_quilt_c);
extern void * c_PVR_quilt_img_ctl_iflag(void *f_quilt_c);
extern void * c_PVR_quilt_num_column_row_ctl(void *f_quilt_c);
extern void * c_PVR_quilt_num_row_column_ctl(void *f_quilt_c);
extern void * c_PVR_quilt_mul_qmats_ctl(void *f_quilt_c);



static struct f_PVR_quilt_image_ctl * init_f_PVR_quilt_image_ctl(void *(*c_load_self)(void *f_parent),
                                                                 void *f_parent)
{
    struct f_PVR_quilt_image_ctl *f_quilt_c
            = (struct f_PVR_quilt_image_ctl *) malloc(sizeof(struct f_PVR_quilt_image_ctl));
    if(f_quilt_c == NULL){
        printf("malloc error for f_PVR_quilt_image_ctl\n");
        exit(0);
    };
    f_quilt_c->f_self =  c_load_self(f_parent);
    
    f_quilt_c->f_iflag =   (int *) c_PVR_quilt_img_ctl_iflag(f_quilt_c->f_self);
    char *f_block_name =   (char *) c_PVR_quilt_img_ctl_block_name(f_quilt_c->f_self);
    f_quilt_c->c_block_name = strngcopy_from_f(f_block_name);
    
    
    f_quilt_c->f_num_column_row_ctl = init_f_ctl_i2_item(c_PVR_quilt_num_column_row_ctl, f_quilt_c->f_self);
    f_quilt_c->f_num_row_column_ctl = init_f_ctl_i2_item(c_PVR_quilt_num_row_column_ctl, f_quilt_c->f_self);
    f_quilt_c->f_mul_qmats_c = init_f_PVR_mul_vmats_ctls(c_PVR_quilt_mul_qmats_ctl(f_quilt_c->f_self));
    return f_quilt_c;
}

static void dealloc_f_PVR_quilt_image_ctl(struct f_PVR_quilt_image_ctl *f_quilt_c){
    dealloc_f_PVR_mul_vmats_ctls(f_quilt_c->f_mul_qmats_c);
    dealloc_f_ctl_i2_item(f_quilt_c->f_num_column_row_ctl);
    dealloc_f_ctl_i2_item(f_quilt_c->f_num_row_column_ctl);
    
    free(f_quilt_c->c_block_name);
    free(f_quilt_c->f_iflag);
    f_quilt_c->f_self = NULL;
    free(f_quilt_c);
    return;
};



struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl(void *(*c_load_self)(int idx, void *f_parent),
                                          int idx, void *f_parent)
{
	struct f_VIZ_PVR_ctl *f_pvr_ctl 
			= (struct f_VIZ_PVR_ctl *) malloc(sizeof(struct f_VIZ_PVR_ctl));
	if(f_pvr_ctl == NULL){
		printf("malloc error for f_VIZ_PVR_ctl\n");
		exit(0);
	};
	
	f_pvr_ctl->f_self =  c_load_self(idx, f_parent);
	
	f_pvr_ctl->f_iflag =        (int *) c_VIZ_pvr_ctl_iflag(f_pvr_ctl->f_self);
	char *f_block_name =   (char *) c_VIZ_pvr_ctl_block_name(f_pvr_ctl->f_self);
	f_pvr_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_block_name = c_pvr_render_ctls_fname(idx, f_parent);
	f_pvr_ctl->pvr_ctl_file_name = strngcopy_from_f(f_block_name);
	
	f_block_name = (char *) c_VIZ_pvr_fname_mat_ctl(f_pvr_ctl->f_self);
    f_pvr_ctl->f_mat = init_f_VIZ_view_matrix_ctl(strngcopy_from_f(f_block_name),
                                                  c_VIZ_pvr_viewmat_ctl,
                                                  f_pvr_ctl->f_self);
    
    f_block_name = (char *) c_VIZ_pvr_fname_pvr_light_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_light =       init_f_PVR_lighting_ctl(strngcopy_from_f(f_block_name),
                                                       c_VIZ_pvr_light_ctl,
                                                       f_pvr_ctl->f_self);
    
    f_block_name =                   (char *) c_VIZ_pvr_fname_cmap_cbar_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_cmap_cbar_c = init_f_PVR_colormap_bar_ctl(strngcopy_from_f(f_block_name),
                                                           c_VIZ_pvr_cmap_cbar_ctl,
                                                           f_pvr_ctl->f_self);
	f_pvr_ctl->f_movie =             c_VIZ_pvr_movie_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_quilt_c = init_f_PVR_quilt_image_ctl(c_VIZ_pvr_quilt_c,
                                                      f_pvr_ctl->f_self);
    
	f_pvr_ctl->f_updated_ctl =       init_f_ctl_chara_item(c_VIZ_pvr_updated_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_file_head_ctl =     init_f_ctl_chara_item(c_VIZ_pvr_file_head_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_file_fmt_ctl =      init_f_ctl_chara_item(c_VIZ_pvr_file_fmt_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_monitoring_ctl =    init_f_ctl_chara_item(c_VIZ_pvr_monitoring_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_streo_ctl =         init_f_ctl_chara_item(c_VIZ_pvr_streo_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_anaglyph_ctl =      init_f_ctl_chara_item(c_VIZ_pvr_anaglyph_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_quilt_ctl =         init_f_ctl_chara_item(c_VIZ_pvr_quilt_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_render_area_c =     c_VIZ_pvr_render_area_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_pvr_field_ctl =     init_f_ctl_chara_item(c_VIZ_pvr_field_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_pvr_comp_ctl =      init_f_ctl_chara_item(c_VIZ_pvr_comp_ctl, f_pvr_ctl->f_self);
	f_pvr_ctl->f_pvr_scts_c =        c_VIZ_pvr_section_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_pvr_isos_c =        c_VIZ_pvr_isosurf_ctl(f_pvr_ctl->f_self);
	return f_pvr_ctl;
}

void *dealloc_f_VIZ_PVR_ctl(void *block_item){
    struct f_VIZ_PVR_ctl *f_pvr_ctl = (struct f_VIZ_PVR_ctl *) block_item;
    
    dealloc_modelview_ctl_c(f_pvr_ctl->f_mat);
    dealloc_lighting_ctl_c(f_pvr_ctl->f_light);
    dealloc_colormap_colorbar_ctl_c(f_pvr_ctl->f_cmap_cbar_c);
	f_pvr_ctl->f_movie =          NULL;
    dealloc_f_PVR_quilt_image_ctl(f_pvr_ctl->f_quilt_c);
    
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_updated_ctl);
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_file_head_ctl);
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_file_fmt_ctl);
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_monitoring_ctl);
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_streo_ctl);
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_anaglyph_ctl);
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_quilt_ctl);
	f_pvr_ctl->f_render_area_c =  NULL;
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_pvr_field_ctl);
	dealloc_chara_ctl_item_c(f_pvr_ctl->f_pvr_comp_ctl);
	f_pvr_ctl->f_pvr_scts_c =     NULL;
	f_pvr_ctl->f_pvr_isos_c =     NULL;
	return f_pvr_ctl;
}
