/*
//  c_control_data_pvrs.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_control_data_pvrs.h"

extern char * c_pvr_render_ctls_fname(int idx, void *f_pvr_ctls);
extern void * c_pvr_render_ctls_pvr_ctl(int idx, void *f_pvr_ctls);

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



struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl(int idx, void *f_parent)
{
	struct f_VIZ_PVR_ctl *f_pvr_ctl 
			= (struct f_VIZ_PVR_ctl *) malloc(sizeof(struct f_VIZ_PVR_ctl));
	if(f_pvr_ctl == NULL){
		printf("malloc error for f_VIZ_PVR_ctl\n");
		exit(0);
	};
	
	f_pvr_ctl->f_self =  c_pvr_render_ctls_pvr_ctl(idx, f_parent);
	
	f_pvr_ctl->f_iflag =        (int *) c_VIZ_pvr_ctl_iflag(f_pvr_ctl->f_self);
	char *f_block_name =   (char *) c_VIZ_pvr_ctl_block_name(f_pvr_ctl->f_self);
	f_pvr_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_block_name = c_pvr_render_ctls_fname(idx, f_parent);
	f_pvr_ctl->pvr_ctl_file_name = strngcopy_from_f(f_block_name);
	
	f_pvr_ctl->f_fname_mat_ctl =     (char *) c_VIZ_pvr_fname_mat_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_mat =               c_VIZ_pvr_viewmat_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_fname_pvr_light_c = (char *) c_VIZ_pvr_fname_pvr_light_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_light =             c_VIZ_pvr_light_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_fname_cmap_cbar_c = (char *) c_VIZ_pvr_fname_cmap_cbar_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_cmap_cbar_c =       c_VIZ_pvr_cmap_cbar_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_movie =             c_VIZ_pvr_movie_ctl(f_pvr_ctl->f_self);
	f_pvr_ctl->f_quilt_c =           c_VIZ_pvr_quilt_c(f_pvr_ctl->f_self);
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

void dealloc_f_VIZ_PVR_ctl(void *block_item){
    struct f_VIZ_PVR_ctl *f_pvr_ctl = (struct f_VIZ_PVR_ctl *) block_item;
    
	f_pvr_ctl->f_fname_mat_ctl =  NULL;
	f_pvr_ctl->f_mat =            NULL;
	f_pvr_ctl->f_fname_pvr_light_c = NULL;
	f_pvr_ctl->f_light =          NULL;
	f_pvr_ctl->f_fname_cmap_cbar_c = NULL;
	f_pvr_ctl->f_cmap_cbar_c =    NULL;
	f_pvr_ctl->f_movie =          NULL;
	f_pvr_ctl->f_quilt_c =        NULL;
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
	return;
}
