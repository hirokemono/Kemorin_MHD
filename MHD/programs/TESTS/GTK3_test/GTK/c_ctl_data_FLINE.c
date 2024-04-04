/*
//  c_ctl_data_FLINE.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_FLINE.h"

extern void * c_fline_ctls_block_name(void *f_fline_ctls);
extern int    c_fline_ctls_num_fline_ctl(void *f_fline_ctls);
extern char * c_fline_ctls_fname(int idx, void *f_fline_ctls);
extern void * c_fline_ctls_fline_ctl(int idx, void *f_fline_ctls);

extern void * c_VIZ_FLINE_ctl_block_name(void *f_fline_ctl);
extern void * c_VIZ_FLINE_ctl_iflag(void *f_fline_ctl);
extern void * c_VIZ_FLINE_file_head_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_output_type_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_field_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_color_field_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_color_comp_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_area_grp_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_starting_type_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_selection_type_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_line_direction_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_start_surf_grp_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_num_fieldline_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_max_line_step_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_seed_point_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_seed_surface_ctl(void *f_fline_ctl);


struct f_VIZ_FLINE_ctl * init_f_VIZ_FLINE_ctl(int idx, void *f_parent)
{
	struct f_VIZ_FLINE_ctl *f_fline_ctl 
			= (struct f_VIZ_FLINE_ctl *) malloc(sizeof(struct f_VIZ_FLINE_ctl));
	if(f_fline_ctl == NULL){
		printf("malloc error for f_VIZ_FLINE_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_fline_ctls_fname(idx, f_parent);
    f_fline_ctl->fline_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_fline_ctl->f_self =  c_fline_ctls_fline_ctl(idx, f_parent);
	
	f_fline_ctl->f_iflag =   (int *) c_VIZ_FLINE_ctl_iflag(f_fline_ctl->f_self);
	f_block_name =   (char *) c_VIZ_FLINE_ctl_block_name(f_fline_ctl->f_self);
	f_fline_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_fline_ctl->f_fline_file_head_ctl =   init_f_ctl_chara_item(c_VIZ_FLINE_file_head_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_output_type_ctl = init_f_ctl_chara_item(c_VIZ_FLINE_output_type_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_field_ctl =       init_f_ctl_chara_item(c_VIZ_FLINE_field_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_color_field_ctl = init_f_ctl_chara_item(c_VIZ_FLINE_color_field_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_color_comp_ctl =  init_f_ctl_chara_item(c_VIZ_FLINE_color_comp_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_area_grp_ctl =    init_f_ctl_chara_array(c_VIZ_FLINE_area_grp_ctl,
																  f_fline_ctl->f_self);
    f_fline_ctl->f_starting_type_ctl =     init_f_ctl_chara_item(c_VIZ_FLINE_starting_type_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_selection_type_ctl =    init_f_ctl_chara_item(c_VIZ_FLINE_selection_type_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_line_direction_ctl =    init_f_ctl_chara_item(c_VIZ_FLINE_line_direction_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_start_surf_grp_ctl =    init_f_ctl_chara_item(c_VIZ_FLINE_start_surf_grp_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_num_fieldline_ctl =     init_f_ctl_int_item(c_VIZ_FLINE_num_fieldline_ctl,
															   f_fline_ctl->f_self);
    f_fline_ctl->f_max_line_stepping_ctl = init_f_ctl_int_item(c_VIZ_FLINE_max_line_step_ctl,
															   f_fline_ctl->f_self);
    f_fline_ctl->f_seed_point_ctl =        init_f_ctl_r3_array(c_VIZ_FLINE_seed_point_ctl,
															   f_fline_ctl->f_self);
    f_fline_ctl->f_seed_surface_ctl =      init_f_ctl_i2_array(c_VIZ_FLINE_seed_surface_ctl,
															   f_fline_ctl->f_self);
	return f_fline_ctl;
}

void dealloc_f_VIZ_FLINE_ctl(struct f_VIZ_FLINE_ctl *f_fline_ctl)
{
	f_fline_ctl->f_self = NULL;
    free(f_fline_ctl->fline_ctl_file_name);
	free(f_fline_ctl->c_block_name);
    
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_file_head_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_output_type_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_field_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_color_field_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_color_comp_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_starting_type_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_selection_type_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_line_direction_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_start_surf_grp_ctl);
    dealloc_int_ctl_item_c(f_fline_ctl->f_num_fieldline_ctl);
    dealloc_int_ctl_item_c(f_fline_ctl->f_max_line_stepping_ctl);
    dealloc_chara_clist(f_fline_ctl->f_fline_area_grp_ctl);
    dealloc_real3_clist(f_fline_ctl->f_seed_point_ctl);
    dealloc_int2_clist(f_fline_ctl->f_seed_surface_ctl);
    free(f_fline_ctl);
	return;
}

struct void_clist * init_f_VIZ_fline_ctls(void *f_parent, int *f_num_fline_ctl)
{
    char *f_block_name =   (char *) c_fline_ctls_block_name(f_parent);
	struct void_clist *f_fline_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_fline_ctls->f_parent = f_parent;
	*f_num_fline_ctl = c_fline_ctls_num_fline_ctl(f_fline_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_fline_ctl;i++){
		struct f_VIZ_FLINE_ctl *f_ctl_tmp = init_f_VIZ_FLINE_ctl(i, f_fline_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_fline_ctls);
	}
	return f_fline_ctls;
}

