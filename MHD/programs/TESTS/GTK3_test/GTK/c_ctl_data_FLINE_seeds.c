/*
//  c_ctl_data_FLINE_seeds.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_FLINE_seeds.h"

extern void * c_VIZ_FLINE_seeds_ctl_blk_name(void *f_fline_ctls);
extern void * c_VIZ_FLINE_seeds_lists_ctl(void *f_fline_ctls);

extern void * c_VIZ_FLINE_seed_point_ctl(void *f_fline_seeds_ctl);
extern void * c_VIZ_FLINE_geological_pnt_ctl(void *f_fline_seeds_ctl);
extern void * c_VIZ_FLINE_spherical_pnt_ctl(void *f_fline_seeds_ctl);
extern void * c_VIZ_FLINE_seed_surface_ctl(void *f_fline_seeds_ctl);


struct f_VIZ_FLINE_seeds_ctl * init_f_VIZ_FLINE_seeds_ctl(void *f_parent)
{
    struct f_VIZ_FLINE_seeds_ctl *f_fline_seeds_ctl 
            = (struct f_VIZ_FLINE_seeds_ctl *) malloc(sizeof(struct f_VIZ_FLINE_seeds_ctl));
    if(f_fline_seeds_ctl == NULL){
        printf("malloc error for f_VIZ_FLINE_seeds_ctl\n");
        exit(0);
    };
    
	f_fline_seeds_ctl->f_self =  c_VIZ_FLINE_seeds_lists_ctl(f_parent);
	
	f_fline_seeds_ctl->f_iflag =   (int *) c_VIZ_FLINE_ctl_iflag(f_fline_seeds_ctl->f_self);
	char *f_block_name =   (char *) c_VIZ_FLINE_seeds_ctl_blk_name(f_fline_seeds_ctl->f_self);
	f_fline_seeds_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_fline_seeds_ctl->f_seed_point_ctl =      init_f_ctl_r3_array(c_VIZ_FLINE_seed_point_ctl,
                                                                   f_fline_seeds_ctl->f_self);
    f_fline_seeds_ctl->f_seed_geological_ctl = init_f_ctl_r3_array(c_VIZ_FLINE_seed_point_ctl,
                                                                   f_fline_seeds_ctl->f_self);
    f_fline_seeds_ctl->f_seed_spherical_ctl =  init_f_ctl_r3_array(c_VIZ_FLINE_seed_point_ctl,
                                                                   f_fline_seeds_ctl->f_self);
    f_fline_seeds_ctl->f_seed_surface_ctl =    init_f_ctl_i2_array(c_VIZ_FLINE_seed_surface_ctl,
                                                                   f_fline_seeds_ctl->f_self);
    return f_fline_seeds_ctl;
}

void dealloc_f_VIZ_FLINE_ctl(struct f_VIZ_FLINE_seeds_ctl *f_fline_seeds_ctl)
{
	f_fline_seeds_ctl->f_self = NULL;
	free(f_fline_seeds_ctl->c_block_name);
    
    dealloc_real3_clist(f_fline_seeds_ctl->f_seed_point_ctl);
    dealloc_real3_clist(f_fline_seeds_ctl->f_seed_geological_ctl);
    dealloc_real3_clist(f_fline_seeds_ctl->f_seed_spherical_ctl);
    dealloc_int2_clist(f_fline_seeds_ctl->f_seed_surface_ctl);
    free(f_fline_seeds_ctl);
	return;
}
