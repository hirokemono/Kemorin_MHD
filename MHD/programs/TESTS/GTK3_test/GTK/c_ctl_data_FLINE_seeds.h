/*
//  c_ctl_data_FLINE_seeds.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_FLINE_H_
#define C_CTL_DATA_FLINE_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_ctl_array_int2_items_c.h"


struct f_VIZ_FLINE_seeds_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
    struct real3_clist     *f_seed_point_ctl;
    struct real3_clist     *f_seed_geological_ctl;
    struct real3_clist     *f_seed_spherical_ctl;
    struct int2_clist      *f_seed_surface_ctl;
    
    void *void_panel;
};


/* prototypes */

struct f_VIZ_FLINE_seeds_ctl * init_f_VIZ_FLINE_seeds_ctl(void *f_parent);
void dealloc_f_VIZ_FLINE_seeds_ctl(struct f_VIZ_FLINE_seeds_ctl *f_fline_seeds_ctl);

#endif /* C_CTL_DATA_FLINE_H_ */
