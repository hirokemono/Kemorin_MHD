/*
//  control_panel_4_MHD_evo_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef CONTROL_PANEL_4_MHD_EVO_GTK_H_
#define CONTROL_PANEL_4_MHD_EVO_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "c_ctl_data_platforms.h"
#include "tree_view_4_force_GTK.h"
#include "control_boxes_single_items_GTK.h"


struct f_MHD_time_evo_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_clist *f_t_evo_field_ctl;
};


/* prototypes */

struct f_MHD_time_evo_control * init_f_MHD_time_evo_control(void *(*c_load_self)(void *f_parent), 
                                                            void *f_parent);

GtkWidget * add_MHD_evo_selection_box(struct f_MHD_time_evo_control *f_evo_ctl,
                                      struct chara_clist *bc_types, 
                                      struct boundary_condition_view *bc_vws,
                                      GtkWidget *window){


#endif /* CONTROL_PANEL_4_MHD_EVO_GTK_H_ */
