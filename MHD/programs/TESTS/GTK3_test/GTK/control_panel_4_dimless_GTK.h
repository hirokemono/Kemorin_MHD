/*
//  control_panel_4_dimless_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef control_panel_4_dimless_GTK_h_
#define control_panel_4_dimless_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "tree_view_4_force_GTK.h"
#include "ctl_data_platforms_GTK.h"

struct f_MHD_dimless_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	struct f_ctl_cr_array * f_dimess_names;
	struct dimless_views *  f_dimless_vws;
};


/* prototypes */

struct f_MHD_dimless_control * init_f_MHD_dimless_ctl(void *(*c_load_self)(void *f_parent),
													  void *f_parent);
GtkWidget * add_dimless_selection_box(struct f_MHD_dimless_control *f_dless_ctl, GtkWidget *window);


#endif /* control_panel_4_dimless_GTK_h */
