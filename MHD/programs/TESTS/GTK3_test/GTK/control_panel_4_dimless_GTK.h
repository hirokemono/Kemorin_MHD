/*
//  control_panel_4_dimless_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef CONTROL_PANEL_4_DIMLESS_GTK_H_
#define CONTROL_PANEL_4_DIMLESS_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "tree_view_4_force_GTK.h"
#include "ctl_data_platforms_GTK.h"

struct f_MHD_dimless_control{
	void * f_self;
	int * f_iflag;
    char * c_block_name;
	
	struct chara_real_clist * f_dimess_names;
	struct dimless_views *  f_dimless_vws;
};


/* prototypes */

GtkWidget * add_dimless_selection_box(struct f_MHD_dimless_control *f_dless_ctl, 
									  struct dimless_views *f_dimless_vws, GtkWidget *window);


#endif /* CONTROL_PANEL_4_DIMLESS_GTK_H_ */
