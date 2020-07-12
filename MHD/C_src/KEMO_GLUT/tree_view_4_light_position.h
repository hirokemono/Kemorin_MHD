/*
//  tree_view_4_light_position.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_LIGHT_POSITION_
#define TREE_VIEW_4_LIGHT_POSITION_

#include "calypso_GTK.h"
#include "t_control_real_IO.h"
#include "t_control_real3_IO.h"
#include "m_phong_light_table_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_real3_GTK.h"
#include "quicksort_c.h"

struct lightparams_view{
    struct r3_clist_view *light_rtp_vws;
	struct phong_lights *lights_gtk;
	
    GtkWidget *scrolled_window;
};

/* prototypes */

struct lightparams_view * init_light_views_4_ctl(struct real3_clist *light_list);
struct lightparams_view * init_light_views_4_viewer(struct phong_lights *lights);
void dealloc_light_views_4_viewer(struct lightparams_view *light_vws);

GtkWidget * init_light_list_frame(struct lightparams_view *light_vws);


#endif /* tree_view_4_pvr_colormap_h_ */
