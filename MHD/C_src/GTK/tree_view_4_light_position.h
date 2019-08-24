/*
//  tree_view_4_light_position.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_LIGHT_POSITION_
#define TREE_VIEW_4_LIGHT_POSITION_

#include <gtk/gtk.h>

#include "t_control_real_IO.h"
#include "t_control_real3_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_real3_GTK.h"
#include "quicksort_c.h"

struct lightparams_view{
    struct r3_clist_view *light_rtp_vws;

    GtkWidget *scrolled_window;
};

/* prototypes */

void init_light_views_4_ctl(struct real3_clist *light_list, 
			struct lightparams_view *light_vws);
void init_light_views_4_viewer(struct lightparams_view *light_vws);
void dealloc_light_views_4_viewer(struct lightparams_view *light_vws);

void add_light_list_box(struct lightparams_view *light_vws, GtkWidget *vbox);


#endif /* tree_view_4_pvr_colormap_h_ */
