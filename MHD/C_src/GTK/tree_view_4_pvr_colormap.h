/*
//  tree_view_4_pvr_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef tree_view_4_pvr_colormap_h_
#define tree_view_4_pvr_colormap_h_

#include <gtk/gtk.h>

#include "t_control_real2_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_real2_GTK.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "quicksort_c.h"
#include "set_rgb_colors_c.h"
#include "set_rgba_table_c.h"
#include "m_color_table_c.h"

struct colormap_view{
    int index_cmap;
	struct chara_ctl_item *colormap_mode_gtk;
    struct r2_clist_view *cmap_vws;
    struct r2_clist_view *opacity_vws;
	
	
    GtkWidget *scrolled_window;
};

/* prototypes */

void init_colormap_views(struct colormap_ctl_c *cmap_c, struct colormap_view *color_vws);
void add_colormp_list_box(struct colormap_view *color_vws, GtkWidget *vbox);


#endif /* tree_view_4_pvr_colormap_h_ */
