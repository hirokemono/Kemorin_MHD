/*
//  tree_view_4_pvr_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef tree_view_4_pvr_colormap_h_
#define tree_view_4_pvr_colormap_h_

#include <gtk/gtk.h>

#include "t_control_chara_IO.h"
#include "t_control_real_IO.h"
#include "t_control_real2_IO.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_real2_GTK.h"
#include "quicksort_c.h"
#include "set_rgb_colors_c.h"
#include "set_rgba_table_c.h"
#include "m_color_table_c.h"
#include "m_kemoview_psf_menu.h"
#include "set_each_psf_parameters.h"

struct colormap_view{
	struct chara_ctl_item *colormap_mode_gtk;
    struct r2_clist_view *cmap_vws;
    struct r2_clist_view *opacity_vws;

	struct colormap_params *cmap_param;	
    GtkWidget *scrolled_window;
};

/* prototypes */

void init_colormap_views_4_ctl(struct colormap_ctl_c *cmap_c, 
			struct colormap_view *color_vws);

void init_colormap_views_4_viewer(struct psf_menu_val *psf_current_menu, struct colormap_view *color_vws);
void load_color_opacity_map_from_list(struct psf_menu_val *psf_current_menu, 
			struct colormap_view *color_vws);

void dealloc_colormap_views_4_viewer(struct colormap_view *color_vws);

void add_colormp_list_box(struct colormap_view *color_vws, GtkWidget *vbox);


#endif /* tree_view_4_pvr_colormap_h_ */
