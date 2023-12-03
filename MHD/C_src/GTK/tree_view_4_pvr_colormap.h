/*
//  tree_view_4_pvr_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef tree_view_4_pvr_colormap_h_
#define tree_view_4_pvr_colormap_h_

#include "calypso_GTK.h"
#include "t_control_chara_IO.h"
#include "t_control_real_IO.h"
#include "t_control_real2_IO.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"
#include "tree_view_real2_GTK.h"
#include "tree_view_4_colormap.h"

/* prototypes */

gboolean pvr_draw_colorabar_CB(GtkWidget *widget, cairo_t *cr, gpointer user_data);
void add_pvr_cmap_list_box(struct colormap_view *color_vws, GtkWidget *vbox);
void add_pvr_omap_list_box(struct colormap_view *color_vws, GtkWidget *vbox);
void set_pvr_color_mode_CB(GtkComboBox *combobox_cmap, gpointer user_data);


GtkWidget * add_pvr_colormap_list_box_2(struct colormap_view *color_vws);

#endif /* tree_view_4_pvr_colormap_h_ */
