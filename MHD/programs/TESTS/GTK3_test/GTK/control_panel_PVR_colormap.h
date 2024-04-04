/*
//  control_panel_PVR_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_PVR_COLORMAP_H_
#define CONTROL_PANEL_PVR_COLORMAP_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "control_panel_fld_on_psf_GTK.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panel_real3_GTK.h"
#include "tree_view_4_colormap.h"
#include "tree_view_4_pvr_colormap.h"


/* prototypes */

void append_viz_cmap_cbar_vbox(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                               struct colormap_view *color_vws, 
                               struct chara_int2_clist *label_field_list, 
                               struct chara2_int_clist *label_dir_list,
                               GtkWidget *window, GtkWidget *vbox_cc);
GtkWidget * draw_pvr_lighting_vbox(struct lighting_ctl_c *light_ctl_c,
                                   GtkWidget *lighting_tree_view, GtkWidget *window);

#endif /* CONTROL_PANEL_PVR_COLORMAP_H_ */
