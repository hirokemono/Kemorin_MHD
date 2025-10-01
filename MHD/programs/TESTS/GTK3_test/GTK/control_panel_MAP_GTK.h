/*
//  control_panel_MAP_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_MAP_GTK_H_
#define CONTROL_PANEL_MAP_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_ctl_data_4_fields_c.h"
#include "c_ctl_data_MAP.h"
#include "tree_view_4_colormap.h"
#include "control_panel_PSF_ISO_GTK.h"
#include "control_panel_PVR_colormap.h"
#include "control_panel_PVR_viewmatrix_GTK.h"

struct MAP_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
    struct colormap_view *color_vws;
    
    struct PSF_GTK_widgets *psf_def_vws;
    GtkWidget * map_area_view;
};


/* prototypes */

struct MAP_GTK_widgets * init_MAP_GTK_widgets(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                                              struct chara_int2_clist *f_field_ctl);
void dealloc_MAP_GTK_widgets(struct MAP_GTK_widgets *map_vws);

struct f_VIZ_MAP_ctl * init_f_VIZ_MAP_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void *dealloc_f_VIZ_MAP_ctl_GTK(void *void_in);

GtkWidget * draw_viz_each_map_ctl_vbox(char *label_name, struct f_VIZ_MAP_ctl *f_map_item,
                                       GtkWidget *window);

#endif /* CONTROL_PANEL_MAP_GTK_H_ */
