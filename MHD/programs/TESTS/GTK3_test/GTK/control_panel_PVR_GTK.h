/*
//  control_panel_PVR_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_PVR_GTK_H_
#define CONTROL_PANEL_PVR_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara2_int_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "c_control_data_pvrs.h"
#include "c_ctl_data_MAP.h"
#include "tree_view_4_colormap.h"
#include "control_block_panel_GTK.h"
#include "control_panel_text_text_real_GTK.h"
#include "control_panel_PVR_colormap.h"
#include "control_panel_PSF_ISO_GTK.h"
#include "control_panel_PVR_viewmatrix_GTK.h"


struct pvr_area_widgets{
    GtkWidget *f_pvr_area_tree;
    GtkWidget *f_surf_enhanse_tree;
};

struct PVR_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
    struct colormap_view *color_vws;
    
    struct block_array_widgets *pvr_psfs_Wgts;
    struct block_array_widgets *pvr_isos_Wgts;
    struct block_array_widgets *pvr_qmat_Wgts;
    struct block_array_widgets *pvr_mmat_Wgts;
    struct pvr_area_widgets * pvr_areaWgts;
    
    GtkWidget * lighting_tree_view;
};


/* prototypes */

struct PVR_GTK_widgets * init_PVR_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                                              struct chara_int2_clist *f_field_ctl);
void dealloc_PVR_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                             struct PVR_GTK_widgets *pvr_vws);

struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void * dealloc_f_VIZ_PVR_ctl_GTK(void *void_in);

GtkWidget * draw_viz_each_pvr_ctl_vbox(char *label_name, struct f_VIZ_PVR_ctl *f_pvr_item, 
                                       GtkWidget *window);


#endif /* CONTROL_PANEL_PVR_GTK_H_ */
