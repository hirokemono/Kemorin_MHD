/*
//  control_panel_LIC_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_LIC_GTK_H_
#define CONTROL_PANEL_LIC_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_int2_IO.h"
#include "t_control_chara2_int_IO.h"
#include "c_control_data_pvrs.h"
#include "c_ctl_data_LIC.h"
#include "control_panel_PVR_GTK.h"
#include "control_panel_VIZ_repartition_GTK.h"


struct LIC_GTK_widgets{
    struct PVR_GTK_widgets *lic_pvr_vws;
    struct VIZ_repartition_widgets *f_repart_vws;
    struct block_array_widgets *masking_Wgts;
    
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
};


/* prototypes */

struct LIC_GTK_widgets * init_LIC_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                                              struct chara_int2_clist *f_field_ctl);
void dealloc_LIC_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                             struct LIC_GTK_widgets *lic_vws);

struct f_VIZ_LIC_PVR_ctl * init_f_VIZ_LIC_PVR_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void * dealloc_f_VIZ_LIC_PVR_ctl_GTK(void *void_in);

GtkWidget * draw_viz_each_lic_ctl_vbox(char *label_name, struct f_VIZ_LIC_PVR_ctl *f_lic_item, 
                                       GtkWidget *window);

#endif /* CONTROL_PANEL_LIC_GTK_H_ */
