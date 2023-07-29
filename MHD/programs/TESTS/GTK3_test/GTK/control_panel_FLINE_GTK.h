/*
//  control_panel_FLINE_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_FLINE_GTK_H_
#define CONTROL_PANEL_FLINE_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_ctl_array_chara2_int_items_c.h"
#include "t_control_chara_int2_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "c_ctl_data_FLINE.h"
#include "control_block_panel_GTK.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panel_chara_int_GTK.h"
#include "control_panel_chara_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_panel_real3_GTK.h"


struct FLINE_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
    
    GtkWidget * fline_area_grp_view;
    GtkWidget * f_seed_point_vws;
    GtkWidget * f_seed_surface_vws;
};


/* prototypes */

struct FLINE_GTK_widgets * init_FLINE_GTK_widgets(struct chara_int2_clist *f_field_ctl);
void dealloc_FLINE_GTK_widgets(struct FLINE_GTK_widgets *fline_vws);

struct f_VIZ_FLINE_ctl * init_f_VIZ_FLINE_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void * dealloc_f_VIZ_FLINE_ctl_GTK(void *void_in);


GtkWidget * draw_viz_each_fline_ctl_vbox(char *label_name, struct f_VIZ_FLINE_ctl *f_fline_item, 
                                         GtkWidget *window);

#endif /* CONTROL_PANEL_FLINE_GTK_H_ */
