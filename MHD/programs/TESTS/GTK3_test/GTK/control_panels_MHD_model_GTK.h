//
//  control_panels_MHD_model_GTK.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/1/23.
//

#ifndef CONTROL_PANELS_MHD_MODEL_GTK_H
#define CONTROL_PANELS_MHD_MODEL_GTK_H

#include <stdio.h>
#include <gtk/gtk.h>

#include "c_ctl_data_MHD_model.h"
#include "ctl_data_platforms_GTK.h"
#include "tree_view_4_field_GTK.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panel_cbox_GTK.h"
#include "control_panel_cbox_real_GTK.h"
#include "control_panel_chara_real_GTK.h"
#include "control_panel_4_field_GTK.h"
#include "control_panel_4_SGS_model_GTK.h"
#include "control_panel_4_MHD_BCs_GTK.h"
#include "control_panel_4_dimless_GTK.h"

struct MHD_model_widgets{
	struct f_MHD_equations_views *f_eqs_vws;
    struct dimless_views * f_t_evo_vws;
    struct dimless_views * f_dimless_vws;
    
	struct f_MHD_BCs_tree_views *bc_nod_bc_vws;
 	struct f_MHD_BCs_tree_views *bc_surf_bc_vws;
    struct f_SGS_model_widgets *SGSWgts;
    GtkWidget * f_magnetic_scale_tree;

	GtkWidget *f_fluid_area_tree_view;
	GtkWidget *f_conduct_area_tree_view;
    
    struct chara_cbox_table_view * time_evo_vws;
    struct chara_cbox_table_view * force_vws;
    struct chara_cbox_table_view * ex_magne_vws;
    struct chara_cbox_table_view * mag_scale_vws;
    struct chara_cbox_table_view * gravity_vws;
    
    struct chara_clist *label_time_evo_list;
    struct chara_clist *label_force_list;
    struct chara_clist *label_reftemp_list;
    struct chara2_int_clist *label_xyz_dir_list;
};

/* prototypes */
extern void * c_link_xyz_dir_list_to_ctl(void);
extern void * c_link_xyzw_dir_list_to_ctl(void);

GtkWidget *MHD_model_ctl_expander(void *f_parent, struct f_MHD_model_control *f_model_ctl,
                                  struct MHD_model_widgets *model_wgts, GtkWidget *window);

#endif /* CONTROL_PANELS_MHD_MODEL_GTK_H */
