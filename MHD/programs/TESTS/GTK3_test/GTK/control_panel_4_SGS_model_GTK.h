/*
//  control_panel_4_SGS_model_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef CONTROL_PANEL_4_SGS_MODEL_GTK_H_
#define CONTROL_PANEL_4_SGS_MODEL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "c_ctl_data_SGS_model.h"
#include "control_block_panel_GTK.h"
#include "control_panel_chara_GTK.h"
#include "control_panel_int_GTK.h"
#include "control_boxes_single_items_GTK.h"

struct f_SGS_model_widgets{
    GtkWidget *f_igrp_stack_layer_tree;
	GtkWidget *f_layer_grp_name_tree;
	GtkWidget *f_whole_filter_grp_tree;
	GtkWidget *f_fluid_filter_grp_tree;
	GtkWidget *f_SGS_terms_tree_view;
	GtkWidget *f_commutate_fld_trww_view;
	struct block_array_widgets * sfilter_Widgets;
};

/* prototypes */

GtkWidget *SGS_MHD_model_expander(GtkWidget *window, struct f_MHD_SGS_model_control *f_sgs_ctl, 
                                  struct f_SGS_model_widgets *SGSWgts);


#endif /* CONTROL_PANEL_4_SGS_MODEL_GTK_H_ */
