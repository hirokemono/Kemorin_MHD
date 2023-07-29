/*
//  ctl_panel_SPH_MHD_model_GTK.h
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#ifndef ctl_panel_SPH_MHD_model_GTK__
#define ctl_panel_SPH_MHD_model_GTK__

#include "calypso_GTK.h"
#include "t_ctl_data_SGS_MHD_model_c.h"
#include "t_ctl_data_4_fields_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "control_elements_IO_GTK.h"
#include "control_panel_4_field_GTK.h"
#include "tree_view_4_field_GTK.h"

/* prototypes */

GtkWidget *make_mhd_model_ctl_hbox(const char *label_hd, struct mhd_model_control_c *model_ctl,
                                   GtkWidget *window);
GtkWidget *make_mhd_control_ctl_hbox(const char *label_hd, 
			struct sph_mhd_control_control_c *control_ctl);

#endif /* ctl_panel_SPH_MHD_model_GTK__ */
