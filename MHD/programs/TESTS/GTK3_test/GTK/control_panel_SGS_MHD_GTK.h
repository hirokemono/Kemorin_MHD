/*
//  control_panel_SGS_MHD_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_SGS_MHD_GTK_H_
#define CONTROL_PANEL_SGS_MHD_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_IO.h"
#include "ctl_data_platforms_GTK.h"
#include "control_panel_4_sph_monitor_GTK.h"
#include "control_panel_VIZ_repartition_GTK.h"
#include "control_panel_VIZs_GTK.h"


struct main_widgets{
	GtkWidget *main_Vbox;
	GtkWidget *open_Hbox;
	GtkWidget *ctl_MHD_Vbox;
    GtkWidget *ctl_MHD_inner_box;
    struct VIZ_repartition_widgets *f_repart_vws;
	
	struct f_sph_shell_views *f_psph_vws;
    
	struct f_sph_monitor_widgets *f_lp_vws;
    
    struct MHD_model_widgets *model_wgts;
    struct VIZs_widgets        *vizs_Wgts;
    struct dynamo_VIZs_widgets *dviz_Wgts;
    
    struct chara_clist *label_file_format_list;
};


/* prototypes */

struct main_widgets init_main_widgets();

void MHD_control_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl, 
                          struct main_widgets *mWidgets);

#endif /* CONTROL_PANEL_SGS_MHD_GTK_H_ */
