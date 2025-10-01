/*
//  control_panel_VIZs_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_VIZS_GTK_H_
#define CONTROL_PANEL_VIZS_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "c_ctl_data_SGS_MHD.h"
#include "control_block_panel_GTK.h"
#include "control_panel_VIZ_repartition_GTK.h"
#include "control_panel_PVR_GTK.h"
#include "control_panel_MAP_GTK.h"
#include "control_panel_LIC_GTK.h"
#include "control_panel_FLINE_GTK.h"


struct VIZs_widgets{
    struct VIZ_repartition_widgets *f_repart_vws;
    
	struct block_array_widgets *vpsf_Wgts;
	struct block_array_widgets *viso_Wgts;
	struct block_array_widgets *vmap_Wgts;
	struct block_array_widgets *vpvr_Wgts;
	struct block_array_widgets *vlic_Wgts;
    struct block_array_widgets *vfline_Wgts;
    
    struct chara_clist *label_file_format_list;
};

struct dynamo_VIZs_widgets{
    struct block_array_widgets *zm_psf_Wgts;
	struct block_array_widgets *zrms_psf_Wgts;
	struct block_array_widgets *zm_map_Wgts;
	struct block_array_widgets *zrms_map_Wgts;
};


/* prototypes */

struct VIZs_widgets *init_MHD_VIZs_GTK(struct f_MHD_viz_ctls *f_viz_ctls, 
                                       struct f_MHD_fields_control *f_fld_ctl);
struct dynamo_VIZs_widgets *init_dynamo_VIZs_GTK(struct f_MHD_zm_ctls *f_zm_ctls, 
                                                 struct f_MHD_fields_control *f_fld_ctl);

GtkWidget *MHD_VIZs_ctl_expander(GtkWidget *window, struct f_MHD_viz_ctls *f_viz_ctls, 
                                 struct f_MHD_fields_control *f_fld_ctl,
                                 struct VIZs_widgets *vizs_Wgts);
GtkWidget * MHD_dynamo_VIZs_expander(GtkWidget *window, struct f_MHD_zm_ctls *f_zm_ctls, 
                                     struct f_MHD_fields_control *f_fld_ctl,
                                     struct dynamo_VIZs_widgets *dviz_Wgts);

#endif /* CONTROL_PANEL_VIZS_GTK_H_ */
