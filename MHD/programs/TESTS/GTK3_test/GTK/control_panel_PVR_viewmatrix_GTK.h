/*
//  control_panel_PVR_viewmatrix_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef CONTROL_PANEL_PVR_VIEWMATRIX_GTK_H_
#define CONTROL_PANEL_PVR_VIEWMATRIX_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "c_ctl_data_SGS_model.h"
#include "t_control_chara2_int_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_ctl_data_4_view_transfer_c.h"
#include "control_block_panel_GTK.h"
#include "control_panel_chara_GTK.h"
#include "control_panel_int_GTK.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panel_cbox_cbox_real_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panels_MHD_model_GTK.h"


struct viewmat_GTK_widgets{
    struct chara2_int_clist *label_xyz_dir_list;
    struct chara2_int_clist *label_xyzw_dir_list;
    
    struct cbox_cbox_table_view *f_map_image_prefix_vws;
    struct chara_cbox_table_view *f_lookpoint_vws;
    struct chara_cbox_table_view *f_viewpoint_vws;
    struct chara_cbox_table_view *f_up_dir_vws;
    struct chara_cbox_table_view *f_scale_vector_vws;
    struct chara_cbox_table_view *f_viewpt_in_viewer_vws;
    struct chara_cbox_table_view *f_view_rot_vec_vws;
};

/* prototypes */
struct viewmat_GTK_widgets * init_viewmat_GTK_widgets(struct chara_int2_clist *f_field_ctl);
void dealloc_viewmat_GTK_widgets(struct viewmat_GTK_widgets *viewmatrix_vws);

struct modelview_ctl_c * init_modelview_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void * dealloc_modelview_ctl_GTK(void *void_in);

GtkWidget * draw_viz_viewmatrix_vbox(char *label_name, 
                                     struct modelview_ctl_c *f_mat_c, 
                                     GtkWidget *window);

#endif /* CONTROL_PANEL_PVR_VIEWMATRIX_GTK_H_ */
