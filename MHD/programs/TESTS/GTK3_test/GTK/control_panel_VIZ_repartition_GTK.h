/*
//  control_panel_VIZ_repartition_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_VIZ_REPARTITION_GTK_H_
#define CONTROL_PANEL_VIZ_REPARTITION_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "control_block_panel_GTK.h"
#include "control_panel_fld_on_psf_GTK.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panel_chara_int_GTK.h"
#include "ctl_data_platforms_GTK.h"
#include "c_ctl_VIZ_repartition.h"


struct VIZ_masking_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
};

struct VIZ_repartition_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara_clist      *label_file_format_list;
    
    struct block_array_widgets *masking_Wgts;
    
    GtkWidget * fline_area_grp_view;
    GtkWidget * f_ndomain_section_tree;
};


/* prototypes */
extern void * c_append_multi_mask_ctls(int idx, char *block_name, void *f_new_part_ctl);
extern void * c_delete_multi_mask_ctls(int idx, void *f_new_part_ctl);


struct VIZ_masking_GTK_widgets * init_VIZ_masking_GTK_widgets(struct chara_int2_clist *f_field_ctl);
void dealloc_VIZ_masking_GTK_widgets(struct VIZ_masking_GTK_widgets *masking_vws);

struct f_LIC_masking_ctl * init_f_VIZ_masking_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void * dealloc_f_VIZ_masking_ctl_GTK(void *void_in);

GtkWidget * draw_viz_each_masking_vbox(char *label_name, struct f_LIC_masking_ctl *f_mask_ctl, 
                                       GtkWidget *window);

GtkWidget * draw_VIZ_repartition_ctl_vbox(struct f_VIZ_repartition_ctl *f_repart_ctl, 
                                          struct VIZ_repartition_widgets *f_repart_vws,
                                          GtkWidget *window);


#endif /* CONTROL_PANEL_VIZ_REPARTITION_GTK_H_ */
