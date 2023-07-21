/*
//  control_panel_PSF_ISO_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_PSF_ISO_GTK_H_
#define CONTROL_PANEL_PSF_ISO_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_int2_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "c_ctl_data_PSF_ISOs.h"
#include "control_panel_fld_on_psf_GTK.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panels_MHD_model_GTK.h"


struct PSF_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara_clist *label_dir_list;
    struct chara_clist *label_xyz_dir_list;
    
    struct chara2_cbox_table_view *field_output_vws;
    
    struct chara_cbox_table_view * psf_coefs_vws;
    struct chara_cbox_table_view * psf_center_vws;
    struct chara_cbox_table_view * psf_normal_vws;
    struct chara_cbox_table_view * psf_axis_vws;
    GtkWidget * psf_area_view;
};

struct ISO_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara_clist *label_dir_list;
    
    struct chara2_cbox_table_view *field_output_vws;
    GtkWidget * iso_area_view;
};

/* prototypes */
extern void * c_link_scalar_dir_list_to_ctl(void *fld_names_c);
extern void * c_link_vector_dir_list_to_ctl(void *fld_names_c);
extern void * c_link_stensor_dir_list_to_ctl(void *fld_names_c);
extern void * c_link_atensor_dir_list_to_ctl(void *fld_names_c);


struct PSF_GTK_widgets * init_PSF_GTK_widgets(struct chara_int2_clist *f_field_ctl);
void dealloc_PSF_GTK_widgets(struct PSF_GTK_widgets *psf_def_vws);

struct ISO_GTK_widgets * init_ISO_GTK_widgets(struct chara_int2_clist *f_field_ctl);

struct f_VIZ_PSF_ctl * init_f_VIZ_PSF_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void * dealloc_f_VIZ_PSF_ctl_GTK(void *void_in);

struct f_VIZ_ISO_ctl * init_f_VIZ_ISO_ctl_GTK(int idx, void *f_parent, void *void_in_gtk);
void * dealloc_f_VIZ_ISO_ctl_GTK(void *void_in);

GtkWidget * draw_psf_def_ctl_vbox(struct f_VIZ_PSF_def_ctl *f_psf_def_c,
                                  struct PSF_GTK_widgets *psf_def_vws, GtkWidget *window);

GtkWidget * draw_viz_each_psf_ctl_vbox(char *label_name, struct f_VIZ_PSF_ctl *f_psf_item,
                                       GtkWidget *window);
GtkWidget * draw_viz_each_iso_ctl_vbox(char *label_name, struct f_VIZ_ISO_ctl *f_iso_item, 
                                       GtkWidget *window);

#endif /* CONTROL_PANEL_PSF_ISO_GTK_H_ */
