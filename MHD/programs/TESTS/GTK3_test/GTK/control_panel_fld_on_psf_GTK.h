/*
//  control_panel_fld_on_psf_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_FLD_ON_PSF_GTK_H_
#define CONTROL_PANEL_FLD_ON_PSF_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_control_chara2_int_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara2_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panel_cbox_cbox_GTK.h"
#include "control_panel_field_cbox_GTK.h"

/* prototypes */

GtkWidget * c2_list_fld_on_psf_expander(struct chara2_clist *ctl_clist,
                                        struct chara_int2_clist *f_field_ctl, 
                                        struct chara2_int_clist *label_dir_list,
                                        struct chara2_cbox_table_view *chara2_tbl_vws,
                                        GtkWidget *window);

#endif /* CONTROL_PANEL_FLD_ON_PSF_GTK_H_ */
