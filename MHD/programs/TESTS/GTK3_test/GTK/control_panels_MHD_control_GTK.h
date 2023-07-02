//
//  control_panels_MHD_control_GTK.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/1/23.
//

#ifndef control_panels_MHD_control_GTK_h
#define control_panels_MHD_control_GTK_h

#include <stdio.h>
#include <gtk/gtk.h>

#include "t_control_MHD_controls.h"
#include "control_boxes_single_items_GTK.h"


/* prototypes */

GtkWidget * draw_MHD_control_expand(GtkWidget *window, struct f_MHD_control_ctls *f_smctl_ctl);

#endif /* control_panels_MHD_control_GTK_h */
