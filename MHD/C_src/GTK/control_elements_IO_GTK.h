/*
//  control_elements_IO_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef control_elements_IO_GTK_k_
#define control_elements_IO_GTK_k_


#include <gtk/gtk.h>
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

GtkWidget *make_toggle_hbox (const char *label, struct chara_ctl_item *ctl_item,
			gboolean is_on, gboolean is_sensitive);
GtkWidget *make_text_hbox (const char *label, struct chara_ctl_item *ctl_item);
GtkWidget *make_integer_hbox (const char *label, struct int_ctl_item *ctl_item);
GtkWidget *make_real_hbox (const char *label, struct real_ctl_item *ctl_item);
GtkWidget *make_filename_hbox (const char *label, struct chara_ctl_item *ctl_item);


#endif /* control_elements_IO_GTK_k_ */
