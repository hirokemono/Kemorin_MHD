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

void cb_expander_switch(GObject *switch_3, GParamSpec *pspec, gpointer data);
void cb_expander_action(GObject *switch_3, gpointer data);
GtkWidget *make_control_block_hbox(char *c_label, int *iflag_box, GtkWidget *expander_b);
GtkWidget * make_empty_ctl_hbox(const char *label_hd, int *iflag_use);

GtkWidget *make_chara_ctl_switch_hbox(const char *label, struct chara_ctl_item *ctl_item);
GtkWidget *make_toggle_hbox (const char *label, struct chara_ctl_item *ctl_item,
			gboolean is_on, gboolean is_sensitive);
GtkWidget *make_text_hbox (const char *label, struct chara_ctl_item *ctl_item);
GtkWidget *make_integer_hbox (const char *label, struct int_ctl_item *ctl_item);
GtkWidget *make_real_hbox (const char *label, struct real_ctl_item *ctl_item);
GtkWidget *make_filename_hbox (const char *label, struct chara_ctl_item *ctl_item);


#endif /* control_elements_IO_GTK_k_ */
