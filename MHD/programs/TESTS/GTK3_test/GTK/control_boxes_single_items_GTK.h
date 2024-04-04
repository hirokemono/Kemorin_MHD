/*
//  control_boxes_single_items_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#ifndef CONTROL_BOXES_SINGLE_ITEMS_GTK_H_
#define CONTROL_BOXES_SINGLE_ITEMS_GTK_H_

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <gtk/gtk.h>

#include "kemoview_gtk_routines.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_control_int2_IO.h"
#include "t_control_real2_IO.h"
#include "t_control_real3_IO.h"
#include "control_combobox_GTK.h"

/* prototypes */

GtkWidget *hbox_with_block_checkbox(int *iflag_ptr, const char *c_block_name);

GtkWidget * draw_control_block(const char * title, int *iflag_ptr, 
							   GtkWidget *window, GtkWidget *box_in);
          
void append_block_file_switch_hbox(char *f_file_name, GtkWidget *hbox2);
GtkWidget * draw_control_block_w_file_switch(const char * title, int *iflag_ptr, char *f_file_name,
                                             GtkWidget *window, GtkWidget *box_in);

GtkWidget *draw_chara_switch_entry_hbox(struct chara_ctl_item * f_citem);
GtkWidget *draw_chara_item_entry_hbox(struct chara_ctl_item * f_citem);
GtkWidget *draw_chara_item_combobox_hbox(struct chara_clist *item_list,
                                         struct chara_ctl_item *f_citem, GtkWidget *window);


GtkWidget *draw_int_item_entry_hbox(struct int_ctl_item *f_iitem);
GtkWidget *draw_real_item_entry_hbox(struct real_ctl_item * f_ritem);

GtkWidget *draw_int2_item_entry_hbox(struct int2_ctl_item * f_i2item);
GtkWidget *draw_real2_item_entry_hbox(struct real2_ctl_item * f_r2item);
GtkWidget *draw_real3_item_entry_hbox(struct real3_ctl_item * f_r3item);

#endif /* CONTROL_BOXES_SINGLE_ITEMS_GTK_H_ */
