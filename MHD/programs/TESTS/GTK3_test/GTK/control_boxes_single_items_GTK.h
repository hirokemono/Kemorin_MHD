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
#include "m_base_control_labels_from_f.h"
#include "t_ctl_array_single_items_c.h"

/* prototypes */

GtkWidget *hbox_with_block_checkbox(int *iflag_ptr);

GtkWidget * draw_control_block(const char * title, int *iflag_ptr, 
							   int width, int height,
							   GtkWidget *window, GtkWidget *box_in);
GtkWidget * draw_control_block_w_file_switch(const char * title, int *iflag_ptr, 
											 char *f_file_name, int width, int height,
											 GtkWidget *window, GtkWidget *box_in);

GtkWidget * draw_file_format_select_hbox(struct control_labels_f *label_file_format_list, 
										 struct chara_ctl_item * f_citem, GtkWidget *window);

GtkWidget *draw_chara_switch_entry_hbox(struct chara_ctl_item * f_citem);
GtkWidget *draw_chara_item_entry_hbox(struct chara_ctl_item * f_citem);
GtkWidget *draw_int_item_entry_hbox(struct int_ctl_item *f_iitem);
GtkWidget *draw_real_item_entry_hbox(struct f_ctl_real_item * f_ritem);

#endif /* CONTROL_BOXES_SINGLE_ITEMS_GTK_H_ */
