/*
//  control_elements_IO_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef control_elements_IO_GTK_k_
#define control_elements_IO_GTK_k_


#include "calypso_GTK.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "tree_view_chara_int_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"

#define NONE_MODE   0
#define FILE_MODE  -1
#define TYPE_MODE   1

struct entry_and_flag{
    int iflag_fix;
    int *iflag;
    GtkWidget *entry;
    GtkWidget *check;
};

/*  prototypes */

void cb_expander_action(GObject *switch_3, gpointer data);
void cb_toggle_switch(GtkCheckButton *check, gpointer data);
void cb_toggle_entry(GtkCheckButton *check, gpointer data);

GtkWidget *make_expand_ctl_hbox(const char *label_hd, int *iflag_use, int vsize_scroll,
			GtkWidget *vbox_1);
GtkWidget *make_empty_ctl_hbox(const char *label_hd, int *iflag_use);
GtkWidget *make_expand_ctl_file_hbox(const char *label_hd, int *iflag_use, char *filke_name, 
                                     int vsize_scroll, GtkWidget *vbox_1, GtkWidget *save_bottun);

GtkWidget *make_entry_with_switch_hbox(int iflag_fix_on, const char *label, int *iflag, 
			struct entry_and_flag *tbox_flag);
GtkWidget *make_entry_with_check_hbox(int iflag_fix_on, const char *label, int *iflag, 
			struct entry_and_flag *tbox_flag);

GtkWidget *make_chara_ctl_switch_hbox(int iflag_fix_on, const char *label, struct chara_ctl_item *ctl_item);
GtkWidget *make_toggle_hbox(const char *label, struct chara_ctl_item *ctl_item,
			gboolean is_on, gboolean is_sensitive);
GtkWidget *make_text_hbox(int iflag_fix_on, const char *label, struct chara_ctl_item *ctl_item);
GtkWidget *make_integer_hbox(int iflag_fix_on, const char *label, struct int_ctl_item *ctl_item);
GtkWidget *make_real_hbox(int iflag_fix_on, const char *label, struct real_ctl_item *ctl_item);
GtkWidget *make_filename_hbox(const char *label, struct chara_ctl_item *ctl_item);


#endif /* control_elements_IO_GTK_k_ */
