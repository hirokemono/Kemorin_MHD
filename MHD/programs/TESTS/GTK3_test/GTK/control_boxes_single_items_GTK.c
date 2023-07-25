/*
//  control_boxes_single_items_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "control_boxes_single_items_GTK.h"

extern int c_yes_flag(const char *text);
extern int c_no_file_flag(const char *file_name);

void cb_chara_ctl_item(GtkEntry *entry, gpointer data)
{
	struct chara_ctl_item *f_citem = (struct chara_ctl_item *) data;
	char * input_text;
	
	if(f_citem->f_self != NULL) {
/*		f_citem->f_iflag[0] = 1; */
		input_text = (char *) gtk_entry_get_text(entry);
        c_store_chara_item_charavalue(f_citem->f_self, input_text);
	};
	return;
}


void cb_check_toggle(GtkWidget *widget, gpointer iflag_ptr){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	int *iflag_block = (int *) iflag_ptr;
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		*iflag_block = 1;
	}else{
		*iflag_block = 0;
	};
	printf("*iflag_block %d \n", *iflag_block);
	return;
}

void cb_file_switch(GtkWidget *widget, gpointer data){
	GtkSwitch *file_switch = GTK_SWITCH(widget);
	GtkEntry *file_entry = GTK_ENTRY(g_object_get_data(G_OBJECT(widget), "entry"));
	char *f_file_name = (char *) g_object_get_data(G_OBJECT(widget), "file_name");
	
	if(gtk_switch_get_active(file_switch) == FALSE){
		sprintf(f_file_name, "%s", "NO_FILE");
		load_chara_from_c(f_file_name);
	}else{
		if(c_no_file_flag(f_file_name)){
			sprintf(f_file_name, "%s", "SET_FILE_NAME");
			gtk_entry_set_text(GTK_ENTRY(file_entry), f_file_name);
			load_chara_from_c(f_file_name);
		};
	};
	return;
}

void cb_char_switch(GtkWidget *widget, gpointer data){
	GtkSwitch *text_switch = GTK_SWITCH(widget);
    struct chara_ctl_item *f_citem = g_object_get_data(G_OBJECT(widget), "chara_ctl_item");
	
	if(gtk_switch_get_active(text_switch) == FALSE){
        c_store_chara_item_charavalue(f_citem->f_self, "Off");
	}else{
        c_store_chara_item_charavalue(f_citem->f_self, "On");
	};
	return;
}

void cb_file_name(GtkEntry *widget, gpointer data)
{
	GtkEntry *file_entry = GTK_ENTRY(widget);
	char *f_file_name = (char *) g_object_get_data(G_OBJECT(widget), "file_name");
	char *input_text = (char *) gtk_entry_get_text(file_entry);
	if(input_text != NULL) {
		sprintf(f_file_name, "%s", input_text);
		load_chara_from_c(f_file_name);
	};
	return;
}


GtkWidget *hbox_with_block_checkbox(int *iflag_ptr, const char *c_block_name){
    GtkWidget *label = gtk_label_new(c_block_name);
	GtkWidget *checkbox = gtk_check_button_new();
	if(*iflag_ptr == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	g_signal_connect(G_OBJECT(checkbox), "toggled", 
                     G_CALLBACK(cb_check_toggle), (gpointer) iflag_ptr);
	
    GtkWidget *hbox1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox1), checkbox, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox1), label, TRUE, TRUE, 0);
    
    GtkWidget *vbox1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox1), hbox1, TRUE, TRUE, 0);

    GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(hbox), vbox1, TRUE, TRUE, 0);
	return hbox;
}

GtkWidget * draw_control_block(const char * title, int *iflag_ptr, 
							   GtkWidget *window, GtkWidget *box_in)
{
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	GtkWidget *hbox1 = hbox_with_block_checkbox(iflag_ptr, "");
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *expander = wrap_into_expanded_frame_gtk
			(duplicate_underscore(title), window, box_in);
	
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, TRUE, TRUE, 0);
	return hbox0;
};

void append_block_file_switch_hbox(char *f_file_name, GtkWidget *hbox2)
{
	GtkWidget *file_entry = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(file_entry), strngcopy_from_f(f_file_name));
	g_object_set_data(G_OBJECT(file_entry), "file_name", (gpointer) f_file_name);
	g_signal_connect(G_OBJECT(file_entry), "notify::text",
					 G_CALLBACK(cb_file_name), (gpointer) NULL);
	
	GtkWidget *file_switch = gtk_switch_new();
	g_object_set_data(G_OBJECT(file_switch), "entry", (gpointer) file_entry);
	g_object_set_data(G_OBJECT(file_switch), "file_name", (gpointer) f_file_name);
	if(c_no_file_flag(f_file_name) == 0){
		gtk_switch_set_active(GTK_SWITCH(file_switch), TRUE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(file_switch), FALSE);
	};
	g_signal_connect(G_OBJECT(file_switch), "notify::active",
					 G_CALLBACK(cb_file_switch), (gpointer) NULL);
	
	GtkWidget *vbox2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox2), file_switch, FALSE, TRUE, 0);
	
	GtkWidget *file_label = gtk_label_new("File_name: ");
	
	gtk_box_pack_start(GTK_BOX(hbox2), file_label, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox2), vbox2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox2), file_entry, FALSE, TRUE, 0);
	return;
};

GtkWidget * draw_control_block_w_file_switch(const char * title, int *iflag_ptr, char *f_file_name,
                                             GtkWidget *window, GtkWidget *box_in)
{
	GtkWidget *hbox1 = hbox_with_block_checkbox(iflag_ptr, "");
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	append_block_file_switch_hbox(f_file_name, hbox2);
	
	GtkWidget *vbox1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox1), hbox2, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox1), box_in, FALSE, TRUE, 0);
	
	GtkWidget *expander = wrap_into_expanded_frame_gtk
			(duplicate_underscore(title), window, vbox1);
	
    GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, FALSE, 0);
	return hbox0;
};


static void cb_int_ctl_item(GtkSpinButton *spinner, gpointer data)
{
	struct int_ctl_item *f_iitem = (struct int_ctl_item *) g_object_get_data(G_OBJECT(spinner), "int_ctl_item");
    if(f_iitem->f_self == NULL) return;
    f_iitem->i_data = gtk_spin_button_get_value_as_int(spinner);
    c_store_int_item_intvalue(f_iitem->f_self, f_iitem->i_data);
	return;
}
static void cb_real_ctl_item(GtkEntry *spinner, gpointer data)
{
    struct real_ctl_item *f_ritem = (struct real_ctl_item *) data;
    if(f_ritem->f_self == NULL) return;
    f_ritem->r_data = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spinner));
    c_store_real_item_realvalue(f_ritem->f_self, f_ritem->r_data);
	return;
}

GtkWidget * draw_chara_switch_entry_hbox(struct chara_ctl_item * f_citem)
{
	GtkWidget *hbox = hbox_with_block_checkbox(f_citem->f_iflag, f_citem->c_block_name);
	
	GtkWidget *char_switch = gtk_switch_new();
	g_object_set_data(G_OBJECT(char_switch), "chara_ctl_item", (gpointer) f_citem);
	if(c_yes_flag(f_citem->c_tbl) == 0){
		gtk_switch_set_active(GTK_SWITCH(char_switch), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(char_switch), TRUE);
	};
	g_signal_connect(G_OBJECT(char_switch), "notify::active",
					 G_CALLBACK(cb_char_switch), (gpointer) NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), char_switch, TRUE, FALSE, 0);
	return hbox;
}

GtkWidget *draw_chara_item_entry_hbox(struct chara_ctl_item * f_citem)
{
	GtkWidget *hbox = hbox_with_block_checkbox(f_citem->f_iflag, f_citem->c_block_name);
	
	/* Generate file entry  */
	GtkWidget *entry = gtk_entry_new();
	gtk_entry_set_max_width_chars(GTK_ENTRY(entry), 32);
	gtk_entry_set_text(GTK_ENTRY(entry), f_citem->c_tbl);
	g_signal_connect(G_OBJECT(entry), "activate", G_CALLBACK(cb_chara_ctl_item), 
					 (gpointer) f_citem);
	
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	return hbox;
}

GtkWidget *draw_chara_item_combobox_hbox(struct chara_clist *item_list,
                                         struct chara_ctl_item *f_citem, GtkWidget *window){
    GtkWidget *hbox = hbox_with_block_checkbox(f_citem->f_iflag, f_citem->c_block_name);
    
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    add_control_combobox_vbox(f_citem, item_list, vbox);
    
    gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
    return hbox;
}


GtkWidget *draw_int_item_entry_hbox(struct int_ctl_item *f_iitem){
	GtkAdjustment *adjust = gtk_adjustment_new(f_iitem->i_data, 0, 2147483648, 1,
                    100, 21474836);
	
	GtkWidget *hbox = hbox_with_block_checkbox(f_iitem->f_iflag, f_iitem->c_block_name);
	
	/* Generate file entry  */
	GtkWidget *entry = gtk_spin_button_new(adjust, 1, 0);
	g_object_set_data(G_OBJECT(entry), "int_ctl_item", (gpointer) f_iitem);
	g_signal_connect(G_OBJECT(entry), "value-changed",
				G_CALLBACK(cb_int_ctl_item), (gpointer) NULL);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry), f_iitem->i_data);
	
	gtk_widget_set_halign(entry, GTK_ALIGN_START);
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	return hbox;
}

GtkWidget *draw_real_item_entry_hbox(struct real_ctl_item * f_ritem){
	GtkAdjustment *adjust = gtk_adjustment_new(f_ritem->r_data,
                                               -1.0e30, 1.0e30, 0.1, 100, 21474836);
	GtkWidget *hbox = hbox_with_block_checkbox(f_ritem->f_iflag, f_ritem->c_block_name);
	
	/* Generate file entry  */
	GtkWidget *entry = gtk_spin_button_new(adjust, 1, 0);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry), 9);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry), f_ritem->r_data);
	g_signal_connect(G_OBJECT(entry), "value-changed",
				G_CALLBACK(cb_real_ctl_item), (gpointer) f_ritem);
	
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	return hbox;
}



static void cb_real1_ctl_item(GtkEntry *spinner, gpointer data)
{
    struct real2_ctl_item *f_r2item = (struct real2_ctl_item *) data;
    if(f_r2item->f_self == NULL) return;
    f_r2item->r_data[0] = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spinner));
    c_store_real2_items(f_r2item->f_self,
                                f_r2item->r_data[0], f_r2item->r_data[1]);
	return;
}
static void cb_real2_ctl_item(GtkEntry *spinner, gpointer data)
{
    struct real2_ctl_item *f_r2item = (struct real2_ctl_item *) data;
    if(f_r2item->f_self == NULL) return;
    f_r2item->r_data[1] = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spinner));
    c_store_real2_items(f_r2item->f_self,
                                f_r2item->r_data[0], f_r2item->r_data[1]);
	return;
}

GtkWidget *draw_real2_item_entry_hbox(struct real2_ctl_item * f_r2item){
	GtkAdjustment *adjust1 = gtk_adjustment_new(f_r2item->r_data[0],
                                               -1.0e30, 1.0e30, 0.1, 100, 21474836);
	GtkAdjustment *adjust2 = gtk_adjustment_new(f_r2item->r_data[1],
                                               -1.0e30, 1.0e30, 0.1, 100, 21474836);

	GtkWidget *hbox = hbox_with_block_checkbox(f_r2item->f_iflag, f_r2item->c_block_name);
	
	/* Generate file entry  */
	GtkWidget *entry1 = gtk_spin_button_new(adjust1, 1, 0);
	GtkWidget *entry2 = gtk_spin_button_new(adjust2, 1, 0);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry1), 9);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry2), 9);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry1), f_r2item->r_data[0]);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry2), f_r2item->r_data[1]);
	g_signal_connect(G_OBJECT(entry1), "value-changed",
				G_CALLBACK(cb_real1_ctl_item), (gpointer) f_r2item);
	g_signal_connect(G_OBJECT(entry2), "value-changed",
				G_CALLBACK(cb_real2_ctl_item), (gpointer) f_r2item);
	
	gtk_box_pack_start(GTK_BOX(hbox), entry1, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), entry2, TRUE, TRUE, 0);
	return hbox;
}

static void cb_real3_1_ctl_item(GtkEntry *spinner, gpointer data)
{
    struct real3_ctl_item *f_r3item = (struct real3_ctl_item *) data;
    if(f_r3item->f_self == NULL) return;
    f_r3item->r_data[0] = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spinner));
    c_store_real3_items(f_r3item->f_self, f_r3item->r_data[0],
                        f_r3item->r_data[1], f_r3item->r_data[2]);
	return;
}
static void cb_real3_2_ctl_item(GtkEntry *spinner, gpointer data)
{
    struct real3_ctl_item *f_r3item = (struct real3_ctl_item *) data;
    if(f_r3item->f_self == NULL) return;
    f_r3item->r_data[1] = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spinner));
    c_store_real3_items(f_r3item->f_self, f_r3item->r_data[0],
                        f_r3item->r_data[1], f_r3item->r_data[2]);
	return;
}
static void cb_real3_3_ctl_item(GtkEntry *spinner, gpointer data)
{
    struct real3_ctl_item *f_r3item = (struct real3_ctl_item *) data;
    if(f_r3item->f_self == NULL) return;
    f_r3item->r_data[2] = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spinner));
    c_store_real3_items(f_r3item->f_self, f_r3item->r_data[0],
                        f_r3item->r_data[1], f_r3item->r_data[2]);
	return;
}

GtkWidget *draw_real3_item_entry_hbox(struct real3_ctl_item * f_r3item){
	GtkAdjustment *adjust1 = gtk_adjustment_new(f_r3item->r_data[0],
                                               -1.0e30, 1.0e30, 0.1, 100, 21474836);
	GtkAdjustment *adjust2 = gtk_adjustment_new(f_r3item->r_data[1],
                                               -1.0e30, 1.0e30, 0.1, 100, 21474836);
	GtkAdjustment *adjust3 = gtk_adjustment_new(f_r3item->r_data[2],
                                               -1.0e30, 1.0e30, 0.1, 100, 21474836);
	GtkWidget *hbox = hbox_with_block_checkbox(f_r3item->f_iflag, f_r3item->c_block_name);
	
	/* Generate file entry  */
	GtkWidget *entry1 = gtk_spin_button_new(adjust1, 1, 0);
	GtkWidget *entry2 = gtk_spin_button_new(adjust2, 1, 0);
	GtkWidget *entry3 = gtk_spin_button_new(adjust3, 1, 0);

    gtk_entry_set_width_chars(GTK_ENTRY(entry1), (gint) 12);
    gtk_entry_set_width_chars(GTK_ENTRY(entry2), (gint) 12);
    gtk_entry_set_width_chars(GTK_ENTRY(entry3), (gint) 12);

	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry1), 9);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry2), 9);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry3), 9);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry1), f_r3item->r_data[0]);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry2), f_r3item->r_data[1]);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry3), f_r3item->r_data[1]);
	g_signal_connect(G_OBJECT(entry1), "value-changed",
				G_CALLBACK(cb_real3_1_ctl_item), (gpointer) f_r3item);
	g_signal_connect(G_OBJECT(entry2), "value-changed",
				G_CALLBACK(cb_real3_2_ctl_item), (gpointer) f_r3item);
	g_signal_connect(G_OBJECT(entry3), "value-changed",
				G_CALLBACK(cb_real3_3_ctl_item), (gpointer) f_r3item);
	
	gtk_box_pack_start(GTK_BOX(hbox), entry1, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), entry2, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), entry3, TRUE, TRUE, 0);
	return hbox;
}


