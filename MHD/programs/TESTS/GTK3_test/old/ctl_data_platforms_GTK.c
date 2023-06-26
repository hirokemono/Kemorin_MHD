/*
//  ctl_data_platforms_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "ctl_data_platforms_GTK.h"

extern void load_chara_from_c(char *c_ctl);


struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_ctl_chara_item *f_citem = (struct f_ctl_chara_item *) malloc(sizeof(struct f_ctl_chara_item));
	if(f_citem == NULL){
		printf("malloc error for f_ctl_chara_item\n");
		exit(0);
	};
	f_citem->f_self =  c_load_self(f_parent);
	
	f_citem->f_block_name =  (char *)  c_chara_item_block_name(f_citem->f_self);
	f_citem->f_iflag =        (int *)   c_chara_item_iflag(f_citem->f_self);
	f_citem->f_charavalue =  (char *)  c_chara_item_charavalue(f_citem->f_self);
	c_chara_item_clength(f_citem->f_self, f_citem->f_clength);
	
	printf("f_citem->f_self %p \n", f_citem->f_self);
	printf("f_citem->f_block_name %s \n", f_citem->f_block_name);
	printf("f_citem->f_charavalue %d %s \n", 
		   f_citem->f_iflag[0], f_citem->f_charavalue);
		   
	return f_citem;
}

struct f_platform_control * init_f_platform_control(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_platform_control *f_plt = (struct f_platform_control *) malloc(sizeof(struct f_platform_control));
	if(f_plt == NULL){
		printf("malloc error for f_plt\n");
		exit(0);
	};
	f_plt->f_self =  c_load_self(f_parent);
	f_plt->f_block_name =  (char *) c_plt_block_name(f_plt->f_self);
	f_plt->f_iflag =       (int *)  c_plt_iflag(f_plt->f_self);
	
	f_plt->f_ndomain_ctl =               c_plt_ndomain_ctl(f_plt->f_self);
	f_plt->f_num_smp_ctl =               c_plt_num_smp_ctl(f_plt->f_self);
	
	
	f_plt->f_debug_flag_ctl =            init_f_ctl_chara_item(c_plt_debug_flag_ctl, f_plt->f_self);
	f_plt->f_sph_file_prefix =           init_f_ctl_chara_item(c_plt_sph_file_prefix, f_plt->f_self);
	f_plt->f_mesh_file_prefix =          init_f_ctl_chara_item(c_plt_mesh_file_prefix, f_plt->f_self);
	f_plt->f_restart_file_prefix =       init_f_ctl_chara_item(c_plt_restart_file_prefix, f_plt->f_self);
	f_plt->f_field_file_prefix =         init_f_ctl_chara_item(c_plt_field_file_prefix, f_plt->f_self);
	f_plt->f_spectr_field_file_prefix =  init_f_ctl_chara_item(c_plt_spectr_field_file_prefix, f_plt->f_self);
	f_plt->f_coriolis_int_file_name =    init_f_ctl_chara_item(c_plt_coriolis_int_file_name, f_plt->f_self);
	f_plt->f_bc_data_file_name_ctl =     init_f_ctl_chara_item(c_plt_bc_data_file_name_ctl, f_plt->f_self);
	f_plt->f_radial_data_file_name_ctl = init_f_ctl_chara_item(c_plt_radial_data_file_name_ctl, f_plt->f_self);
	f_plt->f_interpolate_sph_to_fem =    init_f_ctl_chara_item(c_plt_interpolate_sph_to_fem, f_plt->f_self);
	f_plt->f_interpolate_fem_to_sph =    init_f_ctl_chara_item(c_plt_interpolate_fem_to_sph, f_plt->f_self);
	f_plt->f_rayleigh_spectr_dir =       init_f_ctl_chara_item(c_plt_rayleigh_spectr_dir, f_plt->f_self);
	f_plt->f_rayleigh_field_dir =        init_f_ctl_chara_item(c_plt_rayleigh_field_dir, f_plt->f_self);
	f_plt->f_sph_file_fmt_ctl =          init_f_ctl_chara_item(c_plt_sph_file_fmt_ctl, f_plt->f_self);
	f_plt->f_mesh_file_fmt_ctl =         init_f_ctl_chara_item(c_plt_mesh_file_fmt_ctl, f_plt->f_self);
	f_plt->f_restart_file_fmt_ctl =      init_f_ctl_chara_item(c_plt_restart_file_fmt_ctl, f_plt->f_self);
	f_plt->f_field_file_fmt_ctl =        init_f_ctl_chara_item(c_plt_field_file_fmt_ctl, f_plt->f_self);
	f_plt->f_spectr_field_fmt_ctl =      init_f_ctl_chara_item(c_plt_spectr_field_fmt_ctl, f_plt->f_self);
	f_plt->f_itp_file_fmt_ctl =          init_f_ctl_chara_item(c_plt_itp_file_fmt_ctl, f_plt->f_self);
	f_plt->f_coriolis_file_fmt_ctl =     init_f_ctl_chara_item(c_plt_coriolis_file_fmt_ctl, f_plt->f_self);
	f_plt->f_del_org_data_ctl =          init_f_ctl_chara_item(c_plt_del_org_data_ctl, f_plt->f_self);
	return f_plt;
}

void cb_chara_ctl_item(GtkEntry *entry, gpointer data)
{
	struct f_ctl_chara_item *f_citem = (struct f_ctl_chara_item *) data;
	char * input_text;
	
	if(f_citem->f_self != NULL) {
/*		f_citem->f_iflag[0] = 1; */
		input_text = (char *) gtk_entry_get_text(entry);
		f_citem->f_clength[0] = strngcopy(f_citem->f_charavalue, (char *) input_text);
		load_chara_from_c(f_citem->f_charavalue);
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


GtkWidget * draw_control_block(const char * title, int *iflag_ptr, 
							   int width, int height,
							   GtkWidget *window, GtkWidget *box_in)
{
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	if(iflag_ptr[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	g_signal_connect(G_OBJECT(checkbox), "toggled", 
                     G_CALLBACK(cb_check_toggle), (gpointer) iflag_ptr);
	
	GtkWidget *expander = wrap_into_expanded_frame_gtk
			(duplicate_underscore(title), width, height, window, box_in);
	gtk_box_pack_start(GTK_BOX(vbox0), checkbox, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, FALSE, 0);
	return hbox0;
};

GtkWidget * draw_chara_item_entry_hbox(struct f_ctl_chara_item * f_citem, GtkWidget *window)
{
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	if(f_citem->f_iflag[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	g_signal_connect(G_OBJECT(checkbox), "toggled", 
                     G_CALLBACK(cb_check_toggle), (gpointer) f_citem->f_iflag);
	GtkWidget *label = gtk_label_new(f_citem->f_block_name);
	
	/* Generate file entry  */
	GtkWidget *entry = gtk_entry_new();
	gtk_entry_set_max_width_chars(GTK_ENTRY(entry), 80);
	gtk_entry_set_text(GTK_ENTRY(entry), f_citem->f_charavalue);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_signal_connect(G_OBJECT(entry), "activate", G_CALLBACK(cb_chara_ctl_item), 
					 (gpointer) f_citem);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	return hbox;
}



GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_plt->f_debug_flag_ctl, window);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_plt->f_sph_file_prefix, window);
	GtkWidget *hbox_c3 = draw_chara_item_entry_hbox(f_plt->f_sph_file_fmt_ctl, window);
	/*
	GtkWidget *hbox_c4 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_prefix, window);
	GtkWidget *hbox_c5 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c6 = draw_chara_item_entry_hbox(f_plt->f_restart_file_prefix, window);
	GtkWidget *hbox_c7 = draw_chara_item_entry_hbox(f_plt->f_restart_file_fmt_ctl, window);
	GtkWidget *hbox_c8 = draw_chara_item_entry_hbox(f_plt->f_field_file_prefix, window);
	GtkWidget *hbox_c9 = draw_chara_item_entry_hbox(f_plt->f_field_file_fmt_ctl, window);
	/*
	GtkWidget *hbox_c10 = draw_chara_item_entry_hbox(f_plt->f_spectr_field_file_prefix, window);
	GtkWidget *hbox_c11 = draw_chara_item_entry_hbox(f_plt->f_spectr_field_fmt_ctl, window);
	GtkWidget *hbox_c12 = draw_chara_item_entry_hbox(f_plt->f_coriolis_int_file_name, window);
	GtkWidget *hbox_c13 = draw_chara_item_entry_hbox(f_plt->f_coriolis_int_file_name, window);
	*/
	GtkWidget *hbox_c14 = draw_chara_item_entry_hbox(f_plt->f_coriolis_file_fmt_ctl, window);
	GtkWidget *hbox_c15 = draw_chara_item_entry_hbox(f_plt->f_radial_data_file_name_ctl, window);
	/*
	GtkWidget *hbox_c16 = draw_chara_item_entry_hbox(f_plt->f_interpolate_sph_to_fem, window);
	GtkWidget *hbox_c17 = draw_chara_item_entry_hbox(f_plt->f_interpolate_fem_to_sph, window);
	GtkWidget *hbox_c18 = draw_chara_item_entry_hbox(f_plt->f_itp_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c19 = draw_chara_item_entry_hbox(f_plt->f_rayleigh_spectr_dir, window);
	GtkWidget *hbox_c20 = draw_chara_item_entry_hbox(f_plt->f_rayleigh_field_dir, window);
	
	GtkWidget *hbox_c21 = draw_chara_item_entry_hbox(f_plt->f_del_org_data_ctl, window);
	
	GtkWidget *vbox_plt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c7, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c8, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c9, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c14, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c15, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c19, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c20, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c21, FALSE, FALSE, 0);
	
	GtkWidget *expand_PLT = draw_control_block(f_plt->f_block_name, f_plt->f_iflag,
											   560, 280, window, vbox_plt);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_PLT, FALSE, FALSE, 0);
	return vbox_out;
};
