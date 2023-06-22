/*
//  ctl_data_platforms_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "ctl_data_platforms_GTK.h"

int lengthchara_f();
extern int num_file_fmt_items_f();
extern void set_file_fmt_items_f(char *fmt_names_c);


struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent), 
												void *f_parent)
{
	struct f_ctl_chara_item *f_citem = (struct f_ctl_chara_item *) malloc(sizeof(struct f_ctl_chara_item));
	if(f_citem == NULL){
		printf("malloc error for f_ctl_chara_item\n");
		exit(0);
	};
	f_citem->f_self =  c_load_self(f_parent);
	
	f_citem->f_block_name =  (char *) c_chara_item_block_name(f_citem->f_self);
	f_citem->f_iflag =        (int *) c_chara_item_iflag(f_citem->f_self);
	f_citem->f_charavalue =  (char *) c_chara_item_charavalue(f_citem->f_self);
	
	c_chara_item_clength(f_citem->f_block_name, f_citem->f_namelength);
	f_citem->c_block_name = alloc_string((long) f_citem->f_namelength[0]);
	strngcopy_w_length(f_citem->c_block_name, f_citem->f_namelength[0], 
					   f_citem->f_block_name);
	
	c_chara_item_clength(f_citem->f_charavalue, f_citem->f_clength);
	f_citem->c_charavalue = alloc_string((long) f_citem->f_clength[0]);
	strngcopy_w_length(f_citem->c_charavalue, f_citem->f_clength[0], 
					   f_citem->f_charavalue);
	/*
	printf("f_citem->f_self %p \n", f_citem->f_self);
	printf("f_citem->c_block_name %s \n", f_citem->c_block_name);
	printf("f_citem->c_charavalue %d %s \n", 
		   f_citem->f_iflag[0], f_citem->c_charavalue);
	*/
	return f_citem;
}


void dealloc_f_ctl_chara_item(struct f_ctl_chara_item *f_citem)
{
	free(f_citem->c_charavalue);
	free(f_citem->c_block_name);
	
	f_citem->f_charavalue = NULL;
	f_citem->f_iflag = NULL;
	f_citem->f_block_name = NULL;
	f_citem->f_self = NULL;
	return;
}

struct f_ctl_chara_array * init_f_ctl_chara_array(void *(*c_load_self)(void *f_parent), 
												  void *f_parent)
{
	struct f_ctl_chara_array *f_carray = (struct f_ctl_chara_array *) malloc(sizeof(struct f_ctl_chara_array));
	if(f_carray == NULL){
		printf("malloc error for f_ctl_chara_array\n");
		exit(0);
	};
	f_carray->f_self =  c_load_self(f_parent);
	
	f_carray->f_block_name =  (char *) c_chara_array_block_name(f_carray->f_self);
	f_carray->f_num =         (int *)  c_chara_array_num(f_carray->f_self);
	f_carray->f_icou =        (int *)  c_chara_array_icou(f_carray->f_self);
	f_carray->f_cctls =       (char *) c_chara_array_c_tbl(f_carray->f_self);
	
	c_chara_item_clength(f_carray->f_block_name, f_carray->f_namelength);
	f_carray->c_block_name = alloc_string((long) f_carray->f_namelength[0]);
	strngcopy_w_length(f_carray->c_block_name, f_carray->f_namelength[0], 
					   f_carray->f_block_name);
	
	f_carray->c_charavalue = (char **) malloc(f_carray->f_num[0] * sizeof(char *));
	if(f_carray->c_charavalue == NULL){
		printf("malloc error for f_carray->c_charavalue \n");
		exit(0);
	};
	if((f_carray->f_clength = (int *) calloc(f_carray->f_num[0], sizeof(int))) == NULL){
		printf("malloc error for f_carray->f_clength\n");
		exit(0);
	}
	
	int i;
	int flen = 255;
	for(i=0;i<f_carray->f_num[0];i++){
		c_chara_item_clength(&f_carray->f_cctls[i*flen], &f_carray->f_clength[i]);
		f_carray->c_charavalue[i] = alloc_string((long) f_carray->f_clength[i]);
		strngcopy_w_length(f_carray->c_charavalue[i], f_carray->f_clength[i], 
						   &f_carray->f_cctls[i*flen]);
	};
	
	printf("f_carray->f_self %p \n", f_carray->f_self);
	printf("f_carray->c_block_name %s %d\n", f_carray->c_block_name, f_carray->f_num[0]);
	for(i=0;i<f_carray->f_num[0];i++){
		printf("%d f_carray->c_charavalue %d %p %s \n", i, 
			   f_carray->f_clength[i], f_carray->c_charavalue[i], f_carray->c_charavalue[i]);
	}
	
	return f_carray;
}

void dealloc_f_ctl_chara_array(struct f_ctl_chara_array *f_carray)
{
	int i;
	for(i=0;i<f_carray->f_num[0];i++){free(f_carray->c_charavalue[i]);};
	free(f_carray->c_charavalue);
	free(f_carray->f_clength);
	free(f_carray->c_block_name);
	
	f_carray->f_cctls = NULL;
	f_carray->f_icou = NULL;
	f_carray->f_num = NULL;
	f_carray->f_block_name = NULL;
	f_carray->f_self = NULL;
	return;
}

struct c_array_views * init_c_array_views(struct f_ctl_chara_array *f_carray)
{
	int i;
	struct c_array_views *c_array_vws = (struct c_array_views *) malloc(sizeof(struct c_array_views));
	if(c_array_vws == NULL){
		printf("malloc error for c_array_views\n");
		exit(0);
	};
	
	c_array_vws->c_array_clist = init_chara_clist();
	for(i=0;i<f_carray->f_num[0];i++){
		append_chara_clist(f_carray->c_charavalue[i], c_array_vws->c_array_clist);
	}
	
	
	printf("count_chara_clist %d\n", count_chara_clist(c_array_vws->c_array_clist));
	for(i=0;i<f_carray->f_num[0];i++){
		printf("item %d %s\n", i, chara_clist_at_index(i, c_array_vws->c_array_clist)->c_tbl);
	}
	return c_array_vws;
}

void dealloc_c_array_views(struct c_array_views *c_array_vws)
{
	dealloc_chara_clist(c_array_vws->c_array_clist);
	return;
}


struct f_ctl_cr_item * init_f_ctl_cr_item(void *(*c_load_self)(void *f_parent),
											 void *f_parent)
{
	struct f_ctl_cr_item *f_cr_item = (struct f_ctl_cr_item *) malloc(sizeof(struct f_ctl_cr_item));
	if(f_cr_item == NULL){
		printf("malloc error for f_ctl_cr_item\n");
		exit(0);
	};
	f_cr_item->f_self =  c_load_self(f_parent);
	
	f_cr_item->f_block_name =  (char *) c_chara_real_item_block_name(f_cr_item->f_self);
	f_cr_item->f_iflag =        (int *) c_chara_real_item_iflag(f_cr_item->f_self);
	f_cr_item->f_charavalue =  (char *) c_chara_real_item_charavalue(f_cr_item->f_self);
	f_cr_item->f_realvalue =  (double *) c_chara_real_item_realvalue(f_cr_item->f_self);
	
	c_chara_item_clength(f_cr_item->f_block_name, f_cr_item->f_namelength);
	f_cr_item->c_block_name = alloc_string((long) f_cr_item->f_namelength[0]);
	strngcopy_w_length(f_cr_item->c_block_name, f_cr_item->f_namelength[0], 
					   f_cr_item->f_block_name);
	
	c_chara_item_clength(f_cr_item->f_charavalue, f_cr_item->f_clength);
	f_cr_item->c_charavalue = alloc_string((long) f_cr_item->f_clength[0]);
	strngcopy_w_length(f_cr_item->c_charavalue, f_cr_item->f_clength[0], 
					   f_cr_item->f_charavalue);
	/*
	printf("f_cr_item->f_self %p \n", f_cr_item->f_self);
	printf("f_cr_item->c_block_name %s \n", f_cr_item->c_block_name);
	printf("f_cr_item->c_charavalue %d %s \n", 
		   f_cr_item->f_iflag[0], f_cr_item->c_charavalue, f_cr_item->c_charavalue);
	*/
	return f_cr_item;
}

void dealloc_f_ctl_cr_item(struct f_ctl_cr_item *f_cr_item)
{
	free(f_cr_item->c_charavalue);
	free(f_cr_item->c_block_name);
	
	f_cr_item->f_realvalue = NULL;
	f_cr_item->f_charavalue = NULL;
	f_cr_item->f_iflag = NULL;
	f_cr_item->f_block_name = NULL;
	f_cr_item->f_self = NULL;
	return;
}


struct f_ctl_cr_array * init_f_ctl_cr_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct f_ctl_cr_array *f_cr_array = (struct f_ctl_cr_array *) malloc(sizeof(struct f_ctl_cr_array));
	if(f_cr_array == NULL){
		printf("malloc error for f_ctl_cr_array\n");
		exit(0);
	};
	f_cr_array->f_self =  c_load_self(f_parent);
	
	f_cr_array->f_block_name =  (char *) c_chara_real_array_block_name(f_cr_array->f_self);
	f_cr_array->f_num =         (int *)  c_chara_real_array_num(f_cr_array->f_self);
	f_cr_array->f_icou =        (int *)  c_chara_real_array_icou(f_cr_array->f_self);
	f_cr_array->f_cctls =       (char *) c_chara_real_array_c_tbl(f_cr_array->f_self);
	f_cr_array->f_rctls =       (double *) c_chara_real_array_r_tbl(f_cr_array->f_self);
	
	c_chara_item_clength(f_cr_array->f_block_name, f_cr_array->f_namelength);
	f_cr_array->c_block_name = alloc_string((long) f_cr_array->f_namelength[0]);
	strngcopy_w_length(f_cr_array->c_block_name, f_cr_array->f_namelength[0], 
					   f_cr_array->f_block_name);
	
	f_cr_array->c_charavalue = (char **) malloc(f_cr_array->f_num[0] * sizeof(char *));
	if(f_cr_array->c_charavalue == NULL){
		printf("malloc error for f_cr_array->c_charavalue \n");
		exit(0);
	};
	if((f_cr_array->f_clength = (int *) calloc(f_cr_array->f_num[0], sizeof(int))) == NULL){
		printf("malloc error for f_cr_array->f_clength\n");
		exit(0);
	}
	
	int i;
	int flen = 255;
	for(i=0;i<f_cr_array->f_num[0];i++){
		c_chara_item_clength(&f_cr_array->f_cctls[i*flen], &f_cr_array->f_clength[i]);
		f_cr_array->c_charavalue[i] = alloc_string((long) f_cr_array->f_clength[i]);
		strngcopy_w_length(f_cr_array->c_charavalue[i], f_cr_array->f_clength[i], 
						   &f_cr_array->f_cctls[i*flen]);
	};
	
	printf("f_cr_array->f_self %p \n", f_cr_array->f_self);
	printf("f_cr_array->c_block_name %s %d\n", f_cr_array->c_block_name, f_cr_array->f_num[0]);
	for(i=0;i<f_cr_array->f_num[0];i++){
		printf("%d f_cr_array->c_charavalue %d %p %s %le\n", i, 
			   f_cr_array->f_clength[i], f_cr_array->c_charavalue[i],
			   f_cr_array->c_charavalue[i], f_cr_array->f_rctls[i]);
	}
	
	return f_cr_array;
}


void dealloc_f_ctl_cr_array(struct f_ctl_cr_array *f_cr_array)
{
	int i;
	for(i=0;i<f_cr_array->f_num[0];i++){free(f_cr_array->c_charavalue[i]);};
	free(f_cr_array->c_charavalue);
	free(f_cr_array->f_clength);
	free(f_cr_array->c_block_name);
	
	f_cr_array->f_rctls = NULL;
	f_cr_array->f_cctls = NULL;
	f_cr_array->f_icou = NULL;
	f_cr_array->f_num = NULL;
	f_cr_array->f_block_name = NULL;
	f_cr_array->f_self = NULL;
	return;
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
	c_chara_item_clength(f_plt->f_block_name, f_plt->f_namelength);
	
	f_plt->c_block_name = alloc_string((long) f_plt->f_namelength[0]);
	strngcopy_w_length(f_plt->c_block_name, f_plt->f_namelength[0], 
					   f_plt->f_block_name);
	printf("f_plt->f_block_name %d %s\n", f_plt->f_namelength[0], f_plt->f_block_name);
	printf("f_plt->c_block_name %s\n", f_plt->c_block_name);
	
	
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
	
	f_plt->label_file_format_list = init_control_labels_f(num_file_fmt_items_f, 
														  set_file_fmt_items_f);
	check_control_labels_f(f_plt->label_file_format_list);
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
	GtkWidget *label = gtk_label_new(f_citem->c_block_name);
	
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

GtkWidget * draw_file_format_select_hbox(struct control_labels_f *label_file_format_list, 
										 struct f_ctl_chara_item * f_citem, GtkWidget *window){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	if(f_citem->f_iflag[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	g_signal_connect(G_OBJECT(checkbox), "toggled", 
                     G_CALLBACK(cb_check_toggle), (gpointer) f_citem->f_iflag);
	GtkWidget *label = gtk_label_new(f_citem->c_block_name);
	
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	GtkWidget *file_formats_tree_view
			= create_control_flags_tree_view(label_file_format_list);
	add_control_combobox_vbox(f_citem->f_charavalue, f_citem->c_charavalue, 
							  label_file_format_list, 
							  file_formats_tree_view, vbox);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
	return hbox;
}


GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_plt->f_debug_flag_ctl, window);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_plt->f_sph_file_prefix, window);
	GtkWidget *hbox_c3 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_sph_file_fmt_ctl, window);
	/*
	GtkWidget *hbox_c4 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_prefix, window);
	GtkWidget *hbox_c5 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c6 = draw_chara_item_entry_hbox(f_plt->f_restart_file_prefix, window);
	GtkWidget *hbox_c7 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_restart_file_fmt_ctl, window);
	GtkWidget *hbox_c8 = draw_chara_item_entry_hbox(f_plt->f_field_file_prefix, window);
	GtkWidget *hbox_c9 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_field_file_fmt_ctl, window);
	/*
 	GtkWidget *hbox_c10 = draw_chara_item_entry_hbox(f_plt->f_spectr_field_file_prefix, window);
	GtkWidget *hbox_c11 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_spectr_field_fmt_ctl, window);
	GtkWidget *hbox_c12 = draw_chara_item_entry_hbox(f_plt->f_coriolis_int_file_name, window);
	GtkWidget *hbox_c13 = draw_chara_item_entry_hbox(f_plt->f_coriolis_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c14 = draw_chara_item_entry_hbox(f_plt->f_bc_data_file_name_ctl, window);
	GtkWidget *hbox_c15 = draw_chara_item_entry_hbox(f_plt->f_radial_data_file_name_ctl, window);
	/*
	GtkWidget *hbox_c16 = draw_chara_item_entry_hbox(f_plt->f_interpolate_sph_to_fem, window);
	GtkWidget *hbox_c17 = draw_chara_item_entry_hbox(f_plt->f_interpolate_fem_to_sph, window);
	GtkWidget *hbox_c18 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_itp_file_fmt_ctl, window);
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
	
	GtkWidget *expand_PLT = draw_control_block(f_plt->c_block_name, f_plt->f_iflag,
											   560, 280, window, vbox_plt);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_PLT, FALSE, FALSE, 0);
	return vbox_out;
};
