/*
//  ctl_data_platforms_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include <string.h>
#include <stdio.h>
#include <gtk/gtk.h>

#include "skip_comment_c.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_control_label_from_f.h"
#include "control_combobox_GTK.h"
#include "kemoview_gtk_routines.h"

#ifndef CTL_DATA_PLATFORMS_GTK_
#define CTL_DATA_PLATFORMS_GTK_

extern void * c_plt_block_name(void *f_plt);
extern void * c_plt_iflag(void *f_plt);
extern void * c_plt_ndomain_ctl(void *f_plt);
extern void * c_plt_num_smp_ctl(void *f_plt);
extern void * c_plt_debug_flag_ctl(void *f_plt);
extern void * c_plt_sph_file_prefix(void *f_plt);
extern void * c_plt_mesh_file_prefix(void *f_plt);
extern void * c_plt_restart_file_prefix(void *f_plt);
extern void * c_plt_field_file_prefix(void *f_plt);
extern void * c_plt_spectr_field_file_prefix(void *f_plt);
extern void * c_plt_coriolis_int_file_name(void *f_plt);
extern void * c_plt_bc_data_file_name_ctl(void *f_plt);
extern void * c_plt_radial_data_file_name_ctl(void *f_plt);
extern void * c_plt_interpolate_sph_to_fem(void *f_plt);
extern void * c_plt_interpolate_fem_to_sph(void *f_plt);
extern void * c_plt_rayleigh_spectr_dir(void *f_plt);
extern void * c_plt_rayleigh_field_dir(void *f_plt);
extern void * c_plt_sph_file_fmt_ctl(void *f_plt);
extern void * c_plt_mesh_file_fmt_ctl(void *f_plt);
extern void * c_plt_restart_file_fmt_ctl(void *f_plt);
extern void * c_plt_field_file_fmt_ctl(void *f_plt);
extern void * c_plt_spectr_field_fmt_ctl(void *f_plt);
extern void * c_plt_itp_file_fmt_ctl(void *f_plt);
extern void * c_plt_coriolis_file_fmt_ctl(void *f_plt);
extern void * c_plt_del_org_data_ctl(void *f_plt);

extern void * c_chara_item_clength(void *f_ctl, int *length);

extern void * c_chara_item_block_name(void *f_ctl);
extern void * c_chara_item_iflag(void *f_ctl);
extern void * c_chara_item_charavalue(void *f_ctl);

extern void * c_chara_array_block_name(void *f_ctl);
extern void * c_chara_array_num(void *f_ctl);
extern void * c_chara_array_icou(void *f_ctl);
extern void * c_chara_array_c_tbl(void *f_ctl);


extern void * c_chara_real_item_block_name(void *f_ctl);
extern void * c_chara_real_item_iflag(void *f_ctl);
extern void * c_chara_real_item_charavalue(void *f_ctl);
extern void * c_chara_real_item_realvalue(void *f_ctl);

extern void * c_chara_real_array_block_name(void *f_ctl);
extern void * c_chara_real_array_num(void *f_ctl);
extern void * c_chara_real_array_icou(void *f_ctl);
extern void * c_chara_real_array_c_tbl(void *f_ctl);
extern void * c_chara_real_array_r_tbl(void *f_ctl);



extern void load_chara_from_c(void *c_ctl);

struct f_ctl_chara_item{
	void * f_self;
	char * f_block_name;
	int * f_iflag;
	char * f_charavalue;
	
	char * c_block_name;
	
	char * c_charavalue;
};

struct f_ctl_chara_array{
	void * f_self;
	char * f_block_name;
	int * f_num;
	int * f_icou;
	char * f_cctls;
	
	char * c_block_name;
	
	char ** c_charavalue;
};

struct c_array_views{
    int index_c_array;
    GtkWidget *c_array_tree_view;
    GtkWidget *c_array_default_view;
    
    struct chara_clist *c_array_clist;
};

struct f_ctl_cr_item{
	void * f_self;
	char * f_block_name;
	int * f_iflag;
	char * f_charavalue;
	double * f_realvalue;
	
	char * c_block_name;
	
	char * c_charavalue;
};

struct f_ctl_cr_array{
	void * f_self;
	char * f_block_name;
	int * f_num;
	int * f_icou;
	char * f_cctls;
	double * f_rctls;
	
	char * c_block_name;
	char ** c_charavalue;
};

struct cr_array_views{
    int index_cr_array;
    GtkWidget *cr_array_tree_view;
    GtkWidget *cr_array_default_view;
    
    struct chara_real_clist *cr_array_clist;
};


struct f_platform_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	char * c_block_name;
	
	void * f_ndomain_ctl;
	void * f_num_smp_ctl;
	struct f_ctl_chara_item * f_debug_flag_ctl;
	struct f_ctl_chara_item * f_sph_file_prefix;
	struct f_ctl_chara_item * f_mesh_file_prefix;
	struct f_ctl_chara_item * f_restart_file_prefix;
	struct f_ctl_chara_item * f_field_file_prefix;
	struct f_ctl_chara_item * f_spectr_field_file_prefix;
	struct f_ctl_chara_item * f_coriolis_int_file_name;
	struct f_ctl_chara_item * f_bc_data_file_name_ctl;
	struct f_ctl_chara_item * f_radial_data_file_name_ctl;
	struct f_ctl_chara_item * f_interpolate_sph_to_fem;
	struct f_ctl_chara_item * f_interpolate_fem_to_sph;
	struct f_ctl_chara_item * f_rayleigh_spectr_dir;
	struct f_ctl_chara_item * f_rayleigh_field_dir;
	struct f_ctl_chara_item * f_sph_file_fmt_ctl;
	struct f_ctl_chara_item * f_mesh_file_fmt_ctl;
	struct f_ctl_chara_item * f_restart_file_fmt_ctl;
	struct f_ctl_chara_item * f_field_file_fmt_ctl;
	struct f_ctl_chara_item * f_spectr_field_fmt_ctl;
	struct f_ctl_chara_item * f_itp_file_fmt_ctl;
	struct f_ctl_chara_item * f_coriolis_file_fmt_ctl;
	struct f_ctl_chara_item * f_del_org_data_ctl;
	
	struct control_labels_f *label_file_format_list;
};

char * strngcopy_from_f(char * f_char);


struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent),
												void *f_parent);
void dealloc_f_ctl_chara_item(struct f_ctl_chara_item *f_citem);

struct f_ctl_chara_array * init_f_ctl_chara_array(void *(*c_load_self)(void *f_parent),
												  void *f_parent);
void dealloc_f_ctl_chara_array(struct f_ctl_chara_array *f_carray);

struct c_array_views * init_c_array_views(struct f_ctl_chara_array *f_carray);
void dealloc_c_array_views(struct c_array_views *c_array_vws);

struct cr_array_views * init_cr_array_views(struct f_ctl_cr_array *f_cr_array);
void dealloc_cr_array_views(struct cr_array_views *cr_array_vws);



struct f_ctl_cr_item * init_f_ctl_cr_item(void *(*c_load_self)(void *f_parent), 
											 void *f_parent);
void dealloc_f_ctl_cr_item(struct f_ctl_cr_item *f_cr_item);

struct f_ctl_cr_array * init_f_ctl_cr_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent);
void dealloc_f_ctl_cr_array(struct f_ctl_cr_array *f_cr_array);


struct f_platform_control * init_f_platform_control(void *(*c_load_self)(void *f_parent), void *f_parent);
void dealloc_f_ctl_chara_array(struct f_ctl_chara_array *f_carray);

void cb_chara_ctl_item(GtkEntry *entry, gpointer data);
void cb_check_toggle(GtkWidget *widget, gpointer iflag_ptr);

GtkWidget * draw_control_block(const char * title, int *iflag_ptr, 
							   int width, int height,
							   GtkWidget *window, GtkWidget *box_in);
GtkWidget * draw_control_block_w_file_switch(const char * title, int *iflag_ptr, 
							   char *file_name, int width, int height,
							   GtkWidget *window, GtkWidget *box_in);

GtkWidget * draw_chara_item_entry_hbox(struct f_ctl_chara_item * f_citem, GtkWidget *window);
GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window);

#endif    /* CTL_DATA_PLATFORMS_GTK_ */
