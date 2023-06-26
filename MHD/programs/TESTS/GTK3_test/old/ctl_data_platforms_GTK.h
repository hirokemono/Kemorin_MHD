/*
//  ctl_data_platforms_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include <string.h>
#include <gtk/gtk.h>

#include "skip_comment_c.h"
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

extern void * c_chara_item_block_name(void *f_plt);
extern void * c_chara_item_iflag(void *f_plt);
extern void * c_chara_item_clength(void *f_plt, int *length);
extern void * c_chara_item_charavalue(void *f_plt);

struct f_ctl_chara_item{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	int f_clength[1];
	char * f_charavalue;
};

struct f_platform_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
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
};

struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent), void *f_parent);

struct f_platform_control * init_f_platform_control(void *(*c_load_self)(void *f_parent), void *f_parent);

void cb_chara_ctl_item(GtkEntry *entry, gpointer data);
void cb_check_toggle(GtkWidget *widget, gpointer iflag_ptr);

GtkWidget * draw_control_block(const char * title, int *iflag_ptr, 
							   int width, int height,
							   GtkWidget *window, GtkWidget *box_in);
GtkWidget * draw_chara_item_entry_hbox(struct f_ctl_chara_item * f_citem, GtkWidget *window);
GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window);

#endif    /* CTL_DATA_PLATFORMS_GTK_ */
