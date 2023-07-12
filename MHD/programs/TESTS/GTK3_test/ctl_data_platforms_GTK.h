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
#include "t_control_label_from_f.h"
#include "t_control_chara_real_IO.h"
#include "control_combobox_GTK.h"
#include "kemoview_gtk_routines.h"
#include "c_ctl_data_platforms.h"
#include "control_boxes_single_items_GTK.h"
#include "control_panel_real_GTK.h"
#include "control_panel_int_real_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_panel_chara_real_GTK.h"
#include "control_panel_chara_int_GTK.h"
#include "control_panel_chara_GTK.h"
#include "tree_view_real_GTK.h"


#ifndef CTL_DATA_PLATFORMS_GTK_
#define CTL_DATA_PLATFORMS_GTK_


struct f_MHD_sph_resolution_views{
	GtkWidget *f_radius_ctl_tree;
	GtkWidget *f_add_ext_layer_tree;
	GtkWidget *f_radial_grp_ctl_tree;
	GtkWidget *f_radial_layer_list_ctl_tree;
	GtkWidget *f_med_layer_list_ctl_tree;
};

struct f_MHD_sph_subdomain_views{
	GtkWidget *f_ndomain_sph_grid_tree;
	GtkWidget *f_ndomain_legendre_tree;
	GtkWidget *f_ndomain_spectr_tree;
};

struct f_sph_shell_views{
	struct f_MHD_sph_subdomain_views *f_sdctl_vws;
	struct f_MHD_sph_resolution_views *f_spctl_vws;
};



struct f_MHD_mom_eq_views{
	GtkWidget * f_coef_4_intertia_tree;
	GtkWidget * f_coef_4_grad_p_tree;
	GtkWidget * f_coef_4_viscous_tree;
	GtkWidget * f_coef_4_termal_buo_tree;
	GtkWidget * f_coef_4_comp_buo_tree;
	GtkWidget * f_coef_4_Coriolis_tree;
	GtkWidget * f_coef_4_Lorentz_tree;
};

struct f_MHD_induct_eq_views{
	GtkWidget * f_coef_4_magne_evo_tree;
	GtkWidget * f_coef_4_mag_diffuse_tree;
	GtkWidget * f_coef_4_mag_potential_tree;
	GtkWidget * f_coef_4_induction_tree;
};

struct f_MHD_heat_eq_views{
	GtkWidget * f_coef_4_adv_flux_tree;
	GtkWidget * f_coef_4_diffuse_tree;
	GtkWidget * f_coef_4_source_tree;
};

struct f_MHD_equations_views{
	struct f_MHD_mom_eq_views    *f_mom_eq_vws;
	struct f_MHD_induct_eq_views *f_induct_vws;
	struct f_MHD_heat_eq_views   *f_heat_vws;
	struct f_MHD_heat_eq_views   *f_comp_vws;
};



void cb_chara_ctl_item(GtkEntry *entry, gpointer data);
void cb_check_toggle(GtkWidget *widget, gpointer iflag_ptr);

GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window);


GtkWidget *MHD_sph_shell_ctl_expander(GtkWidget *window, struct f_MHD_sph_shell_control * f_psph_ctl, 
									  char * f_fname_psph, struct f_sph_shell_views *f_psph_vws);


GtkWidget * draw_momentum_equation_vbox(struct f_MHD_mom_eq_control *f_mom_eq_ctl, 
										struct f_MHD_mom_eq_views *f_mom_eq_vws, GtkWidget *window);
GtkWidget * draw_induction_equation_vbox(struct f_MHD_induct_eq_control *f_induct_ctl, 
										 struct f_MHD_induct_eq_views *f_induct_vws, GtkWidget *window);
GtkWidget * draw_heat_equation_vbox(struct f_MHD_heat_eq_control *f_heat_ctl, 
									struct f_MHD_heat_eq_views *f_heat_vws, GtkWidget *window);
GtkWidget * draw_MHD_equations_vbox(struct f_MHD_equations_control *f_eqs_ctl, 
									struct f_MHD_equations_views *f_eqs_vws, GtkWidget *window);


#endif    /* CTL_DATA_PLATFORMS_GTK_ */
