/*
//  control_panel_4_MHD_BCs_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#ifndef control_panel_4_MHD_BCs_GTK_H_
#define control_panel_4_MHD_BCs_GTK_H_

#include "calypso_GTK.h"
#include "c_ctl_data_MHD_BCs.h"
#include "tree_view_boundary_condition_GTK.h"

struct f_MHD_BCs_tree_views{
	struct boundary_condition_view *bc_mom_vws;
 	struct boundary_condition_view *bc_induction_vws;
	struct boundary_condition_view *bc_temp_vws;
	struct boundary_condition_view *bc_comp_vws;
	struct boundary_condition_view *bc_infinity_vws;
	
	struct boundary_condition_view *bc_press_vws;
	struct boundary_condition_view *bc_magp_vws;
	struct boundary_condition_view *f_vecp_bc_type;
	struct boundary_condition_view *f_current_bc_type;
};

/* prototypes */

GtkWidget * draw_node_bc_ctl_vbox(struct f_MHD_node_bc_control *f_nbc_ctl,
                                  struct f_MHD_BCs_tree_views *bc_nod_bc_vws, 
                                  GtkWidget *window);
GtkWidget * draw_surf_bc_ctl_vbox(struct f_MHD_surf_bc_control *f_sbc_ctl,
                                  struct f_MHD_BCs_tree_views *bc_surf_bc_vws, 
                                  GtkWidget *window);


#endif /* control_panel_4_MHD_BCs_GTK_H_ */
