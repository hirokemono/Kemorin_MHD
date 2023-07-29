/*
//  control_panel_SGS_MHD_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_SGS_MHD_GTK.h"

extern void * set_file_fmt_items_f(void *fmt_names_c);

struct main_widgets init_main_widgets(){
	struct main_widgets *mWidgets = (struct main_widgets *) malloc(sizeof(struct main_widgets));
		printf("mWidgets %p\n", mWidgets);
	if(mWidgets == NULL){
		printf("malloc error for mWidgets\n");
		exit(0);
    };
    
    return mWidgets;
};


void MHD_control_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl, 
						  struct main_widgets *mWidgets){
    mWidgets->label_file_format_list = init_f_ctl_chara_array(set_file_fmt_items_f,
                                                              f_MHD_ctl->f_self);
	mWidgets->ctl_MHD_inner_box =   gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget * vbox_plt_c = draw_platform_control_vbox(f_MHD_ctl->f_plt,
                                                        mWidgets->label_file_format_list,
                                                        window);
	GtkWidget * vbox_plt_o = draw_platform_control_vbox(f_MHD_ctl->f_org_plt,
                                                        mWidgets->label_file_format_list,
                                                        window);
	GtkWidget * vbox_plt_n = draw_platform_control_vbox(f_MHD_ctl->f_new_plt,
                                                        mWidgets->label_file_format_list,
                                                        window);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), vbox_plt_c, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), vbox_plt_o, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), vbox_plt_n, FALSE, FALSE, 0);
	
	GtkWidget *expand_sph_shell = MHD_sph_shell_ctl_expander(window, f_MHD_ctl->f_psph_ctl,
															 mWidgets->f_psph_vws);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_sph_shell, FALSE, FALSE, 0);
	
    GtkWidget *expand_MHD_model = MHD_model_ctl_expander(f_MHD_ctl->f_self, f_MHD_ctl->f_model_ctl,
                                                         mWidgets->model_wgts, window);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_model, FALSE, FALSE, 0);
	
	
    GtkWidget *expand_MHD_control = draw_MHD_control_expand(window, f_MHD_ctl->f_smctl_ctl);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_control, FALSE, FALSE, 0);
	
	GtkWidget *expand_smntr = draw_MHD_sph_monitor_ctls_vbox(f_MHD_ctl->f_smonitor_ctl, 
															 mWidgets->f_lp_vws, window);
	gtk_container_add(GTK_CONTAINER(mWidgets->ctl_MHD_inner_box), expand_smntr);
	
	/*
	GtkWidget *vbox_node_monitor = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_node_monitor = draw_control_block(f_MHD_ctl->f_nmtr_ctl->c_block_name, 
													 f_MHD_ctl->f_nmtr_ctl->f_iflag,
													 window, _node_monitor);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_node_monitor, FALSE, FALSE, 0);
	*/
	
    mWidgets->vizs_Wgts = init_MHD_VIZs_GTK(f_MHD_ctl->f_viz_ctls,
                                            f_MHD_ctl->f_model_ctl->f_fld_ctl);
    mWidgets->dviz_Wgts = init_dynamo_VIZs_GTK(f_MHD_ctl->f_zm_ctls,
                                               f_MHD_ctl->f_model_ctl->f_fld_ctl);
    
    GtkWidget *expand_MHD_viz = MHD_VIZs_ctl_expander(window, f_MHD_ctl->f_viz_ctls,
                                                      f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                      mWidgets->vizs_Wgts);
    GtkWidget *expand_MHD_zm = MHD_dynamo_VIZs_expander(window, f_MHD_ctl->f_zm_ctls, 
                                                        f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                        mWidgets->dviz_Wgts);
    
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_viz, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_zm, FALSE, FALSE, 0);
	
    mWidgets->ctl_MHD_Vbox = wrap_into_scroll_expansion_gtk(f_MHD_ctl->c_block_name, 560, 640,
                                                            window, mWidgets->ctl_MHD_inner_box);
	return;
};

