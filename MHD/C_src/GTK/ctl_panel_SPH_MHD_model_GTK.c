/*
//  ctl_panel_SPH_MHD_model_GTK.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#include "ctl_panel_SPH_MHD_model_GTK.h"

GtkWidget *make_mhd_model_ctl_hbox(const char *label_hd, struct mhd_model_control_c *model_ctl,
                                   GtkWidget *window){
	int i;
	
	GtkWidget *hbox_3[NLBL_MHD_MODEL_CTL];
	
    GtkWidget *vbox_3;
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
    char *c_label = (char *)calloc(KCHARA_C, sizeof(char));
	struct field_views *fields_vws = init_field_views_GTK(model_ctl->fld_ctl);
    
    fields_vws->used_tree_view = create_field_tree_view(fields_vws->all_fld_list, fields_vws->fld_ctl_gtk);
    fields_vws->unused_field_tree_view = create_unused_field_tree_views(fields_vws->all_fld_list);
    create_direction_tree_views(fields_vws);
    
    vbox_3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_field_selection_box(fields_vws, window, vbox_3);
    
	get_label_mhd_model_ctl(0, c_label);
	hbox_3[0] = make_expand_ctl_hbox(c_label, &model_ctl->fld_ctl->iflag_use,
                                     700, vbox_3);
	
	get_label_mhd_model_ctl(1, c_label);
	hbox_3[1] = make_empty_ctl_hbox(c_label, &model_ctl->evo_ctl->iflag_use);
	
	get_label_mhd_model_ctl(2, c_label);
	hbox_3[2] = make_empty_ctl_hbox(c_label, &model_ctl->earea_ctl->iflag_use);
	
	get_label_mhd_model_ctl(3, c_label);
	hbox_3[3] = make_empty_ctl_hbox(c_label, &model_ctl->nbc_ctl->iflag_use);
	
	get_label_mhd_model_ctl(4, c_label);
	hbox_3[4] = make_empty_ctl_hbox(c_label, &model_ctl->sbc_ctl->iflag_use);
	
	get_label_mhd_model_ctl(5, c_label);
	hbox_3[5] = make_empty_ctl_hbox(c_label, &model_ctl->frc_ctl->iflag_use);
	
	get_label_mhd_model_ctl(6, c_label);
	hbox_3[6] = make_empty_ctl_hbox(c_label, &model_ctl->dless_ctl->iflag_use);
	
	get_label_mhd_model_ctl(7, c_label);
	hbox_3[7] = make_empty_ctl_hbox(c_label, &model_ctl->eqs_ctl->iflag_use);
	
	get_label_mhd_model_ctl(8, c_label);
	hbox_3[8] = make_empty_ctl_hbox(c_label, &model_ctl->g_ctl->iflag_use);
	
	get_label_mhd_model_ctl(9, c_label);
	hbox_3[9] = make_empty_ctl_hbox(c_label, &model_ctl->cor_ctl->iflag_use);
	
	get_label_mhd_model_ctl(10, c_label);
	hbox_3[10] = make_empty_ctl_hbox(c_label, &model_ctl->mcv_ctl->iflag_use);
	
	get_label_mhd_model_ctl(11, c_label);
	hbox_3[11] = make_empty_ctl_hbox(c_label, &model_ctl->reft_ctl->iflag_use);
	
	get_label_mhd_model_ctl(12, c_label);
	hbox_3[12] = make_empty_ctl_hbox(c_label, &model_ctl->refc_ctl->iflag_use);
	
	get_label_mhd_model_ctl(13, c_label);
	hbox_3[13] = make_empty_ctl_hbox(c_label, &model_ctl->sgs_ctl->iflag_use);

    for(i=0;i<NLBL_MHD_MODEL_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], TRUE, TRUE, 0);
	};
	
	hbox = make_expand_ctl_hbox(label_hd, &model_ctl->iflag_use, 600, vbox_1);
	return hbox;
}

GtkWidget *make_mhd_control_ctl_hbox(const char *label_hd, 
			struct sph_mhd_control_control_c *control_ctl){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_MHD_CONTROL_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_mhd_control_ctl(0, c_label);
	hbox_3[0] = make_empty_ctl_hbox(c_label, &control_ctl->tctl->iflag_use);
	
	get_label_mhd_control_ctl(1, c_label);
	hbox_3[1] = make_empty_ctl_hbox(c_label, &control_ctl->mrst_ctl->iflag_use);
	
	get_label_mhd_control_ctl(2, c_label);
	hbox_3[2] = make_empty_ctl_hbox(c_label, &control_ctl->mevo_ctl->iflag_use);

    for(i=0;i<NLBL_MHD_CONTROL_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], TRUE, TRUE, 0);
	};
	
	hbox = make_expand_ctl_hbox(label_hd, &control_ctl->iflag_use, 400, vbox_1);
	return hbox;
}
