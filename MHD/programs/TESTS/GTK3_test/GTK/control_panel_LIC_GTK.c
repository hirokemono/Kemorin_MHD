/*
//  control_panel_LIC_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_LIC_GTK.h"

extern void * set_file_fmt_items_f(void *fmt_names_c);

struct LIC_GTK_widgets * init_LIC_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                                              struct chara_int2_clist *f_field_ctl)
{
	struct LIC_GTK_widgets *lic_vws 
			= (struct LIC_GTK_widgets *) malloc(sizeof(struct LIC_GTK_widgets));
	if(lic_vws == NULL){
		printf("malloc error for LIC_GTK_widgets\n");
		exit(0);
    };
	lic_vws->f_repart_vws
			= (struct VIZ_repartition_widgets *) malloc(sizeof(struct VIZ_repartition_widgets));
	if(lic_vws->f_repart_vws == NULL){
		printf("malloc error for VIZ_repartition_widgets\n");
		exit(0);
    };
    
    lic_vws->lic_pvr_vws = init_PVR_GTK_widgets(f_pvr_ctl, f_field_ctl);
    lic_vws->label_field_list = lic_vws->lic_pvr_vws->label_field_list;
    lic_vws->label_dir_list = lic_vws->lic_pvr_vws->label_dir_list;
    
    lic_vws->f_repart_vws->label_field_list = lic_vws->label_field_list;
    lic_vws->f_repart_vws->label_file_format_list
            = init_f_ctl_chara_array(set_file_fmt_items_f, NULL);
    return lic_vws;
};
void dealloc_LIC_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                             struct LIC_GTK_widgets *lic_vws){
    lic_vws->label_field_list = NULL;
    lic_vws->label_dir_list = NULL;
    
    dealloc_PVR_GTK_widgets(f_pvr_ctl, lic_vws->lic_pvr_vws);
    free(lic_vws->f_repart_vws);
    free(lic_vws);
}


struct f_VIZ_LIC_PVR_ctl * init_f_VIZ_LIC_PVR_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_LIC_PVR_ctl *f_lic_ctl = init_f_VIZ_LIC_PVR_ctl(idx, f_parent);
    struct LIC_GTK_widgets *lic_vws = init_LIC_GTK_widgets(f_lic_ctl->f_lic_pvr_ctl, 
                                                          f_fld_ctl->f_field_ctl);
    f_lic_ctl->f_lic_pvr_ctl->void_panel = (void *) lic_vws->lic_pvr_vws;
    f_lic_ctl->void_panel = (void *) lic_vws;
    return f_lic_ctl;
}
void * dealloc_f_VIZ_LIC_PVR_ctl_GTK(void *void_in){
    struct f_VIZ_LIC_PVR_ctl *f_lic_ctl = (struct f_VIZ_LIC_PVR_ctl *) void_in;
    dealloc_LIC_GTK_widgets(f_lic_ctl->f_lic_pvr_ctl, 
                            (struct LIC_GTK_widgets *) f_lic_ctl->void_panel);
    dealloc_f_VIZ_LIC_PVR_ctl(f_lic_ctl);
    return NULL;
}


static GtkWidget * draw_LIC_noise_ctl_vbox(struct f_LIC_noise_ctl *f_noise_ctl, GtkWidget *window){
	GtkWidget *vbox_v_nze = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_noise_ctl->f_noise_type_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_noise_ctl->f_noise_file_name_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_noise_ctl->f_noise_file_format_ctl);
    GtkWidget *hbox_4 = draw_int_item_entry_hbox(f_noise_ctl->f_noise_resolution_ctl);
    GtkWidget *hbox_5 = draw_int_item_entry_hbox(f_noise_ctl->f_noise_stepping_ctl);
    
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_noise_ctl->f_noise_cube_size_ctl);
    GtkWidget *hbox_7 = draw_real_item_entry_hbox(f_noise_ctl->f_noise_deltax_ctl);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_nze), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_nze), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_nze), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_nze), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_nze), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_nze), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_nze), hbox_7,  FALSE, FALSE, 0);
    
    GtkWidget *expander_nze = draw_control_block_w_file_switch(f_noise_ctl->c_block_name,
                                                               f_noise_ctl->f_iflag,
                                                               f_noise_ctl->noise_ctl_file_name,
                                                               window, vbox_v_nze);
    return expander_nze;
};

static GtkWidget * draw_LIC_kernel_ctl_vbox(struct f_LIC_kernel_ctl *f_kernel_ctl, GtkWidget *window){
	GtkWidget *vbox_v_knl = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_kernel_ctl->f_kernel_type_ctl);
    GtkWidget *hbox_2 = draw_int_item_entry_hbox(f_kernel_ctl->f_kernel_resolution_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_kernel_ctl->f_kernel_peak_ctl);
    GtkWidget *hbox_4 = draw_real_item_entry_hbox(f_kernel_ctl->f_kernel_sigma_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_kernel_ctl->f_trace_length_mode_ctl);
    
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_kernel_ctl->f_half_length_ctl);
    GtkWidget *hbox_7 = draw_int_item_entry_hbox(f_kernel_ctl->f_max_trace_count_ctl);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_knl), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_knl), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_knl), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_knl), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_knl), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_knl), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_knl), hbox_7,  FALSE, FALSE, 0);
    
    GtkWidget *expander_knl = draw_control_block_w_file_switch(f_kernel_ctl->c_block_name,
                                                               f_kernel_ctl->f_iflag,
                                                               f_kernel_ctl->kernel_ctl_file_name,
                                                               window, vbox_v_knl);
    return expander_knl;
};


static GtkWidget * draw_viz_each_lic_lic_ctl_vbox(struct f_VIZ_LIC_ctl *f_lic_lic_ctl,
                                                  struct LIC_GTK_widgets *lic_vws,
                                                  GtkWidget *window){
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_LIC_field_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_color_field_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_color_component_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_opacity_field_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_opacity_component_ctl);
    
    GtkWidget *hbox_6 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_vr_sample_mode_ctl);
    GtkWidget *hbox_7 = draw_real_item_entry_hbox(f_lic_lic_ctl->f_step_size_ctl);
    GtkWidget *hbox_8 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_normalization_type_ctl);
    GtkWidget *hbox_9 = draw_real_item_entry_hbox(f_lic_lic_ctl->f_normalization_value_ctl);
    
    GtkWidget *expander_nze =  draw_LIC_noise_ctl_vbox(f_lic_lic_ctl->f_noise_ctl, window);
    GtkWidget *expander_knl =  draw_LIC_kernel_ctl_vbox(f_lic_lic_ctl->f_kernel_ctl, 
                                                       window);
    GtkWidget *expander_rep =  draw_VIZ_repartition_ctl_vbox(f_lic_lic_ctl->f_repart_ctl, 
                                                             lic_vws->f_repart_vws, window);
    
    GtkWidget *expander_mask = draw_array_block_ctl_vbox(f_lic_lic_ctl->f_mask_ctl,
                                                         (void *) lic_vws->f_repart_vws->label_field_list,
                                                         c_append_multi_mask_ctls,
                                                         c_delete_multi_mask_ctls,
                                                         (void *) init_f_VIZ_masking_ctl_GTK,
                                                         (void *) dealloc_f_VIZ_masking_ctl_GTK,
                                                         (void *) draw_viz_each_masking_vbox,
                                                         lic_vws->masking_Wgts, window);
	
    
	GtkWidget *vbox_v_lic = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_9,  FALSE, FALSE, 0);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expander_nze,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expander_knl,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expander_rep,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expander_mask, FALSE, FALSE, 0);
    
    GtkWidget *expand_v_lic = draw_control_block(duplicate_underscore(f_lic_lic_ctl->c_block_name),
                                                 f_lic_lic_ctl->f_iflag,
                                                 window, vbox_v_lic);
    return expand_v_lic;
};

GtkWidget * draw_viz_each_lic_ctl_vbox(char *label_name, struct f_VIZ_LIC_PVR_ctl *f_lic_item, 
                                       GtkWidget *window){
    struct LIC_GTK_widgets *lic_vws = (struct LIC_GTK_widgets *) f_lic_item->void_panel;
    
    GtkWidget *expand_lic_pvr = draw_viz_each_pvr_ctl_vbox(f_lic_item->f_lic_pvr_ctl->c_block_name,
                                                           f_lic_item->f_lic_pvr_ctl, window);
    GtkWidget *expand_lic_lic = draw_viz_each_lic_lic_ctl_vbox(f_lic_item->f_lic_lic_ctl,
                                                               lic_vws, window);
    
	GtkWidget *vbox_v_lic = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expand_lic_pvr,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expand_lic_lic,  FALSE, FALSE, 0);
    
    GtkWidget *expand_v_lic = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_lic_item->f_iflag,
															   f_lic_item->lic_ctl_file_name,
															   window, vbox_v_lic);
    return expand_v_lic;
};

