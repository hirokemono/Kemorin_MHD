/*
//  control_panel_VIZ_repartition_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_VIZ_repartition_GTK.h"

extern void * c_link_scalar_dir_list_to_ctl(void);
extern void * c_link_vector_dir_list_to_ctl(void);
extern void * c_link_stensor_dir_list_to_ctl(void);
extern void * c_link_atensor_dir_list_to_ctl(void);


struct VIZ_masking_GTK_widgets * init_VIZ_masking_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
    struct VIZ_masking_GTK_widgets *masking_vws
            = (struct VIZ_masking_GTK_widgets *) malloc(sizeof(struct VIZ_masking_GTK_widgets));
    if(masking_vws == NULL){
        printf("malloc error for masking_vws\n");
        exit(0);
    };
    masking_vws->label_field_list = f_field_ctl;
    masking_vws->label_dir_list = init_field_label_array(c_link_scalar_dir_list_to_ctl());
    append_field_label_array(c_link_vector_dir_list_to_ctl(),
                             masking_vws->label_dir_list);
    append_field_label_array(c_link_stensor_dir_list_to_ctl(),
                             masking_vws->label_dir_list);
    return masking_vws;
};

void dealloc_VIZ_masking_GTK_widgets(struct VIZ_masking_GTK_widgets *masking_vws){
    dealloc_chara_int2_clist(masking_vws->label_field_list);
    dealloc_chara2_int_clist(masking_vws->label_dir_list);
    free(masking_vws);
    return;
}

struct f_LIC_masking_ctl * init_f_VIZ_masking_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_LIC_masking_ctl *f_masking_ctl = init_f_LIC_masking_ctl(idx, f_parent);
    f_masking_ctl->void_panel = (void *) init_VIZ_masking_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_masking_ctl;
}

void * dealloc_f_VIZ_masking_ctl_GTK(void *void_in){
    struct f_LIC_masking_ctl *f_masking_ctl = (struct f_LIC_masking_ctl *) void_in;
    dealloc_VIZ_masking_GTK_widgets((struct VIZ_masking_GTK_widgets *) f_masking_ctl->void_panel);
    dealloc_f_LIC_masking_ctl(f_masking_ctl);
    return NULL;
}

GtkWidget * draw_viz_each_masking_vbox(char *label_name, struct f_LIC_masking_ctl *f_mask_ctl, 
                                       GtkWidget *window){
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_mask_ctl->f_mask_type_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_mask_ctl->f_field_name_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_mask_ctl->f_component_ctl);
    GtkWidget *hbox_4 = draw_real2_item_entry_hbox(f_mask_ctl->f_mask_range_ctl);
    
    GtkWidget *vbox_mask = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_mask), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mask), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mask), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mask), hbox_4,  FALSE, FALSE, 0);
    
    GtkWidget *expand_mask = draw_control_block(duplicate_underscore(label_name),
                                                f_mask_ctl->f_iflag,
                                                window, vbox_mask);
    return expand_mask;
};


static GtkWidget * draw_new_partition_ctl_vbox(struct f_new_patition_ctl *f_new_part_ctl, 
                                               struct VIZ_repartition_widgets *f_repart_vws,
                                               GtkWidget *window){
	GtkWidget *vbox_v_npt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_new_part_ctl->f_repart_table_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_new_part_ctl->f_repart_table_fmt_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_new_part_ctl->f_partition_reference_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_new_part_ctl->f_trace_count_head_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_new_part_ctl->f_trace_count_fmt_ctl);
    
    GtkWidget *hbox_6 = add_ci_list_box_w_addbottun(f_new_part_ctl->f_ndomain_section_ctl,
                                                    f_repart_vws->f_ndomain_section_tree);
    
    GtkWidget *hbox_7 = draw_int_item_entry_hbox(f_new_part_ctl->f_ratio_of_grouping_ctl);
    GtkWidget *hbox_8 = draw_int_item_entry_hbox(f_new_part_ctl->f_sleeve_level_ctl);
    GtkWidget *hbox_9 = draw_real_item_entry_hbox(f_new_part_ctl->f_weight_to_previous_ctl);
    
    GtkWidget *hbox_10 = draw_chara_item_entry_hbox(f_new_part_ctl->f_masking_switch_ctl);
    GtkWidget *hbox_11 = draw_real_item_entry_hbox(f_new_part_ctl->f_masking_weight_ctl);
    GtkWidget *hbox_12 = draw_real_item_entry_hbox(f_new_part_ctl->f_power_of_volume_ctl);
    
	GtkWidget *expand_mask = draw_array_block_ctl_vbox(f_new_part_ctl->f_mask_ctl,
                                                       (void *) f_repart_vws->label_field_list,
                                                       c_append_multi_mask_ctls,
                                                       c_delete_multi_mask_ctls,
                                                       (void *) init_f_VIZ_masking_ctl_GTK,
                                                       (void *) dealloc_f_VIZ_masking_ctl_GTK,
                                                       (void *) draw_viz_each_masking_vbox,
                                                       f_repart_vws->masking_Wgts, window);
	
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_10, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), hbox_12, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_npt), expand_mask, FALSE, FALSE, 0);
    
    GtkWidget *expander_npt = draw_control_block(f_new_part_ctl->c_block_name,
                                                 f_new_part_ctl->f_iflag,
                                                 window, vbox_v_npt);
    return expander_npt;
};

static GtkWidget * draw_FEM_sleeve_control_vbox(struct f_FEM_sleeve_control *f_Fsleeve_ctl,
                                                GtkWidget *window){
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_Fsleeve_ctl->f_sleeve_extension_mode_ctl);
    GtkWidget *hbox_2 = draw_int_item_entry_hbox(f_Fsleeve_ctl->f_sleeve_level_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_Fsleeve_ctl->f_sleeve_size_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_Fsleeve_ctl->f_ref_vector_ctl);
    
	GtkWidget *vbox_v_slv = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_slv), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_slv), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_slv), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_slv), hbox_4,  FALSE, FALSE, 0);
    
    GtkWidget *expander_slv = draw_control_block(f_Fsleeve_ctl->c_block_name,
                                                 f_Fsleeve_ctl->f_iflag,
                                                 window, vbox_v_slv);
    return expander_slv;
};


GtkWidget * draw_VIZ_repartition_ctl_vbox(struct f_VIZ_repartition_ctl *f_repart_ctl, 
                                          struct VIZ_repartition_widgets *f_repart_vws,
                                          GtkWidget *window){
	GtkWidget *vbox_v_rep = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *expander_plt =  draw_platform_control_vbox(f_repart_ctl->f_viz_plt, 
                                                          f_repart_vws->label_file_format_list,
                                                          window);
    GtkWidget *expander_fmh =  draw_sph_FEM_mesh_file_vbox(f_repart_ctl->f_Fmesh_ctl, 
                                                           window);
    GtkWidget *expander_npt =  draw_new_partition_ctl_vbox(f_repart_ctl->f_new_part_ctl, 
                                                           f_repart_vws,
                                                           window);
    GtkWidget *expander_slv =  draw_FEM_sleeve_control_vbox(f_repart_ctl->f_Fsleeve_ctl, 
                                                            window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_rep), expander_plt,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_rep), expander_fmh,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_rep), expander_npt,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_rep), expander_slv,  FALSE, FALSE, 0);
    
    GtkWidget *expander_rep = draw_control_block_w_file_switch(f_repart_ctl->c_block_name,
                                                               f_repart_ctl->f_iflag,
                                                               f_repart_ctl->repart_ctl_file_name,
                                                               window, vbox_v_rep);
    return expander_rep;
};
