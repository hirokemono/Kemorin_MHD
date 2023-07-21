/*
//  control_panel_PSF_ISO_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_PSF_ISO_GTK.h"

struct PSF_GTK_widgets * init_PSF_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
	struct PSF_GTK_widgets *psf_def_vws 
			= (struct PSF_GTK_widgets *) malloc(sizeof(struct PSF_GTK_widgets));
	if(psf_def_vws == NULL){
		printf("malloc error for psf_def_vws\n");
		exit(0);
    };
    psf_def_vws->label_field_list = f_field_ctl;
    psf_def_vws->label_dir_list = init_f_ctl_chara_array(c_link_scalar_dir_list_to_ctl, NULL);
    append_f_ctl_chara_array(c_link_vector_dir_list_to_ctl(NULL),
                             psf_def_vws->label_dir_list);
    append_f_ctl_chara_array(c_link_stensor_dir_list_to_ctl(NULL), 
                             psf_def_vws->label_dir_list);
    psf_def_vws->label_xyz_dir_list = init_f_ctl_chara_array(c_link_xyz_dir_list_to_ctl, NULL);
    
    return psf_def_vws;
};

void dealloc_PSF_GTK_widgets(struct PSF_GTK_widgets *psf_def_vws){
    dealloc_chara_int2_clist(psf_def_vws->label_field_list);
    dealloc_chara_clist(psf_def_vws->label_dir_list);
    dealloc_chara_clist(psf_def_vws->label_xyz_dir_list);
    free(psf_def_vws);
}

struct ISO_GTK_widgets * init_ISO_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
	struct ISO_GTK_widgets *iso_vws 
			= (struct ISO_GTK_widgets *) malloc(sizeof(struct ISO_GTK_widgets));
	if(iso_vws == NULL){
		printf("malloc error for iso_vws\n");
		exit(0);
    };
    iso_vws->label_field_list = f_field_ctl;
    iso_vws->label_dir_list = init_f_ctl_chara_array(c_link_scalar_dir_list_to_ctl, NULL);
    append_f_ctl_chara_array(c_link_vector_dir_list_to_ctl(NULL), iso_vws->label_dir_list);
    append_f_ctl_chara_array(c_link_stensor_dir_list_to_ctl(NULL), iso_vws->label_dir_list);
    return iso_vws;
};

static void dealloc_ISO_GTK_widgets(struct ISO_GTK_widgets *iso_vws){
    dealloc_chara_int2_clist(iso_vws->label_field_list);
    dealloc_chara_clist(iso_vws->label_dir_list);
    free(iso_vws);
}


struct f_VIZ_PSF_ctl * init_f_VIZ_PSF_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_PSF_ctl *f_psf_ctl = init_f_VIZ_PSF_ctl(idx, f_parent);
    f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_psf_ctl;
}
void * dealloc_f_VIZ_PSF_ctl_GTK(void *void_in){
    struct f_VIZ_PSF_ctl *f_psf_ctl = (struct f_VIZ_PSF_ctl *) void_in;
    dealloc_PSF_GTK_widgets((struct PSF_GTK_widgets *) f_psf_ctl->void_panel);
    dealloc_f_VIZ_PSF_ctl(f_psf_ctl);
    return NULL;
}

struct f_VIZ_ISO_ctl * init_f_VIZ_ISO_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_ISO_ctl *f_iso_ctl = init_f_VIZ_ISO_ctl(idx, f_parent);
    f_iso_ctl->void_panel = (void *) init_ISO_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_iso_ctl;
}

void * dealloc_f_VIZ_ISO_ctl_GTK(void *void_in){
    struct f_VIZ_ISO_ctl *f_iso_ctl = (struct f_VIZ_ISO_ctl *) void_in;
    dealloc_ISO_GTK_widgets((struct ISO_GTK_widgets *) f_iso_ctl->void_panel);
    dealloc_f_VIZ_ISO_ctl(f_iso_ctl);
    return NULL;
}


static GtkWidget * draw_psf_fld_ctl_vbox(struct f_VIZ_fld_on_PSF_ctl *f_fld_on_psf_c,
                                         struct chara_int2_clist *label_field_list,
                                         struct chara_clist *label_dir_list,
                                         struct chara2_cbox_table_view *field_output_vws, 
                                         GtkWidget *window){
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_fld_on_psf_c->f_output_type_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_fld_on_psf_c->f_output_value_ctl);
    GtkWidget * expand_1 = c2_list_fld_on_psf_expander(f_fld_on_psf_c->f_field_output_ctl,
                                                       label_field_list, label_dir_list,
                                                       field_output_vws, window);
    
    GtkWidget *vbox_psf_f = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_psf_f), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_f), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_f), expand_1,  FALSE, FALSE, 0);
    
    GtkWidget *expand_psf_f = draw_control_block_w_file_switch(duplicate_underscore(f_fld_on_psf_c->c_block_name),
															   f_fld_on_psf_c->f_iflag,
															   f_fld_on_psf_c->fname_fld_on_psf,
															   window, vbox_psf_f);
    return expand_psf_f;
}


GtkWidget * draw_psf_def_ctl_vbox(struct f_VIZ_PSF_def_ctl *f_psf_def_c,
                                  struct PSF_GTK_widgets *psf_def_vws, GtkWidget *window){
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_psf_def_c->f_section_method_ctl);
    GtkWidget *hbox_2 = cr_list_combobox_expander(f_psf_def_c->f_psf_coefs_ctl, psf_def_vws->label_xyz_dir_list, 
                                                  psf_def_vws->psf_coefs_vws, window);
    GtkWidget *hbox_3 = cr_list_combobox_expander(f_psf_def_c->f_psf_center_ctl, psf_def_vws->label_xyz_dir_list, 
                                                  psf_def_vws->psf_center_vws, window);
    GtkWidget *hbox_4 = cr_list_combobox_expander(f_psf_def_c->f_psf_normal_ctl, psf_def_vws->label_xyz_dir_list, 
                                                  psf_def_vws->psf_normal_vws, window);
    GtkWidget *hbox_5 = cr_list_combobox_expander(f_psf_def_c->f_psf_axis_ctl, psf_def_vws->label_xyz_dir_list, 
                                                  psf_def_vws->psf_axis_vws, window);
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_psf_def_c->f_radius_psf_ctl);
    GtkWidget *hbox_7 = draw_chara_item_entry_hbox(f_psf_def_c->f_psf_group_name_ctl);
    GtkWidget *hbox_8 = add_c_list_box_w_addbottun(f_psf_def_c->f_psf_area_ctl, psf_def_vws->psf_area_view);
    
	GtkWidget *vbox_psf_d = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf_d), hbox_8,  FALSE, FALSE, 0);
    
    GtkWidget *expand_psf_d = draw_control_block_w_file_switch(f_psf_def_c->c_block_name,
															   f_psf_def_c->f_iflag,
															   f_psf_def_c->psf_def_file_name,
															   window, vbox_psf_d);
    return expand_psf_d;
};

static GtkWidget * draw_iso_def_ctl_vbox(struct f_VIZ_ISO_def_ctl *f_iso_def_c, 
                                         struct ISO_GTK_widgets *iso_vws, GtkWidget *window){
    GtkWidget *hbox_1 = draw_field_combobox_hbox(iso_vws->label_field_list, 
                                                 f_iso_def_c->f_isosurf_data_ctl, window);
    GtkWidget *hbox_2 = draw_chara_item_combobox_hbox(iso_vws->label_dir_list, 
                                                      f_iso_def_c->f_isosurf_comp_ctl, window);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_iso_def_c->f_isosurf_value_ctl);
    GtkWidget *hbox_4 = add_c_list_box_w_addbottun(f_iso_def_c->f_iso_area_ctl, iso_vws->iso_area_view);
    
	GtkWidget *vbox_iso_d = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_iso_d), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_iso_d), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_iso_d), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_iso_d), hbox_3,  FALSE, FALSE, 0);
    
    GtkWidget *expand_iso_d = draw_control_block(f_iso_def_c->c_block_name, f_iso_def_c->f_iflag,
                                                 window, vbox_iso_d);
    return expand_iso_d;
};

GtkWidget * draw_viz_each_psf_ctl_vbox(char *label_name, struct f_VIZ_PSF_ctl *f_psf_item, 
                                       GtkWidget *window){
    struct PSF_GTK_widgets *psf_def_vws = (struct PSF_GTK_widgets *) f_psf_item->void_panel;
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_psf_item->f_psf_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_psf_item->f_psf_output_type_ctl);
    
    GtkWidget *expand_psf_d = draw_psf_def_ctl_vbox(f_psf_item->f_psf_def_c, psf_def_vws, window);
    GtkWidget *expand_psf_f = draw_psf_fld_ctl_vbox(f_psf_item->f_fld_on_psf_c, 
                                                    psf_def_vws->label_field_list,
                                                    psf_def_vws->label_dir_list,
                                                    psf_def_vws->field_output_vws, window);
    
    GtkWidget *vbox_v_psf = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_psf), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_psf), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_psf), expand_psf_d,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_psf), expand_psf_f,  FALSE, FALSE, 0);
    GtkWidget *expand_v_psf = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_psf_item->f_iflag,
															   f_psf_item->psf_ctl_file_name,
															   window, vbox_v_psf);
    return expand_v_psf;
};

GtkWidget * draw_viz_each_iso_ctl_vbox(char *label_name, struct f_VIZ_ISO_ctl *f_iso_item, 
                                       GtkWidget *window){
    struct ISO_GTK_widgets *iso_vws = (struct ISO_GTK_widgets *) f_iso_item->void_panel;
	GtkWidget *vbox_v_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_iso_item->f_iso_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_iso_item->f_iso_output_type_ctl);
    GtkWidget *expand_iso_d = draw_iso_def_ctl_vbox(f_iso_item->f_iso_def_c, iso_vws, window);
    GtkWidget *expand_iso_f = draw_psf_fld_ctl_vbox(f_iso_item->f_fld_on_iso_c, 
                                                    iso_vws->label_field_list, iso_vws->label_dir_list,
                                                    iso_vws->field_output_vws, window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_iso), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_iso), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_iso), expand_iso_d,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_iso), expand_iso_f,  FALSE, FALSE, 0);
    GtkWidget *expand_v_iso = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_iso_item->f_iflag,
															   f_iso_item->iso_ctl_file_name,
															   window, vbox_v_iso);
    return expand_v_iso;
};
