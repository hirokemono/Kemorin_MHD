/*
//  ctl_data_platforms_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "ctl_data_platforms_GTK.h"

extern int lengthchara_f();
extern int c_yes_flag(const char *text);
extern int c_no_file_flag(const char *file_name);
extern int num_file_fmt_items_f();
extern void set_file_fmt_items_f(char *fmt_names_c);

extern void * c_sphere_data_ctl_block_name(void *f_spctl);
extern void * c_sphere_data_ctl_iflag(void *f_spctl);
extern void * c_sphere_data_ltr_ctl(void *f_spctl);
extern void * c_sphere_data_phi_symmetry_ctl(void *f_spctl);
extern void * c_sphere_data_sph_grd_tpe_ctl(void *f_spctl);
extern void * c_sphere_data_coef_type_ctl(void *f_spctl);
extern void * c_sphere_data_n_elevation_ctl(void *f_spctl);
extern void * c_sphere_data_ngrid_azmth_ctl(void *f_spctl);
extern void * c_sphere_data_radius_ctl(void *f_spctl);
extern void * c_sphere_data_radial_grp_ctl(void *f_spctl);
extern void * c_sphere_data_add_ext_ctl(void *f_spctl);
extern void * c_sphere_data_r_grid_type_ctl(void *f_spctl);
extern void * c_sphere_data_num_fld_grid_ctl(void *f_spctl);
extern void * c_sphere_data_icrmnt_cheby_ctl(void *f_spctl);
extern void * c_sphere_data_Min_radius_ctl(void *f_spctl);
extern void * c_sphere_data_ICB_radius_ctl(void *f_spctl);
extern void * c_sphere_data_CMB_radius_ctl(void *f_spctl);
extern void * c_sphere_data_Max_radius_ctl(void *f_spctl);
extern void * c_sphere_data_fld_core_sze_ctl(void *f_spctl);
extern void * c_sphere_data_ICB_CMB_ratio_ctl(void *f_spctl);
extern void * c_sphere_data_num_r_layer_ctl(void *f_spctl);
extern void * c_sphere_data_n_med_layer_ctl(void *f_spctl);
extern void * c_sphere_data_r_layer_list_ctl(void *f_spctl);
extern void * c_sphere_data_med_list_ctl(void *f_spctl);


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

struct cr_array_views * init_cr_array_views(struct f_ctl_cr_array *f_cr_array)
{
    int i;
    struct cr_array_views *cr_array_vws = (struct cr_array_views *) malloc(sizeof(struct cr_array_views));
    if(cr_array_vws == NULL){
        printf("malloc error for cr_array_views\n");
        exit(0);
    };
    
    cr_array_vws->cr_array_clist = init_chara_real_clist();
    for(i=0;i<f_cr_array->f_num[0];i++){
        append_chara_real_clist(f_cr_array->c_charavalue[i], f_cr_array->f_rctls[i],
                                cr_array_vws->cr_array_clist);
    }
    
    
    printf("count_chara_clist %d\n", count_chara_real_clist(cr_array_vws->cr_array_clist));
    for(i=0;i<f_cr_array->f_num[0];i++){
        printf("item %d %s %le\n", i,
               chara_real_clist_at_index(i, cr_array_vws->cr_array_clist)->c_tbl,
               chara_real_clist_at_index(i, cr_array_vws->cr_array_clist)->r_data);
    }
    return cr_array_vws;
}

void dealloc_cr_array_views(struct cr_array_views *cr_array_vws)
{
    dealloc_chara_real_clist(cr_array_vws->cr_array_clist);
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
	
	f_plt->f_iflag =       (int *)  c_plt_iflag(f_plt->f_self);
	f_plt->f_block_name =  (char *) c_plt_block_name(f_plt->f_self);
	f_plt->c_block_name = strngcopy_from_f(f_plt->f_block_name);
	
	f_plt->f_ndomain_ctl =               init_f_ctl_int_item(c_plt_ndomain_ctl, f_plt->f_self);
	f_plt->f_num_smp_ctl =               init_f_ctl_int_item(c_plt_num_smp_ctl, f_plt->f_self);
	
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

GtkWidget * draw_file_format_select_hbox(struct control_labels_f *label_file_format_list, 
										 struct f_ctl_chara_item * f_citem, GtkWidget *window){
	GtkWidget *hbox = hbox_with_block_checkbox(f_citem->f_iflag);
	GtkWidget *label = gtk_label_new(f_citem->c_block_name);
	
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	GtkWidget *file_formats_tree_view
			= create_control_flags_tree_view(label_file_format_list);
	add_control_combobox_vbox(f_citem->f_charavalue, f_citem->c_charavalue, 
							  label_file_format_list, 
							  file_formats_tree_view, vbox);
	
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
	return hbox;
}


GtkWidget * draw_platform_control_vbox(struct f_platform_control *f_plt, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *hbox_c1 = draw_chara_switch_entry_hbox(f_plt->f_debug_flag_ctl);
	GtkWidget *hbox_i1 = draw_int_item_entry_hbox(f_plt->f_ndomain_ctl);
	GtkWidget *hbox_i2 = draw_int_item_entry_hbox(f_plt->f_num_smp_ctl);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_plt->f_sph_file_prefix);
	GtkWidget *hbox_c3 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_sph_file_fmt_ctl, window);
	/*
	GtkWidget *hbox_c4 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_prefix);
	GtkWidget *hbox_c5 = draw_chara_item_entry_hbox(f_plt->f_mesh_file_fmt_ctl);
	*/
	GtkWidget *hbox_c6 = draw_chara_item_entry_hbox(f_plt->f_restart_file_prefix);
	GtkWidget *hbox_c7 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_restart_file_fmt_ctl, window);
	GtkWidget *hbox_c8 = draw_chara_item_entry_hbox(f_plt->f_field_file_prefix);
	GtkWidget *hbox_c9 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_field_file_fmt_ctl, window);
	/*
 	GtkWidget *hbox_c10 = draw_chara_item_entry_hbox(f_plt->f_spectr_field_file_prefix);
	GtkWidget *hbox_c11 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_spectr_field_fmt_ctl, window);
	GtkWidget *hbox_c12 = draw_chara_item_entry_hbox(f_plt->f_coriolis_int_file_name);
	GtkWidget *hbox_c13 = draw_chara_item_entry_hbox(f_plt->f_coriolis_file_fmt_ctl);
	*/
	GtkWidget *hbox_c14 = draw_chara_item_entry_hbox(f_plt->f_bc_data_file_name_ctl);
	GtkWidget *hbox_c15 = draw_chara_item_entry_hbox(f_plt->f_radial_data_file_name_ctl);
	/*
	GtkWidget *hbox_c16 = draw_chara_item_entry_hbox(f_plt->f_interpolate_sph_to_fem);
	GtkWidget *hbox_c17 = draw_chara_item_entry_hbox(f_plt->f_interpolate_fem_to_sph);
	GtkWidget *hbox_c18 = draw_file_format_select_hbox(f_plt->label_file_format_list, 
													  f_plt->f_itp_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c19 = draw_chara_item_entry_hbox(f_plt->f_rayleigh_spectr_dir);
	GtkWidget *hbox_c20 = draw_chara_item_entry_hbox(f_plt->f_rayleigh_field_dir);
	
	GtkWidget *hbox_c21 = draw_chara_item_entry_hbox(f_plt->f_del_org_data_ctl);
	
	GtkWidget *vbox_plt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_i1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_i2, FALSE, FALSE, 0);
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
											   320, 280, window, vbox_plt);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_PLT, FALSE, FALSE, 0);
	return vbox_out;
};

struct f_MHD_sph_resolution_control * init_f_MHD_sph_resolution_control(void *(*c_load_self)(void *f_parent), 
																		void *f_parent)
{
	struct f_MHD_sph_resolution_control *f_spctl 
			= (struct f_MHD_sph_resolution_control *) malloc(sizeof(struct f_MHD_sph_resolution_control));
	if(f_spctl == NULL){
		printf("malloc error for f_spctl\n");
		exit(0);
	};
	
	f_spctl->f_self =  c_load_self(f_parent);
	
	f_spctl->f_iflag =        (int *) c_sphere_data_ctl_iflag(f_spctl->f_self);
	f_spctl->f_block_name =   (char *) c_sphere_data_ctl_block_name(f_spctl->f_self);
	f_spctl->c_block_name = strngcopy_from_f(f_spctl->f_block_name);
	
	f_spctl->f_ltr_ctl =               init_f_ctl_int_item(c_sphere_data_ltr_ctl, f_spctl->f_self);
	f_spctl->f_phi_symmetry_ctl =      init_f_ctl_int_item(c_sphere_data_phi_symmetry_ctl, f_spctl->f_self);
	f_spctl->f_sph_grid_type_ctl =     init_f_ctl_chara_item(c_sphere_data_sph_grd_tpe_ctl, f_spctl->f_self);
	f_spctl->f_sph_coef_type_ctl =     init_f_ctl_chara_item(c_sphere_data_coef_type_ctl, f_spctl->f_self);
	f_spctl->f_ngrid_elevation_ctl =   init_f_ctl_int_item(c_sphere_data_n_elevation_ctl, f_spctl->f_self);
	f_spctl->f_ngrid_azimuth_ctl =     init_f_ctl_int_item(c_sphere_data_ngrid_azmth_ctl, f_spctl->f_self);
	f_spctl->f_radius_ctl =            init_f_ctl_ir_array(c_sphere_data_radius_ctl, f_spctl->f_self);
	f_spctl->f_radial_grp_ctl =        init_f_ctl_ci_array(c_sphere_data_radial_grp_ctl, f_spctl->f_self);
	f_spctl->f_add_ext_layer_ctl =     init_f_ctl_real_array(c_sphere_data_add_ext_ctl, f_spctl->f_self);
	f_spctl->f_radial_grid_type_ctl =  init_f_ctl_chara_item(c_sphere_data_r_grid_type_ctl, f_spctl->f_self);
	f_spctl->f_num_fluid_grid_ctl =    init_f_ctl_int_item(c_sphere_data_num_fld_grid_ctl, f_spctl->f_self);
	f_spctl->f_increment_cheby_ctl =   init_f_ctl_int_item(c_sphere_data_icrmnt_cheby_ctl, f_spctl->f_self);
	f_spctl->f_Min_radius_ctl =        init_f_ctl_real_item(c_sphere_data_Min_radius_ctl, f_spctl->f_self);
	f_spctl->f_ICB_radius_ctl =        init_f_ctl_real_item(c_sphere_data_ICB_radius_ctl, f_spctl->f_self);
	f_spctl->f_CMB_radius_ctl =        init_f_ctl_real_item(c_sphere_data_CMB_radius_ctl, f_spctl->f_self);
	f_spctl->f_Max_radius_ctl =        init_f_ctl_real_item(c_sphere_data_Max_radius_ctl, f_spctl->f_self);
	f_spctl->f_fluid_core_size_ctl =   init_f_ctl_real_item(c_sphere_data_fld_core_sze_ctl, f_spctl->f_self);
	f_spctl->f_ICB_to_CMB_ratio_ctl =  init_f_ctl_real_item(c_sphere_data_ICB_CMB_ratio_ctl, f_spctl->f_self);
	f_spctl->f_num_radial_layer_ctl =  init_f_ctl_int_item(c_sphere_data_num_r_layer_ctl, f_spctl->f_self);
	f_spctl->f_num_med_layer_ctl =     init_f_ctl_int_item(c_sphere_data_n_med_layer_ctl, f_spctl->f_self);
	f_spctl->f_radial_layer_list_ctl = init_f_ctl_i2_array(c_sphere_data_r_layer_list_ctl, f_spctl->f_self);
	f_spctl->f_med_layer_list_ctl =    init_f_ctl_i2_array(c_sphere_data_med_list_ctl, f_spctl->f_self);
	return f_spctl;
};

GtkWidget * draw_sph_resolution_vbox(struct f_MHD_sph_resolution_control *f_spctl, 
									 struct f_MHD_sph_resolution_views *f_spctl_vws, GtkWidget *window){
	int i;
	f_spctl_vws->f_radial_grp_ctl_vws = (struct ci_clist_view *) malloc(sizeof(struct ci_clist_view));
	if(f_spctl_vws->f_radial_grp_ctl_vws == NULL){
		printf("malloc error for f_radial_grp_ctl_vws\n");
		exit(0);
	};
	f_spctl_vws->f_radial_grp_ctl_vws->ci_clist_gtk = init_chara_int_clist();
    for(i=0;i<f_spctl->f_radial_grp_ctl->f_num[0];i++){
		append_chara_int_clist(f_spctl->f_radial_grp_ctl->c_charavalue[i], 
							  f_spctl->f_radial_grp_ctl->f_ictls[i], 
							  f_spctl_vws->f_radial_grp_ctl_vws->ci_clist_gtk);
    }
	
	f_spctl_vws->f_add_ext_layer_vws = (struct r_clist_view *) malloc(sizeof(struct r_clist_view));
	if(f_spctl_vws->f_add_ext_layer_vws == NULL){
		printf("malloc error for f_add_ext_layer_vws\n");
		exit(0);
	};
	f_spctl_vws->f_add_ext_layer_vws->r_clist_gtk = init_real_clist();
    for(i=0;i<f_spctl->f_add_ext_layer_ctl->f_num[0];i++){
		append_real_clist(f_spctl->f_add_ext_layer_ctl->f_rctls[i], 
						f_spctl_vws->f_add_ext_layer_vws->r_clist_gtk);
    }
	
	GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *hbox_i1 = draw_int_item_entry_hbox(f_spctl->f_ltr_ctl);
	GtkWidget *hbox_i2 = draw_int_item_entry_hbox(f_spctl->f_phi_symmetry_ctl);
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_hbox(f_spctl->f_sph_grid_type_ctl);
	GtkWidget *hbox_c2 = draw_chara_item_entry_hbox(f_spctl->f_sph_coef_type_ctl);
	
	GtkWidget *hbox_i3 = draw_int_item_entry_hbox(f_spctl->f_ngrid_elevation_ctl);
	GtkWidget *hbox_i4 = draw_int_item_entry_hbox(f_spctl->f_ngrid_azimuth_ctl);
	
	GtkWidget *hbox_d1 = add_ir_list_box_w_addbottun(f_spctl->f_radius_ctl,
													 f_spctl_vws->f_radius_ctl_tree);
	GtkWidget *hbox_d2 = add_ci_list_box_w_addbottun(f_spctl->f_radial_grp_ctl,
													 f_spctl_vws->f_radial_grp_ctl_vws);
	GtkWidget *hbox_d3 = real_array_vbox_w_addbottun(f_spctl->f_add_ext_layer_ctl,
													 f_spctl_vws->f_add_ext_layer_vws);
	GtkWidget *hbox_c3 = draw_chara_item_entry_hbox(f_spctl->f_radial_grid_type_ctl);
	
	GtkWidget *hbox_i5 = draw_int_item_entry_hbox(f_spctl->f_num_fluid_grid_ctl);
	GtkWidget *hbox_i6 = draw_int_item_entry_hbox(f_spctl->f_increment_cheby_ctl);
	GtkWidget *hbox_r1 = draw_real_item_entry_hbox(f_spctl->f_fluid_core_size_ctl);
	GtkWidget *hbox_r2 = draw_real_item_entry_hbox(f_spctl->f_ICB_to_CMB_ratio_ctl);
	GtkWidget *hbox_r3 = draw_real_item_entry_hbox(f_spctl->f_Min_radius_ctl);
	GtkWidget *hbox_r4 = draw_real_item_entry_hbox(f_spctl->f_ICB_radius_ctl);
	GtkWidget *hbox_r5 = draw_real_item_entry_hbox(f_spctl->f_CMB_radius_ctl);
	GtkWidget *hbox_r6 = draw_real_item_entry_hbox(f_spctl->f_Max_radius_ctl);
	
	GtkWidget *hbox_i7 = draw_int_item_entry_hbox(f_spctl->f_num_radial_layer_ctl);
	GtkWidget *hbox_d4 = add_i2_list_box_w_addbottun(f_spctl->f_radial_layer_list_ctl, 
													 f_spctl_vws->f_radial_layer_list_ctl_tree);
	
	GtkWidget *hbox_i8 = draw_int_item_entry_hbox(f_spctl->f_num_med_layer_ctl);
	GtkWidget *hbox_d5 = add_i2_list_box_w_addbottun(f_spctl->f_med_layer_list_ctl, 
													 f_spctl_vws->f_med_layer_list_ctl_tree);
	
	GtkWidget *vbox_sph = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r5, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_r6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i7, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_i8, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_sph), hbox_d5, FALSE, FALSE, 0);
	GtkWidget *expand = draw_control_block(f_spctl->c_block_name, f_spctl->f_iflag,
										   480, 320, window, vbox_sph);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand, FALSE, FALSE, 0);
	return vbox_out;
};
