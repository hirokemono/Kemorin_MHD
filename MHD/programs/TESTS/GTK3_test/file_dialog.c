
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <sys/stat.h>

#include "control_elements_IO_c.h"
#include "t_control_c_lists.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_control_chara2_int_IO.h"
#include "t_ctl_array_chara3_items_c.h"
#include "t_ctl_array_chara_int3_items_c.h"
#include "t_ctl_array_chara2_real_items_c.h"
#include "t_ctl_array_chara2_items_c.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_ctl_data_4_fields_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"

#include "c_ctl_data_SGS_model.h"
#include "c_control_data_pvrs.h"
#include "c_ctl_data_platforms.h"
#include "c_ctl_data_MHD_BCs.h"
#include "c_ctl_data_MHD_model.h"
#include "c_ctl_data_PSF_ISOs.h"
#include "c_ctl_data_MAP.h"
#include "c_ctl_data_PVR_colormap.h"
#include "c_ctl_data_PVR_view_matrix.h"
#include "c_ctl_data_FLINE.h"
#include "c_ctl_data_LIC.h"
#include "c_ctl_VIZ_repartition.h"
#include "c_ctl_data_SGS_MHD.h"

#include "control_elements_IO_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panel_4_field_GTK.h"
#include "t_control_data_4_iso_c.h"
#include "kemoview_gtk_routines.h"
#include "tree_view_chara_GTK.h"
#include "tree_view_4_field_GTK.h"
#include "tree_view_4_force_GTK.h"
#include "tree_view_4_colormap.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_boundary_condition_GTK.h"

#include "ctl_data_platforms_GTK.h"
#include "control_panel_4_dimless_GTK.h"
#include "control_panels_MHD_control_GTK.h"
#include "control_block_panel_GTK.h"
#include "control_panel_cbox_GTK.h"
#include "control_panel_cbox_real_GTK.h"
#include "control_panel_cbox_cbox_real_GTK.h"
#include "control_panel_text_text_real_GTK.h"
#include "control_panel_int_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_panel_real3_GTK.h"
#include "control_panel_4_sph_monitor_GTK.h"
#include "control_panel_4_MHD_BCs_GTK.h"
#include "control_panels_MHD_model_GTK.h"
#include "control_panel_4_SGS_model_GTK.h"
#include "control_panel_fld_on_psf_GTK.h"
#include "control_panel_PSF_ISO_GTK.h"
#include "control_panel_VIZ_repartition_GTK.h"
#include "control_panel_PVR_colormap.h"
#include "control_panel_VIZ_repartition_GTK.h"


extern void c_view_control_sph_SGS_MHD();

extern void * c_read_control_sph_SGS_MHD(char *file_name);

extern void * set_file_fmt_items_f(void *fmt_names_c);



struct MAP_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
    struct colormap_view *color_vws;
    
    struct PSF_GTK_widgets *psf_def_vws;
    GtkWidget * map_area_view;
};

struct pvr_area_widgets{
    GtkWidget *f_pvr_area_tree;
    GtkWidget *f_surf_enhanse_tree;
};

struct PVR_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
    struct colormap_view *color_vws;
    
    struct block_array_widgets *pvr_psfs_Wgts;
    struct block_array_widgets *pvr_isos_Wgts;
    struct pvr_area_widgets * pvr_areaWgts;
    
    GtkWidget * lighting_tree_view;
};

struct LIC_GTK_widgets{
    struct PVR_GTK_widgets *lic_pvr_vws;
    struct VIZ_repartition_widgets *f_repart_vws;
    struct block_array_widgets *masking_Wgts;
    
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
};

struct FLINE_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara2_int_clist *label_dir_list;
    
    GtkWidget * fline_area_grp_view;
    GtkWidget * f_seed_point_vws;
    GtkWidget * f_seed_surface_vws;
};

struct main_widgets{
	GtkWidget *main_Vbox;
	GtkWidget *open_Hbox;
	GtkWidget *ctl_MHD_Vbox;
    GtkWidget *ctl_MHD_inner_box;
    struct VIZ_repartition_widgets *f_repart_vws;
	
	struct f_sph_shell_views *f_psph_vws;
    
    //	GtkWidget *expand_vpwrs;
//	GtkWidget *expand_smntr;
	struct f_sph_monitor_widgets *f_lp_vws;
	
	struct block_array_widgets *vpsf_Wgts;
	struct block_array_widgets *viso_Wgts;
	struct block_array_widgets *vmap_Wgts;
	struct block_array_widgets *vpvr_Wgts;
	struct block_array_widgets *vlic_Wgts;
    struct block_array_widgets *vfline_Wgts;
    
	struct block_array_widgets *zm_psf_Wgts;
	struct block_array_widgets *zrms_psf_Wgts;
	struct block_array_widgets *zm_map_Wgts;
	struct block_array_widgets *zrms_map_Wgts;
    
    struct chara_clist *label_file_format_list;

    struct MHD_model_widgets *model_wgts;
};

void MHD_control_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl,
						  struct main_widgets *mWidgets);

int iflag_read_iso = 0;

struct iso_ctl_GTK{
	struct iso_ctl_c *iso_c;
	
	struct field_ctl_c *iso_field_ctl;
	struct field_views *iso_fields_vws;
	
	struct field_ctl_c *color_field_ctl;
	struct field_views *color_fields_vws;
};


struct iso_ctl_GTK *iso_GTK0;


// struct SGS_MHD_control_c *mhd_ctl;
GtkWidget *window;

void *MHD_ctl_C;

/*
static gboolean
boolean_to_text (GBinding *binding,
                 const GValue *source,
                 GValue *target,
                 gpointer dummy G_GNUC_UNUSED)
{
	if (g_value_get_boolean (source)){
		g_value_set_string (target, "On");
	}else{
		g_value_set_string (target, "Off");
	}
	
	return TRUE;
}
*/





static void cb_View(GtkButton *button, gpointer data)
{
	c_view_control_sph_SGS_MHD();
}
static void cb_Open(GtkButton *button, gpointer data)
{
	GtkWidget *dialog;
	GtkWidget *parent;
	GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN,
									 GTK_FILE_CHOOSER_ACTION_SAVE,
									 GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
									 GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
  gchar *read_file_name;
  gchar *folder;
	
	
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
	struct main_widgets *mWidgets = (struct main_widgets *) g_object_get_data(G_OBJECT(data), "mWidgets");
	struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) g_object_get_data(G_OBJECT(data), "MHD_ctl");
	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[0],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Open", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);

	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name  */
		read_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "file name: %s\n", read_file_name);
		
		folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
		if (folder == NULL) {
			int length = strlen(read_file_name);
			char *stripped_filehead = (char *) calloc(length+1, sizeof(char));
			char *stripped_dir = (char *) calloc(length+1, sizeof(char));
			split_dir_and_file_name_c((char *) read_file_name, 
									  stripped_dir, stripped_filehead);
			printf("Folder %s\n", stripped_dir);
			chdir(stripped_dir);
		} else {
			g_print( "folder name: %s\n", folder);
			chdir(folder);
		}
		/* Get Folder name */
		printf("f_MHD_ctl %p\n", f_MHD_ctl);
		f_MHD_ctl->f_self =     c_read_control_sph_SGS_MHD((char *) read_file_name);
		f_MHD_ctl->f_addition = c_add_sgs_sph_mhd_ctl();
		set_f_MHD_control(f_MHD_ctl);
		
		/* Show file name in entry */ 
		gtk_entry_set_text(entry, read_file_name);
		
		if((iso_GTK0 = (struct iso_ctl_GTK *) malloc(sizeof(struct iso_ctl_GTK))) == NULL) {
			printf("malloc error for iso_ctl_GTK \n");
			exit(0);
		}
		
		iso_GTK0->iso_c = init_iso_ctl_c();        
		read_iso_ctl_file_c(read_file_name, buf, iso_GTK0->iso_c);
		iflag_read_iso = 1;
		g_free(read_file_name);
		printf("iso_output_type_ctl original %s\n", iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		set_primary_iso_format_flag_c(iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		printf("iso_output_type_ctl modified %s\n", iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		
		MHD_control_expander(window, f_MHD_ctl, mWidgets);
		gtk_box_pack_start(GTK_BOX(mWidgets->main_Vbox), mWidgets->ctl_MHD_Vbox, FALSE, TRUE, 0);
		gtk_widget_show_all(window);
	}else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	}else{
		g_print( "Another response was received.\n" );
	}
	gtk_widget_destroy(dialog);
}

static void cb_Save(GtkButton *button, gpointer data)
{
    struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) g_object_get_data(G_OBJECT(data), "MHD_ctl");

    GtkWidget *dialog;
    GtkWidget *parent;
    GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
    gint response;
    gchar *write_file_name;
    gchar *folder;
    char path_name[LENGTHBUF];

  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[1],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Save", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);
    int i;
    
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		gtk_entry_set_text(entry, write_file_name);
		g_free(write_file_name);
		
		folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
        g_print( "Write folder name: %s\n", write_file_name);
        
        getcwd(path_name, LENGTHBUF);
        printf("before current dir : %s\n", path_name);
        chdir(folder);
        getcwd(path_name, LENGTHBUF);
        printf("Folder to save: %s\n", path_name);
        
        char *stripped_filehead = (char *) calloc(LENGTHBUF+1, sizeof(char));
        char *stripped_dir = (char *) calloc(LENGTHBUF+1, sizeof(char));
        struct stat st;
        
        split_dir_and_file_name_c(f_MHD_ctl->f_psph_ctl->fname_sph_shell, stripped_dir, stripped_filehead);
        printf("fname_sph_shell: %s %s, \n", stripped_dir, f_MHD_ctl->f_psph_ctl->fname_sph_shell);
        if(compare_string(strlen(f_MHD_ctl->f_psph_ctl->fname_sph_shell), 
                          stripped_dir, f_MHD_ctl->f_psph_ctl->fname_sph_shell) == 0){
            if(stat(stripped_dir, &st) != 0) {
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            }
        };
        
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_psf_ctls);i++){
            struct f_VIZ_PSF_ctl *ctl_tmp 
                    = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_psf_ctls);
            split_dir_and_file_name_c(ctl_tmp->psf_ctl_file_name, stripped_dir, stripped_filehead);
            printf("psf_ctl_file_name %d: %s %s, \n", i, stripped_dir, ctl_tmp->psf_ctl_file_name);
            if(compare_string(strlen(ctl_tmp->psf_ctl_file_name),
                              stripped_dir, ctl_tmp->psf_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
            if(compare_string(strlen(ctl_tmp->f_psf_def_c->psf_def_file_name),
                              stripped_dir, ctl_tmp->f_psf_def_c->psf_def_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
            if(compare_string(strlen(ctl_tmp->f_fld_on_psf_c->fname_fld_on_psf),
                              stripped_dir, ctl_tmp->f_fld_on_psf_c->fname_fld_on_psf) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_iso_ctls);i++){
            struct f_VIZ_ISO_ctl *ctl_tmp 
                    = (struct f_VIZ_ISO_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_iso_ctls);
            if(compare_string(strlen(ctl_tmp->iso_ctl_file_name),
                              stripped_dir, ctl_tmp->iso_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
            if(compare_string(strlen(ctl_tmp->f_fld_on_iso_c->fname_fld_on_psf),
                              stripped_dir, ctl_tmp->f_fld_on_iso_c->fname_fld_on_psf) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_map_ctls);i++){
            struct f_VIZ_MAP_ctl *ctl_tmp 
                    = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_map_ctls);
            if(compare_string(strlen(ctl_tmp->map_ctl_file_name),
                              stripped_dir, ctl_tmp->map_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_pvr_ctls);i++){
            struct f_VIZ_PVR_ctl *ctl_tmp 
                    = (struct f_VIZ_PVR_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_pvr_ctls);
            if(compare_string(strlen(ctl_tmp->pvr_ctl_file_name),
                              stripped_dir, ctl_tmp->pvr_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_lic_ctls);i++){
            struct f_VIZ_LIC_PVR_ctl *ctl_tmp 
                    = (struct f_VIZ_LIC_PVR_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_lic_ctls);
            if(compare_string(strlen(ctl_tmp->lic_ctl_file_name),
                              stripped_dir, ctl_tmp->lic_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_fline_ctls);i++){
            struct f_VIZ_FLINE_ctl *ctl_tmp 
                    = (struct f_VIZ_FLINE_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_fline_ctls);
            if(compare_string(strlen(ctl_tmp->fline_ctl_file_name),
                              stripped_dir, ctl_tmp->fline_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
	} else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	} else{
		g_print( "Another response was received.\n" );
	};
	gtk_widget_destroy(dialog);
}

void expander_MHD_ctl_callback(GObject *object, GParamSpec *param_spec, gpointer user_data){
	GtkExpander *expander;

	expander = GTK_EXPANDER (object);
	if (gtk_expander_get_expanded (expander)){
		printf("Expanded \n");
	}else{
		printf("Hided \n");
	}
	gtk_widget_show_all(window);
};

struct viewmat_GTK_widgets{
    struct chara2_int_clist *label_xyz_dir_list;
    struct chara2_int_clist *label_xyzw_dir_list;
    
    struct cbox_cbox_table_view *f_map_image_prefix_vws;
    struct chara_cbox_table_view *f_lookpoint_vws;
    struct chara_cbox_table_view *f_viewpoint_vws;
    struct chara_cbox_table_view *f_up_dir_vws;
    struct chara_cbox_table_view *f_scale_vector_vws;
    struct chara_cbox_table_view *f_viewpt_in_viewer_vws;
    struct chara_cbox_table_view *f_view_rot_vec_vws;
};

static GtkWidget * draw_viewmatrix_pixels_vbox(struct image_size_ctl_c *f_pixel, 
                                               GtkWidget *window){
    GtkWidget *hbox_1 = draw_int_item_entry_hbox(f_pixel->f_num_xpixel_ctl);
    GtkWidget *hbox_2 = draw_int_item_entry_hbox(f_pixel->f_num_ypixel_ctl);
    
    GtkWidget *vbox_v_pix = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_pixel = draw_control_block(f_pixel->c_block_name, f_pixel->f_iflag,
                                                 window, vbox_v_pix);
    return expand_pixel;
};

static GtkWidget * draw_viewmatrix_projection_vbox(struct projection_mat_ctl_c *f_proj, 
                                                   GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_proj->f_perspective_angle_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_proj->f_perspective_xy_ratio_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_proj->f_perspective_near_ctl);
    GtkWidget *hbox_4 = draw_real_item_entry_hbox(f_proj->f_perspective_far_ctl);
    
    GtkWidget *hbox_5 = draw_real2_item_entry_hbox(f_proj->f_horizontal_range_ctl);
    GtkWidget *hbox_6 = draw_real2_item_entry_hbox(f_proj->f_vertical_range_ctl);
    
    GtkWidget *vbox_v_pix = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_6,  FALSE, FALSE, 0);
    
    GtkWidget *expand_proj = draw_control_block(f_proj->c_block_name, f_proj->f_iflag,
                                                window, vbox_v_pix);
    return expand_proj;
};

static GtkWidget * draw_viewmatrix_stereo_vbox(struct streo_view_ctl_c *f_streo, 
                                               GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_streo->f_focalpoint_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_streo->f_eye_separation_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_streo->f_eye_sep_angle_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_streo->f_step_eye_sep_angle_ctl);
    
    GtkWidget *vbox_s = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_4,  FALSE, FALSE, 0);
    
    GtkWidget *expand_streo = draw_control_block(f_streo->c_block_name, f_streo->f_iflag,
                                                 window, vbox_s);
    return expand_streo;
};

static GtkWidget * draw_viz_viewmatrix_vbox(struct modelview_ctl_c *f_mat_c, 
                                            GtkWidget *window){
    struct viewmat_GTK_widgets *viewmatrix_vws = (struct viewmat_GTK_widgets *) f_mat_c->void_panel;
    
    GtkWidget *expand_pixel = draw_viewmatrix_pixels_vbox(f_mat_c->f_pixel, window);
    GtkWidget *expand_proj =  draw_viewmatrix_projection_vbox(f_mat_c->f_proj, window);
    GtkWidget *expand_streo = draw_viewmatrix_stereo_vbox(f_mat_c->f_streo, window);
    
    GtkWidget *hbox_1 = c2r_list_combobox_expander(f_mat_c->f_modelview_mat_ctl,
                                                   viewmatrix_vws->label_xyzw_dir_list,
                                                   viewmatrix_vws->label_xyzw_dir_list, 
                                                   viewmatrix_vws->f_map_image_prefix_vws, window);
    GtkWidget *hbox_2 = cr_list_combobox_expander(f_mat_c->f_lookpoint_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_lookpoint_vws, window);
    GtkWidget *hbox_3 = cr_list_combobox_expander(f_mat_c->f_viewpoint_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_viewpoint_vws, window);
    GtkWidget *hbox_4 = cr_list_combobox_expander(f_mat_c->f_up_dir_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_up_dir_vws, window);
    GtkWidget *hbox_5 = cr_list_combobox_expander(f_mat_c->f_view_rot_vec_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_view_rot_vec_vws, window);
    
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_mat_c->f_view_rotation_deg_ctl);
    GtkWidget *hbox_7 = draw_real_item_entry_hbox(f_mat_c->f_scale_factor_ctl);
    GtkWidget *hbox_8 = draw_chara_item_entry_hbox(f_mat_c->f_projection_type_ctl);
    
    GtkWidget *hbox_9 = cr_list_combobox_expander(f_mat_c->f_scale_vector_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_scale_vector_vws, window);
    GtkWidget *hbox_10 = cr_list_combobox_expander(f_mat_c->f_viewpt_in_viewer_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_viewpt_in_viewer_vws, window);
    
	GtkWidget *vbox_v_mat = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_pixel,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_10,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_proj,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_streo,  FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_map = draw_control_block_w_file_switch(f_mat_c->c_block_name,
															   f_mat_c->f_iflag,
															   f_mat_c->mat_ctl_file_name,
															   window, vbox_v_mat);
    return expand_v_map;
};

static GtkWidget * draw_map_define_vbox(struct f_MAP_section_ctl *f_map_define_ctl, 
                                        struct PSF_GTK_widgets *psf_def_vws, GtkWidget *window){
    GtkWidget *expand_s =  draw_psf_def_ctl_vbox(f_map_define_ctl->f_psf_def_c, 
                                                 psf_def_vws, window);
                                                                                                 
    GtkWidget *hbox_1 = draw_chara_switch_entry_hbox(f_map_define_ctl->f_zeroline_switch_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_map_define_ctl->f_isoline_color_mode);
    GtkWidget *hbox_3 = draw_int_item_entry_hbox(f_map_define_ctl->f_isoline_number_ctl);
    GtkWidget *hbox_4 = draw_real2_item_entry_hbox(f_map_define_ctl->f_isoline_range_ctl);
    GtkWidget *hbox_5 = draw_real_item_entry_hbox(f_map_define_ctl->f_isoline_width_ctl);
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_map_define_ctl->f_grid_width_ctl);
    GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_map_define_ctl->f_tan_cyl_switch_ctl);
    GtkWidget *hbox_8 = draw_real_item_entry_hbox(f_map_define_ctl->f_tangent_cylinder_inner_ctl);
    GtkWidget *hbox_9 = draw_real_item_entry_hbox(f_map_define_ctl->f_tangent_cylinder_outer_ctl);
    
    GtkWidget *vbox_map_def = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), expand_s,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_9,  FALSE, FALSE, 0);
    
    GtkWidget *expand_map_def = draw_control_block(f_map_define_ctl->c_block_name, 
                                                   f_map_define_ctl->f_iflag,
                                                   window, vbox_map_def);
    return expand_map_def;
};

static GtkWidget * draw_viz_each_map_ctl_vbox(char *label_name, struct f_VIZ_MAP_ctl *f_map_item, 
											  GtkWidget *window){
    struct MAP_GTK_widgets *map_vws = (struct MAP_GTK_widgets *) f_map_item->void_panel;
	GtkWidget *vbox_v_map = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_map_item->f_map_image_prefix_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_map_item->f_map_image_fmt_ctl);
    GtkWidget *hbox_3 = draw_field_combobox_hbox(map_vws->label_field_list, 
                                                 f_map_item->f_map_field_ctl, window);
    GtkWidget *hbox_4 = draw_component_combobox_hbox(map_vws->label_dir_list,
                                                     f_map_item->f_map_comp_ctl, window);
    GtkWidget *hbox_5 = draw_field_combobox_hbox(map_vws->label_field_list, 
                                                 f_map_item->f_isoline_field_ctl, window);
    GtkWidget *hbox_6 = draw_component_combobox_hbox(map_vws->label_dir_list,
                                                    f_map_item->f_isoline_comp_ctl, window);
    
    GtkWidget *expand_mdef = draw_map_define_vbox(f_map_item->f_map_define_ctl, 
                                                  map_vws->psf_def_vws, window);
    GtkWidget *expand_vmat = draw_viz_viewmatrix_vbox(f_map_item->f_mat, window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), expand_mdef, FALSE, FALSE, 0);
    
    append_viz_cmap_cbar_vbox(f_map_item->f_cmap_cbar_c, map_vws->color_vws, 
                              map_vws->label_field_list, map_vws->label_dir_list, 
                              window, vbox_v_map);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_map), expand_vmat, FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_map = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_map_item->f_iflag,
															   f_map_item->map_ctl_file_name,
															   window, vbox_v_map);
    return expand_v_map;
};


GtkWidget * draw_pvr_section_vbox(char *label_name, struct f_PVR_section_ctl *f_pvr_psf_c,
                                  GtkWidget *window){
    struct PSF_GTK_widgets *psf_def_vws = (struct PSF_GTK_widgets *) f_pvr_psf_c->void_panel;
    
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_pvr_psf_c->f_opacity_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_pvr_psf_c->f_zeroline_switch_ctl);
    GtkWidget *expand_s =  draw_psf_def_ctl_vbox(f_pvr_psf_c->f_psf_def_c, 
                                                 psf_def_vws, window);
    
    GtkWidget *vbox_psf = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_psf), expand_s,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_psf = draw_control_block(label_name, f_pvr_psf_c->f_iflag,
                                               window, vbox_psf);
    return expand_psf;
}

GtkWidget * draw_pvr_isosurface_vbox(char *label_name, struct f_PVR_isosurface_ctl *f_pvr_iso_ctl,
                                     GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_pvr_iso_ctl->f_iso_value_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_pvr_iso_ctl->f_opacity_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_pvr_iso_ctl->f_isosurf_type_ctl);
    
    GtkWidget *vbox_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_iso = draw_control_block(label_name, f_pvr_iso_ctl->f_iflag,
                                               window, vbox_iso);
    return expand_iso;
}

struct f_PVR_section_ctl * init_f_PVR_sections_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_PVR_section_ctl *f_pvr_psf_c = init_f_PVR_section_ctl(idx, f_parent);
    f_pvr_psf_c->void_panel = (void *) init_PSF_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_pvr_psf_c;
}
static void dealloc_f_PVR_section_GTK(void *void_ctl){
    struct f_PVR_section_ctl *f_pvr_psf_c = (struct f_PVR_section_ctl *) void_ctl;
    dealloc_PSF_GTK_widgets((struct PSF_GTK_widgets *) f_pvr_psf_c->void_panel);
    dealloc_f_PVR_section_ctl(f_pvr_psf_c);
    return;
};

static void dealloc_f_PVR_isosurface_GTK(void *void_ctl){
    struct f_PVR_isosurface_ctl *f_pvr_iso_ctl = ((struct f_PVR_isosurface_ctl *) void_ctl);
    dealloc_f_PVR_isosurface_ctl(f_pvr_iso_ctl);
    return;
};

static GtkWidget * draw_pvr_plot_area_vbox(struct pvr_plot_area_ctl_c *f_area,
                                           struct pvr_area_widgets *pvr_areaWgts, GtkWidget *window){
    GtkWidget *expand_s1 = add_c_list_box_w_addbottun(f_area->f_pvr_area_ctl,
                                                      pvr_areaWgts->f_pvr_area_tree);
    GtkWidget *expand_s2 = c2r_list_text2_expander(f_area->f_surf_enhanse_ctl,
                                                   pvr_areaWgts->f_surf_enhanse_tree, window);
    
    GtkWidget *vbox_area = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_area), expand_s1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_area), expand_s2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_area = draw_control_block(f_area->c_block_name, f_area->f_iflag,
                                                 window, vbox_area);
    return expand_area;
};

static GtkWidget * draw_viz_each_pvr_ctl_vbox(char *label_name, struct f_VIZ_PVR_ctl *f_pvr_item, 
											  GtkWidget *window){
    struct PVR_GTK_widgets *pvr_vws = (struct PVR_GTK_widgets *) f_pvr_item->void_panel;
    printf("PVR_GTK_widgets in %p \n", pvr_vws);
    int j;
    for(j=0;j<count_void_clist(f_pvr_item->f_pvr_scts_c);j++){
         struct f_VIZ_PSF_ctl *f_psf_ctl = void_clist_at_index(j, f_pvr_item->f_pvr_scts_c);
         printf("%d struct PSF_GTK_widgets *psf_def_vws %p %p \n", j, f_psf_ctl, f_psf_ctl->void_panel);
     }

    GtkWidget *vbox_v_pvr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_pvr_item->f_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_pvr_item->f_file_fmt_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_pvr_item->f_monitoring_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_pvr_item->f_streo_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_pvr_item->f_anaglyph_ctl);
    GtkWidget *hbox_6 = draw_chara_item_entry_hbox(f_pvr_item->f_quilt_ctl);
    
    GtkWidget *hbox_7 = draw_field_combobox_hbox(pvr_vws->label_field_list,
                                                f_pvr_item->f_pvr_field_ctl, window);
    GtkWidget *hbox_8 = draw_component_combobox_hbox(pvr_vws->label_dir_list,
                                                     f_pvr_item->f_pvr_comp_ctl, window);
                                                     
    GtkWidget *hbox_9 = draw_pvr_plot_area_vbox(f_pvr_item->f_render_area_c,
                                                pvr_vws->pvr_areaWgts, window);
    
    GtkWidget *expand_v_lgt = draw_pvr_lighting_vbox(f_pvr_item->f_light,
                                                     pvr_vws->lighting_tree_view, window);
    
    GtkWidget *expander_psf = draw_array_block_ctl_vbox(f_pvr_item->f_pvr_scts_c,
                                                        pvr_vws->label_field_list,
                                                        c_append_PVR_sections_ctls,
                                                        c_delete_PVR_sections_ctls,
                                                        (void *) init_f_PVR_sections_GTK,
                                                        (void *) dealloc_f_PVR_section_GTK,
                                                        (void *) draw_pvr_section_vbox,
                                                        pvr_vws->pvr_psfs_Wgts, window);
    GtkWidget *expander_iso = draw_array_block_ctl_vbox(f_pvr_item->f_pvr_isos_c, NULL,
                                                        c_append_PVR_isosurface_ctls,
                                                        c_delete_PVR_isosurface_ctls,
                                                        (void *) init_f_PVR_isosurface_ctl,
                                                        (void *) dealloc_f_PVR_isosurface_GTK,
                                                        (void *) draw_pvr_isosurface_vbox,
                                                        pvr_vws->pvr_isos_Wgts, window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_9,  FALSE, FALSE, 0);
    
    append_viz_cmap_cbar_vbox(f_pvr_item->f_cmap_cbar_c, pvr_vws->color_vws, 
                              pvr_vws->label_field_list, pvr_vws->label_dir_list, 
                              window, vbox_v_pvr);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expand_v_lgt,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expander_psf,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expander_iso,  FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_pwr = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_pvr_item->f_iflag,
															   f_pvr_item->pvr_ctl_file_name,
															   window, vbox_v_pvr);
    return expand_v_pwr;
};

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

static GtkWidget * draw_viz_each_lic_ctl_vbox(char *label_name, struct f_VIZ_LIC_PVR_ctl *f_lic_item, 
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

static GtkWidget * draw_viz_each_fline_ctl_vbox(char *label_name, struct f_VIZ_FLINE_ctl *f_fline_item, 
											  GtkWidget *window){
    struct FLINE_GTK_widgets *fline_vws = (struct FLINE_GTK_widgets *) f_fline_item->void_panel;
    
    GtkWidget *vbox_v_fline = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_fline_item->f_fline_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_fline_item->f_fline_output_type_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_fline_item->f_fline_field_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_fline_item->f_fline_color_field_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_fline_item->f_fline_color_comp_ctl);
    
    GtkWidget *hbox_6 = add_c_list_box_w_addbottun(f_fline_item->f_fline_area_grp_ctl, 
                                                   fline_vws->fline_area_grp_view);
    
    GtkWidget *hbox_7 = draw_chara_item_entry_hbox(f_fline_item->f_starting_type_ctl);
    GtkWidget *hbox_8 = draw_chara_item_entry_hbox(f_fline_item->f_selection_type_ctl);
    GtkWidget *hbox_9 = draw_chara_item_entry_hbox(f_fline_item->f_line_direction_ctl);
    
    GtkWidget *hbox_10 = draw_chara_item_entry_hbox(f_fline_item->f_start_surf_grp_ctl);
    
    GtkWidget *hbox_11 = draw_int_item_entry_hbox(f_fline_item->f_num_fieldline_ctl);
    GtkWidget *hbox_12 = draw_int_item_entry_hbox(f_fline_item->f_max_line_stepping_ctl);
    GtkWidget *hbox_13 = r3_list_combobox_expander(f_fline_item->f_seed_point_ctl,
                                                   fline_vws->f_seed_point_vws, window);
    
    GtkWidget *hbox_14 = add_i2_list_box_w_addbottun(f_fline_item->f_seed_surface_ctl,
                                                     fline_vws->f_seed_surface_vws);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_5,  FALSE, FALSE, 0);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_10, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_12, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_13, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_14, FALSE, FALSE, 0);
    
    GtkWidget *expand_v_fline = draw_control_block_w_file_switch(duplicate_underscore(label_name),
                                                                 f_fline_item->f_iflag,
                                                                 f_fline_item->fline_ctl_file_name,
                                                                 window, vbox_v_fline);
    return expand_v_fline;
};


struct viewmat_GTK_widgets * init_viewmat_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
	struct viewmat_GTK_widgets *viewmatrix_vws 
			= (struct viewmat_GTK_widgets *) malloc(sizeof(struct viewmat_GTK_widgets));
	if(viewmatrix_vws == NULL){
		printf("malloc error for viewmatrix_vws\n");
		exit(0);
    };
    
    viewmatrix_vws->label_xyz_dir_list =  init_field_label_array(c_link_xyz_dir_list_to_ctl());
    viewmatrix_vws->label_xyzw_dir_list = init_field_label_array(c_link_xyzw_dir_list_to_ctl());
    return viewmatrix_vws;
}
void dealloc_viewmat_GTK_widgets(struct viewmat_GTK_widgets *viewmatrix_vws){
    dealloc_chara2_int_clist(viewmatrix_vws->label_xyzw_dir_list);
    dealloc_chara2_int_clist(viewmatrix_vws->label_xyz_dir_list);
    free(viewmatrix_vws);
}

struct MAP_GTK_widgets * init_MAP_GTK_widgets(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                                              struct chara_int2_clist *f_field_ctl)
{
	struct MAP_GTK_widgets *map_vws 
			= (struct MAP_GTK_widgets *) malloc(sizeof(struct MAP_GTK_widgets));
	if(map_vws == NULL){
		printf("malloc error for map_vws\n");
		exit(0);
    };
    
    map_vws->label_field_list = f_field_ctl;
    map_vws->label_dir_list = init_field_label_array(c_link_scalar_dir_list_to_ctl());
    append_field_label_array(c_link_vector_dir_list_to_ctl(),
                             map_vws->label_dir_list);
    append_field_label_array(c_link_stensor_dir_list_to_ctl(),
                             map_vws->label_dir_list);
    map_vws->color_vws = init_colormap_views_4_ctl(f_cmap_cbar_c->cmap_c);
    map_vws->psf_def_vws = init_PSF_GTK_widgets(f_field_ctl);
    return map_vws;
};
void dealloc_MAP_GTK_widgets(struct MAP_GTK_widgets *map_vws){
    dealloc_PSF_GTK_widgets(map_vws->psf_def_vws);
    dealloc_chara_int2_clist(map_vws->label_field_list);
    dealloc_chara2_int_clist(map_vws->label_dir_list);
    dealloc_colormap_views_4_viewer(map_vws->color_vws);
    free(map_vws);
}

struct PVR_GTK_widgets * init_PVR_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                                              struct chara_int2_clist *f_field_ctl)
{
	struct PVR_GTK_widgets *pvr_vws 
			= (struct PVR_GTK_widgets *) malloc(sizeof(struct PVR_GTK_widgets));
	if(pvr_vws == NULL){
		printf("malloc error for pvr_vws\n");
		exit(0);
    };
	pvr_vws->pvr_areaWgts = (struct pvr_area_widgets *) malloc(sizeof(struct pvr_area_widgets));
	if(pvr_vws->pvr_areaWgts == NULL){
		printf("malloc error for pvr_area_widgets\n");
		exit(0);
    };
    
    int i;
    struct f_PVR_section_ctl *f_pvr_sect_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_scts_c);i++){
        f_pvr_sect_ctl = void_clist_at_index(i, f_pvr_ctl->f_pvr_scts_c);
        f_pvr_sect_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_field_ctl);
        printf("%d struct PSF_GTK_widgets *psf_def_vws %p \n", i, f_pvr_sect_ctl->void_panel);
    }
    struct f_PVR_isosurface_ctl *f_pvr_iso_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_isos_c);i++){
        f_pvr_iso_ctl = void_clist_at_index(i, f_pvr_ctl->f_pvr_isos_c);
        f_pvr_iso_ctl->void_panel = (void *) init_ISO_GTK_widgets(f_field_ctl);
    }
    
    pvr_vws->label_field_list = f_field_ctl;
    pvr_vws->label_dir_list = init_field_label_array(c_link_scalar_dir_list_to_ctl());
    append_field_label_array(c_link_vector_dir_list_to_ctl(),
                             pvr_vws->label_dir_list);
    append_field_label_array(c_link_stensor_dir_list_to_ctl(),
                             pvr_vws->label_dir_list);
    pvr_vws->color_vws = init_colormap_views_4_ctl(f_pvr_ctl->f_cmap_cbar_c->cmap_c);
    return pvr_vws;
};
void dealloc_PVR_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                             struct PVR_GTK_widgets *pvr_vws){
    dealloc_colormap_views_4_viewer(pvr_vws->color_vws);
    dealloc_chara_int2_clist(pvr_vws->label_field_list);
    dealloc_chara2_int_clist(pvr_vws->label_dir_list);
    
    int i;
    struct PSF_GTK_widgets *psf_def_vws;
    struct f_PVR_section_ctl *f_pvr_sect_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_scts_c);i++){
        f_pvr_sect_ctl = void_clist_at_index(i,f_pvr_ctl->f_pvr_scts_c);
        dealloc_f_VIZ_PSF_ctl_GTK(f_pvr_sect_ctl->void_panel);
    }
    
    struct f_PVR_isosurface_ctl *f_pvr_iso_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_isos_c);i++){
        f_pvr_iso_ctl = void_clist_at_index(i, f_pvr_ctl->f_pvr_isos_c);
        dealloc_f_VIZ_ISO_ctl_GTK(f_pvr_iso_ctl->void_panel);
    }
    
    free(pvr_vws->pvr_areaWgts);
    free(pvr_vws);
}


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

struct FLINE_GTK_widgets * init_FLINE_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
	struct FLINE_GTK_widgets *fline_vws 
			= (struct FLINE_GTK_widgets *) malloc(sizeof(struct FLINE_GTK_widgets));
	if(fline_vws == NULL){
		printf("malloc error for fline_vws\n");
		exit(0);
    };
    fline_vws->label_field_list = f_field_ctl;
    fline_vws->label_dir_list = init_field_label_array(c_link_scalar_dir_list_to_ctl());
    append_field_label_array(c_link_vector_dir_list_to_ctl(),
                             fline_vws->label_dir_list);
    append_field_label_array(c_link_stensor_dir_list_to_ctl(),
                             fline_vws->label_dir_list);
    return fline_vws;
};
void dealloc_FLINE_GTK_widgets(struct FLINE_GTK_widgets *fline_vws){
    dealloc_chara_int2_clist(fline_vws->label_field_list);
    dealloc_chara2_int_clist(fline_vws->label_dir_list);
    free(fline_vws);
    return;
}


struct f_VIZ_MAP_ctl * init_f_VIZ_MAP_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_MAP_ctl *f_map_ctl = init_f_VIZ_MAP_ctl(idx, f_parent);
    f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                          f_fld_ctl->f_field_ctl);
    f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_map_ctl;
}
void *dealloc_f_VIZ_MAP_ctl_GTK(void *void_in){
    struct f_VIZ_MAP_ctl *f_map_ctl = (struct f_VIZ_MAP_ctl *) void_in;
    dealloc_viewmat_GTK_widgets((struct viewmat_GTK_widgets *) f_map_ctl->f_mat->void_panel);
    dealloc_MAP_GTK_widgets((struct MAP_GTK_widgets *) f_map_ctl->void_panel);
    dealloc_f_VIZ_MAP_ctl(f_map_ctl);
    return NULL;
}

struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_PVR_ctl *f_pvr_ctl = init_f_VIZ_PVR_ctl(c_pvr_render_ctls_pvr_ctl, 
                                                         idx, f_parent);
    f_pvr_ctl->void_panel = (void *) init_PVR_GTK_widgets(f_pvr_ctl, f_fld_ctl->f_field_ctl);
    return f_pvr_ctl;
}
void * dealloc_f_VIZ_PVR_ctl_GTK(void *void_in){
    struct f_VIZ_PVR_ctl *f_pvr_ctl = (struct f_VIZ_PVR_ctl *) void_in;
    dealloc_PVR_GTK_widgets(f_pvr_ctl, (struct PVR_GTK_widgets *) f_pvr_ctl->void_panel);
    dealloc_f_VIZ_PVR_ctl(f_pvr_ctl);
    return NULL;
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

struct f_VIZ_FLINE_ctl * init_f_VIZ_FLINE_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_FLINE_ctl *f_fline_ctl = init_f_VIZ_FLINE_ctl(idx, f_parent);
    f_fline_ctl->void_panel = (void *) init_FLINE_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_fline_ctl;
}
void * dealloc_f_VIZ_FLINE_ctl_GTK(void *void_in){
    struct f_VIZ_FLINE_ctl *f_fline_ctl = (struct f_VIZ_FLINE_ctl *) void_in;
    dealloc_FLINE_GTK_widgets((struct FLINE_GTK_widgets *) f_fline_ctl->void_panel);
    dealloc_f_VIZ_FLINE_ctl(f_fline_ctl);
    return NULL;
}



GtkWidget *MHD_VIZs_ctl_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl, 
                                 struct main_widgets *mWidgets){
    int i;
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_iso_ctls);i++){
        struct f_VIZ_ISO_ctl *f_iso_ctl
                = (struct f_VIZ_ISO_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_iso_ctls);
        f_iso_ctl->void_panel = (void *) init_ISO_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_pvr_ctls);i++){
        struct f_VIZ_PVR_ctl *f_pvr_ctl
                = (struct f_VIZ_PVR_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_pvr_ctls);
        f_pvr_ctl->void_panel = (void *) init_PVR_GTK_widgets(f_pvr_ctl, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    

    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_lic_ctls);i++){
        struct f_VIZ_LIC_PVR_ctl *f_lic_ctl
                = (struct f_VIZ_LIC_PVR_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_lic_ctls);
        struct LIC_GTK_widgets *lic_vws = init_LIC_GTK_widgets(f_lic_ctl->f_lic_pvr_ctl, 
                                                               f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
        f_lic_ctl->f_lic_pvr_ctl->void_panel = (void *) lic_vws->lic_pvr_vws;
        f_lic_ctl->void_panel = (void *) lic_vws;
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_fline_ctls);i++){
        struct f_VIZ_FLINE_ctl *f_fline_ctl
                = (struct f_VIZ_FLINE_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_fline_ctls);
        f_fline_ctl->void_panel = (void *) init_FLINE_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    mWidgets->f_repart_vws->label_field_list = f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl;
    mWidgets->f_repart_vws->label_file_format_list = init_f_ctl_chara_array(set_file_fmt_items_f,
                                                                            f_MHD_ctl->f_self);
    
	GtkWidget *expand_MHD_psf = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_psf_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          mWidgets->vpsf_Wgts, window);
	
	GtkWidget *expand_MHD_iso = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_iso_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_isosurf_ctls,
                                                          c_delete_viz_isosurf_ctls,
                                                          (void *) init_f_VIZ_ISO_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_ISO_ctl_GTK,
                                                          (void *) draw_viz_each_iso_ctl_vbox,
                                                          mWidgets->viso_Wgts, window);
	
	GtkWidget *expand_MHD_map = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_map_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          mWidgets->vmap_Wgts, window);
	
    int j;
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_pvr_ctls);i++){
        struct f_VIZ_PVR_ctl *f_pvr_ctl
                = (struct f_VIZ_PVR_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_pvr_ctls);
       for(j=0;j<count_void_clist(f_pvr_ctl->f_pvr_scts_c);j++){
            struct f_PVR_section_ctl *f_psf_ctl = void_clist_at_index(j, f_pvr_ctl->f_pvr_scts_c);
            printf("%d %d struct PSF_GTK_widgets *psf_def_vws %p %p \n",
                   i, j, f_psf_ctl, f_psf_ctl->void_panel);
        }
        printf("%d f_pvr_ctl->void_panel %p \n", i, f_pvr_ctl->void_panel);
    }
	GtkWidget *expand_MHD_pvr = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_pvr_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_pvr_render_ctls,
                                                          c_delete_viz_pvr_render_ctls,
                                                          (void *) init_f_VIZ_PVR_ctl_GTK,
                                                          dealloc_f_VIZ_PVR_ctl_GTK,
                                                          (void *) draw_viz_each_pvr_ctl_vbox,
                                                          mWidgets->vpvr_Wgts, window);
	
	GtkWidget *expand_MHD_lic = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_lic_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_lic_render_ctls,
                                                          c_delete_viz_lic_render_ctls,
                                                          (void *) init_f_VIZ_LIC_PVR_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_LIC_PVR_ctl_GTK,
                                                          (void *) draw_viz_each_lic_ctl_vbox,
                                                          mWidgets->vlic_Wgts, window);
	
	GtkWidget *expand_MHD_fline = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_fline_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_fline_ctls,
                                                          c_delete_viz_fline_ctls,
                                                          (void *) init_f_VIZ_FLINE_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_FLINE_ctl_GTK,
                                                          (void *) draw_viz_each_fline_ctl_vbox,
                                                          mWidgets->vfline_Wgts, window);
	
    GtkWidget *expander_rep =  draw_VIZ_repartition_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_repart_ctl, 
                                                             mWidgets->f_repart_vws, window);
	
	GtkWidget *vbox_viz = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expander_rep, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_psf, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_iso, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_map, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_pvr, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_lic, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_fline, FALSE, FALSE, 0);
	
	
	
	GtkWidget *expand_MHD_viz = draw_control_block(f_MHD_ctl->f_viz_ctls->c_block_name, 
													 f_MHD_ctl->f_viz_ctls->f_iflag,
                                                   window, vbox_viz);
    return expand_MHD_viz;
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
	
    GtkWidget *expand_MHD_viz = MHD_VIZs_ctl_expander(window, f_MHD_ctl, mWidgets);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_viz, FALSE, FALSE, 0);
	
    GtkWidget *hbox_d1 = draw_int_item_entry_hbox(f_MHD_ctl->f_zm_ctls->f_crust_filter_ctl->f_crust_truncation_ctl);
	GtkWidget *expand_MHD_zm1 = draw_control_block(f_MHD_ctl->f_zm_ctls->f_crust_filter_ctl->c_block_name, 
                                                   f_MHD_ctl->f_zm_ctls->f_crust_filter_ctl->f_iflag,
                                                   window, hbox_d1);
    
    int i;
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zm_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zm_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zRMS_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zRMS_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zm_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zm_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zRMS_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zRMS_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    
	GtkWidget *expand_MHD_zm2 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zm_psf_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          mWidgets->zm_psf_Wgts, window);
	GtkWidget *expand_MHD_zm3 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zRMS_psf_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          mWidgets->zrms_psf_Wgts, window);
    
	GtkWidget *expand_MHD_zm4 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zm_map_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          mWidgets->zm_map_Wgts, window);
	GtkWidget *expand_MHD_zm5 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zRMS_map_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          mWidgets->zrms_map_Wgts, window);
  
    GtkWidget *vbox_zm = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm5, FALSE, FALSE, 0);
    
	GtkWidget *expand_MHD_zm = draw_control_block(f_MHD_ctl->f_zm_ctls->c_block_name, 
													 f_MHD_ctl->f_zm_ctls->f_iflag,
													 window, vbox_zm);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_zm, FALSE, FALSE, 0);
	
	
	
    mWidgets->ctl_MHD_Vbox = wrap_into_scroll_expansion_gtk(f_MHD_ctl->c_block_name, 560, 640,
                                                            window, mWidgets->ctl_MHD_inner_box);
	return;
};


GtkWidget * MHD_control_bottuns_hbox(struct main_widgets *mWidgets){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
		printf("f_MHD_ctl %p\n", f_MHD_ctl);
	if(f_MHD_ctl == NULL){
		printf("malloc error for f_MHD_ctl\n");
		exit(0);
	};
	
	GtkWidget *label = gtk_label_new("File:");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	/* Generate file entry  */
	GtkWidget *entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "MHD_ctl", (gpointer) f_MHD_ctl);
	g_object_set_data(G_OBJECT(entry), "mWidgets", (gpointer) mWidgets);
	
	/* Generate Bottuns */
	GtkWidget *button_O = gtk_button_new_with_label("Open");
	GtkWidget *button_V = gtk_button_new_with_label("View");
	GtkWidget *button_S = gtk_button_new_with_label("Save");
	GtkWidget *button_Q = gtk_button_new_with_label("Quit");
	
	
	
	g_signal_connect(G_OBJECT(button_O), "clicked", G_CALLBACK(cb_Open), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_V), "clicked", G_CALLBACK(cb_View), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_Save), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_Q), "clicked", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), button_O, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_V, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_Q, FALSE, FALSE, 0);
	
	return hbox;
}

int main(int argc, char** argv)
{
	gtk_init(&argc, &argv);
	
	window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
	gtk_container_set_border_width(GTK_CONTAINER(window), 5);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	struct main_widgets *mWidgets = (struct main_widgets *) malloc(sizeof(struct main_widgets));
		printf("mWidgets %p\n", mWidgets);
	if(mWidgets == NULL){
		printf("malloc error for mWidgets\n");
		exit(0);
	};
	mWidgets->f_repart_vws
			= (struct VIZ_repartition_widgets *) malloc(sizeof(struct VIZ_repartition_widgets));
	if(mWidgets->f_repart_vws == NULL){
		printf("malloc error for VIZ_repartition_widgets\n");
		exit(0);
    };
	
	mWidgets->main_Vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	mWidgets->open_Hbox = MHD_control_bottuns_hbox(mWidgets);
	gtk_box_pack_start(GTK_BOX(mWidgets->main_Vbox), mWidgets->open_Hbox, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), mWidgets->main_Vbox);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}

