
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <sys/stat.h>

#include "control_elements_IO_c.h"

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
#include "control_panel_FLINE_GTK.h"
#include "control_panel_VIZs_GTK.h"


extern void c_view_control_sph_SGS_MHD();
extern void * c_read_control_sph_SGS_MHD(char *file_name);
GtkWidget *window;

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
		g_free(read_file_name);
		
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
	
    struct main_widgets *mWidgets = init_main_widgets();
	
	mWidgets->main_Vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	mWidgets->open_Hbox = MHD_control_bottuns_hbox(mWidgets);
	gtk_box_pack_start(GTK_BOX(mWidgets->main_Vbox), mWidgets->open_Hbox, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), mWidgets->main_Vbox);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}

