
#include <string.h>
#include <gtk/gtk.h>
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

#include "control_elements_IO_GTK.h"
#include "control_combobox_GTK.h"
#include "t_control_data_4_iso_c.h"
#include "kemoview_gtk_routines.h"

void draw_MHD_control_list(GtkWidget *vbox0, struct iso_ctl_c *iso_c, struct chara_ctl_item *ptem_t);

int iflag_read_iso = 0;
struct iso_ctl_c *iso_c0;


// struct SGS_MHD_control_c *mhd_ctl;
GtkWidget *window;
GtkWidget *vbox_0;

GtkWidget *entry_3, *entry_4, *entry_5;

double rtest = 2.5;
int ntest = 66;
char *ctest = "ahahahaha";
struct chara_ctl_item item_test = {55, "tako_tako"};
struct chara_ctl_item *ptem_test;

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

static void cb_New(GtkButton *button, gpointer data)
{
	draw_MHD_control_list(vbox_0, iso_c0, ptem_test);
	gtk_widget_show_all(window);
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
		/* Get Foulder name */
		folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
		g_print( "file name: %s\n", read_file_name);
		g_print( "folder name: %s\n", folder);
		g_free(folder);
    /* Show file name in entry */ 
		gtk_entry_set_text(entry, read_file_name);
		
        
        iso_c0 = init_iso_ctl_c();        
        int iflag = read_iso_ctl_file_c(read_file_name, buf, iso_c0);
        iflag_read_iso = 1;
		g_free(read_file_name);
		printf("iso_output_type_ctl original %s\n", iso_c0->iso_output_type_ctl->c_tbl);
        set_primary_iso_format_flag_c(iso_c0->iso_output_type_ctl->c_tbl);
		printf("iso_output_type_ctl modified %s\n", iso_c0->iso_output_type_ctl->c_tbl);
		
        
		draw_MHD_control_list(vbox_0, iso_c0, ptem_test);
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
  GtkWidget *dialog;
  GtkWidget *parent;
  GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
	gchar *write_file_name;

  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[1],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Save", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);
	
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		
		gtk_entry_set_text(entry, write_file_name);
		
		write_iso_ctl_file_c(write_file_name, iso_c0);
		dealloc_iso_ctl_c(iso_c0);
		g_free(write_file_name);
		iflag_read_iso = 0;
		
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

GtkWidget * iso_field_ctl_list_box(struct iso_field_ctl_c *iso_fld_c){
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *vbox_2[2];
	
	char *c_label;
	
	GtkWidget *color_flags_tree_view
			= create_control_flags_tree_view(iso_fld_c->flag_iso_color);
	
	add_control_combobox_vbox(iso_fld_c->label_fld_on_iso_ctl->label[ 0], 
							  iso_fld_c->output_type_ctl, 
							  iso_fld_c->flag_iso_color, 
							  color_flags_tree_view, vbox_1);
	printf("%le\n", iso_fld_c->output_value_ctl->r_data);
	c_label = duplicate_underscore(iso_fld_c->label_fld_on_iso_ctl->label[ 2]);
	vbox_2[0] = make_real_hbox(1, c_label, iso_fld_c->output_value_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	
	return vbox_1;
};


void draw_MHD_control_list(GtkWidget *vbox0, struct iso_ctl_c *iso_c, struct chara_ctl_item *ptem_t){
	GtkWidget *vbox_1;
	GtkWidget *vbox_2[iso_c->label_iso_ctl_w_dpl->num_labels];
	
	char *c_label;
	
	GtkWidget *file_fmt_flags_tree_view
			= create_control_flags_tree_view(iso_c->flag_iso_format);
	
	/* Generate expander */
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	vbox_2[0] = make_text_hbox(0, duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 0]), iso_c->iso_file_head_ctl);

	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	add_control_combobox_vbox(iso_c->label_iso_ctl_w_dpl->label[ 1], 
							  iso_c->iso_output_type_ctl, 
							  iso_c->flag_iso_format, 
							  file_fmt_flags_tree_view, vbox_1);
	
	vbox_2[2] = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	vbox_2[3] = iso_field_ctl_list_box(iso_c->iso_fld_c);
	wrap_into_expanded_frame_gtk
			(duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 2]), 
			 400, 200, vbox_2[2], vbox_1);
	wrap_into_expanded_frame_gtk
			(duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 3]), 
			 400, 200, vbox_2[3], vbox_1);
	c_label = isosurface_control_head();
	wrap_into_expanded_frame_gtk(duplicate_underscore(c_label), 
								 400, 600, vbox_1, vbox0);
	return;
};


void draw_MHD_control_bottuns(GtkWidget *vbox0){
	GtkWidget *button_N, *button_O, *button_S, *button_Q;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *entry;
	
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	label = gtk_label_new("File:");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	/* Generate file entry  */
	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)window);
	
	/* Generate Bottuns */
	button_N = gtk_button_new_with_label("New");
	button_O = gtk_button_new_with_label("Open");
	button_S = gtk_button_new_with_label("Save");
	button_Q = gtk_button_new_with_label("Quit");
	
	g_signal_connect(G_OBJECT(button_N), "clicked", G_CALLBACK(cb_New), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_O), "clicked", G_CALLBACK(cb_Open), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_Save), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_Q), "clicked", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), button_N, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_O, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_Q, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox0), hbox, FALSE, FALSE, 0);
	
}


int main(int argc, char** argv)
{
	iso_c0 = init_iso_ctl_c();
	iflag_read_iso = 1;
	
	gtk_init(&argc, &argv);

	window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
	gtk_container_set_border_width(GTK_CONTAINER(window), 5);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	

//	GtkWidget *scroll_window = gtk_scrolled_window_new(NULL, NULL);
//	gtk_box_pack_start(GTK_BOX(vbox_0), scroll_window, TRUE, TRUE, 0);
	
			ptem_test = init_chara_ctl_item_c();
			ptem_test->iflag = 111;
			ptem_test->c_tbl = "gggg";
	
	vbox_0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	draw_MHD_control_bottuns(vbox_0);
	gtk_container_add(GTK_CONTAINER(window), vbox_0);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}
