#include <gtk/gtk.h>

static void filter_changed(GtkFileChooserDialog *dialog, gpointer data)
{
  g_print( "File filter changed.\n" );
}

static void cb_button(GtkButton *button, gpointer data)
{
  GtkWidget *dialog;
	
	GtkFileFilter *filter;
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
  gchar *filename;
  gchar *folder;

  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget*/
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[0],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Open", GTK_RESPONSE_ACCEPT, NULL);
	
	/* filter = gtk_file_filter_new();
	/* Set filter type 
	gtk_file_filter_set_name(filter, "All Files");
	/* Set filter pattern to be "*" 
	gtk_file_filter_add_pattern(filter, "*");
	/* Add filter 
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	/* Generate filter for JPEG 
	filter = gtk_file_filter_new();
	gtk_file_filter_set_name(filter, "JPEG");
	gtk_file_filter_add_mime_type(filter, "image/jpeg");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	/* Generate filter for PNG 
	filter = gtk_file_filter_new();
	gtk_file_filter_set_name(filter, "PNG");
	gtk_file_filter_add_mime_type(filter, "image/png");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	g_signal_connect(dialog, "notify::filter", G_CALLBACK(filter_changed), NULL);
*/
	gtk_widget_show_all(dialog);

	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
    g_print( "File is selecting \n");
		/* Get file name */
    filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		/* Get Foulder name */
    folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
    g_print( "file name: %s\n", filename);
    g_print( "foulder name: %s\n", folder);
    g_free(folder);
    /* Show file name in entry */
    gtk_entry_set_text(entry, filename);
    g_free(filename);
  }
  else if( response == GTK_RESPONSE_CANCEL ){
    g_print( "Cancel button was pressed.\n" );
  }
  else{
    g_print( "Another response was received.\n" );
  }
  gtk_widget_destroy(dialog);
}

int main(int argc, char** argv)
{
  GtkWidget *window;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;
  GtkWidget *button, *button2;

  gtk_init(&argc, &argv);

  window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
  gtk_container_set_border_width(GTK_CONTAINER(window), 5);
  g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_container_add(GTK_CONTAINER(window), hbox);

  label = gtk_label_new("File:");
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

  //entryを生成
  entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
  g_object_set_data(G_OBJECT(entry), "parent", (gpointer)window);

  //「開く(O)」ボタンを生成
  button = gtk_button_new_with_label("_Open");
  button2 = gtk_button_new_with_label("_Quit");
  g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(cb_button), (gpointer)entry);
  g_signal_connect(G_OBJECT(button2), "clicked", G_CALLBACK(gtk_main_quit), NULL);
  gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), button2, FALSE, FALSE, 0);

  gtk_widget_show_all(window);
  gtk_main();

  return 0;
}