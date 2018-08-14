#include <gtk/gtk.h>

/* Callback function for the undo action */
static void
about_callback (GSimpleAction *simple,
               GVariant      *parameter,
               gpointer       user_data)
{
  g_print ("You clicked \"About\"\n");
}

static void
activate (GtkApplication *app,
          gpointer        user_data)
{
  GMenu *submenu;
  GtkWidget *grid;
  GMenu *menumodel;
  GtkWidget *window;
  GtkWidget *menubutton;
  GSimpleAction *about_action;

  window = gtk_application_window_new (app);
  grid = gtk_grid_new ();

  gtk_window_set_title (GTK_WINDOW (window), "MenuButton Example");
  gtk_window_set_default_size (GTK_WINDOW (window), 600, 400);

  menubutton = gtk_menu_button_new ();
  gtk_widget_set_size_request (menubutton, 80, 35);

  gtk_grid_attach (GTK_GRID (grid), menubutton, 0, 0, 1, 1);
  gtk_container_add (GTK_CONTAINER (window), grid);

  menumodel = g_menu_new ();
  g_menu_append (menumodel, "New", "app.new");
  g_menu_append (menumodel, "About", "win.about");

  submenu = g_menu_new ();
  g_menu_append_submenu (menumodel, "Other", G_MENU_MODEL (submenu));
  g_menu_append (submenu, "Quit", "app.quit");
  gtk_menu_button_set_menu_model (GTK_MENU_BUTTON (menubutton), G_MENU_MODEL (menumodel));

  about_action = g_simple_action_new ("about", NULL);
  g_signal_connect (about_action, "activate", G_CALLBACK (about_callback),
                    GTK_WINDOW (window));
  g_action_map_add_action (G_ACTION_MAP (window), G_ACTION (about_action));

  gtk_widget_show_all (window);
}


static void
new_callback (GSimpleAction *simple,
              GVariant      *parameter,
              gpointer       user_data)
{
  g_print ("You clicked \"New\"\n");
}

static void
quit_callback (GSimpleAction *simple,
               GVariant      *parameter,
               gpointer       user_data)
{
  GApplication *application = user_data;

  g_application_quit (application);
}

static void
startup (GApplication *app,
         gpointer      user_data)
{
  GSimpleAction *new_action;
  GSimpleAction *quit_action;

  new_action = g_simple_action_new ("new", NULL);
  g_signal_connect (new_action, "activate", G_CALLBACK (new_callback), app);
  g_action_map_add_action (G_ACTION_MAP (app), G_ACTION (new_action));

  quit_action = g_simple_action_new ("quit", NULL);
  g_signal_connect (quit_action, "activate", G_CALLBACK (quit_callback), app);
  g_action_map_add_action (G_ACTION_MAP (app), G_ACTION (quit_action));
}


int
main (int argc, char **argv)
{
  GtkApplication *app;
  int status;

  app = gtk_application_new ("org.gtk.example", G_APPLICATION_FLAGS_NONE);
  g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
  g_signal_connect (app, "startup", G_CALLBACK (startup), NULL);
  status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref (app);
  return status;
}
