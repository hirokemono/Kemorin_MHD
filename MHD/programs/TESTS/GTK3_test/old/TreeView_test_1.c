
#include <gtk/gtk.h>

  enum
  {
    COL_FIRST_NAME = 0,
    COL_LAST_NAME,
    COL_YEAR_BORN,
    NUM_COLS
  };

  gboolean
  foreach_func (GtkTreeModel *model,
                GtkTreePath  *path,
                GtkTreeIter  *iter,
                gpointer      user_data)
  {
    gchar *first_name, *last_name, *tree_path_str;
    guint  year_of_birth;

    /* Note: here we use 'iter' and not '&iter', because we did not allocate
     *  the iter on the stack and are already getting the pointer to a tree iter */

    gtk_tree_model_get (model, iter,
                        COL_FIRST_NAME, &first_name,
                        COL_LAST_NAME, &last_name,
                        COL_YEAR_BORN, &year_of_birth,
                        -1);

    tree_path_str = gtk_tree_path_to_string(path);

    g_print ("Row %s: %s %s, born %u\n", tree_path_str,
             first_name, last_name, year_of_birth);

    g_free(tree_path_str);

    g_free(first_name); /* gtk_tree_model_get made copies of       */
    g_free(last_name);  /* the strings for us when retrieving them */

    return FALSE; /* do not stop walking the store, call us with next row */
  }

  void
  create_and_fill_and_dump_store (void)
  {
    GtkListStore *liststore;
    GtkTreeIter   iter;

    liststore = gtk_list_store_new(NUM_COLS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_UINT);

    /* Append an empty row to the list store. Iter will point to the new row */
    gtk_list_store_append(liststore, &iter);

    /* Fill fields with some data */
    gtk_list_store_set (liststore, &iter,
                        COL_FIRST_NAME, "Joe",
                        COL_LAST_NAME, "Average",
                        COL_YEAR_BORN, (guint) 1970,
                        -1);

    /* Append another row, and fill in some data */
    gtk_list_store_append(liststore, &iter);

    gtk_list_store_set (liststore, &iter,
                        COL_FIRST_NAME, "Jane",
                        COL_LAST_NAME, "Common",
                        COL_YEAR_BORN, (guint) 1967,
                        -1);

    /* Append yet another row, and fill it */
    gtk_list_store_append(liststore, &iter);

    gtk_list_store_set (liststore, &iter,
                        COL_FIRST_NAME, "Yo",
                        COL_LAST_NAME, "Da",
                        COL_YEAR_BORN, (guint) 1873,
                        -1);

    /* Now traverse the list */

    gtk_tree_model_foreach(GTK_TREE_MODEL(liststore), foreach_func, NULL);
  }

  int
  main (int argc, char **argv)
  {
    gtk_init(&argc, &argv);

    create_and_fill_and_dump_store();

    return 0;
  }
  