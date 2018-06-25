#include <gtk/gtk.h>

enum
{
  COLUMN_NAME,
  COLUMN_PIXBUF,
  N_COLUMNS
};

static void add_data(GtkIconView *iconview)
{
  GdkPixbuf *folder_pixbuf;
  GdkPixbuf *file_pixbuf;
  GtkListStore *store;
  GtkTreeIter iter;

  file_pixbuf = gdk_pixbuf_new_from_file("./velocity.png", NULL);
  folder_pixbuf = gdk_pixbuf_new_from_file("./ujb.png", NULL);

  store = GTK_LIST_STORE(gtk_icon_view_get_model(iconview));

  gtk_list_store_clear(store);

  //データの追加
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, COLUMN_NAME, "ujb.png", COLUMN_PIXBUF, folder_pixbuf, -1);
  g_object_unref(folder_pixbuf);

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, COLUMN_NAME, "velocity.png", COLUMN_PIXBUF, file_pixbuf, -1);
  g_object_unref(file_pixbuf);
}

static GtkWidget* create_icon_view_widget(void)
{
  GtkWidget *iconview;
  GtkListStore *store;

  //モデルの作成
  store = gtk_list_store_new(N_COLUMNS, G_TYPE_STRING, GDK_TYPE_PIXBUF);
  //アイコンビューの作成
  iconview = gtk_icon_view_new_with_model(GTK_TREE_MODEL(store));
  g_object_unref(store);

  return iconview;
}

static void cb_item_activated(GtkIconView *iconview, GtkTreePath *treepath, gpointer user_data)
{
  GtkListStore *store;
  GtkTreeIter iter;
  gchar *name;

  store = GTK_LIST_STORE(gtk_icon_view_get_model(iconview));
  //クリックされたアイテムに対するGtkTreeIterの値を取得
  gtk_tree_model_get_iter(GTK_TREE_MODEL(store), &iter, treepath);
  gtk_tree_model_get(GTK_TREE_MODEL(store), &iter, COLUMN_NAME, &name, -1);
  g_print( "item '%s' is clicked.\n", name);
  g_free(name);
}

int main(int argc, char** argv)
{
  GtkWidget *window;
  GtkWidget *scroll_window;
  GtkWidget *iconview;

  gtk_init(&argc, &argv);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), "IconView");
  gtk_widget_set_size_request(window, 500, 300);
  gtk_container_set_border_width(GTK_CONTAINER(window), 5);
  g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

  scroll_window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll_window), GTK_SHADOW_ETCHED_IN);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(window), scroll_window);

  iconview = create_icon_view_widget();
  //表示項目のラベル設定
  gtk_icon_view_set_text_column(GTK_ICON_VIEW(iconview), COLUMN_NAME);
  //表示項目の画像設定
  gtk_icon_view_set_pixbuf_column(GTK_ICON_VIEW(iconview), COLUMN_PIXBUF);
  //アイテムの幅を設定
  gtk_icon_view_set_item_width(GTK_ICON_VIEW(iconview), 128);
  //表示されているアイテムがダブルクリックされた時に発生するシグナル
  g_signal_connect(G_OBJECT(iconview), "item-activated", G_CALLBACK(cb_item_activated), NULL);

  gtk_container_add(GTK_CONTAINER(scroll_window), iconview);

  add_data(GTK_ICON_VIEW(iconview));

  gtk_widget_show_all(window);
  gtk_main();

  return 0;
}
