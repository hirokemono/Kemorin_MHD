#include <gtk/gtk.h>

static void cb_button(GtkButton *button, gpointer user_data)
{
  GtkWidget *dialog;
  GtkWidget *parent;
  GtkWidget *label;
  gint response;

  parent = GTK_WIDGET(user_data);

  //YES、NOボタン付きのダイアログを生成
  dialog = gtk_dialog_new_with_buttons("Save Confirmation", GTK_WINDOW(parent), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_NO, GTK_RESPONSE_NO, GTK_STOCK_YES, GTK_RESPONSE_YES, NULL);

  label = gtk_label_new("Confirm are you sure you want to save.");
  //ダイアログにlabelを加える
  gtk_container_add(GTK_CONTAINER(gtk_dialog_get_content_area(GTK_DIALOG(dialog))), label);
  //ダイアログのサイズを設定
  gtk_widget_set_size_request(dialog, 400, 100);
  //ダイアログを表示
  gtk_widget_show_all(dialog);

  //ボタンをクリックした時に戻り値を返す
  response = gtk_dialog_run(GTK_DIALOG(dialog));
  //「はい(Y)」ボタンをクリックした時
  if( response == GTK_RESPONSE_YES ){
    g_print( "Yes button was pressed.\n" );
  }
  //「いいえ(N)」ボタンをクリックした時
  else if( response == GTK_RESPONSE_NO ){
    g_print( "No button was pressed.\n" );
  }
  else{
    g_print( "Another response was received.\n" );
  }
  //ダイアログを閉じる
  gtk_widget_destroy(dialog);
}

int main(int argc, char** argv)
{
  GtkWidget *window;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;
  GtkWidget *button;

  gtk_init(&argc, &argv);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), "Dialog");
  gtk_widget_set_size_request(window, 300, -1);
  gtk_container_set_border_width(GTK_CONTAINER(window), 5);
  g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
  gtk_container_add(GTK_CONTAINER(window), hbox);

  //「保存(S)」ボタン
  button = gtk_button_new_with_label("_Save");
  g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(cb_button), (gpointer)window);
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);

  //「終了(Q)」ボタン
  button = gtk_button_new_with_label("_Quit");
  g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(gtk_main_quit), NULL);
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);

  gtk_widget_show_all(window);
  gtk_main();

  return 0;
}
