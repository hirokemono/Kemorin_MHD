#include <gtk/gtk.h>

GtkWidget *window;

static void show_dialog(GtkButton *button, gpointer user_data)
{
  GtkWidget *dialog;
  //ボタンタイプ
  GtkButtonsType btype[] = {GTK_BUTTONS_CLOSE, GTK_BUTTONS_OK_CANCEL, GTK_BUTTONS_YES_NO, GTK_BUTTONS_OK};
  //メッセージタイプ
  GtkMessageType mtype = (GtkMessageType)user_data;
  //メッセージ
  gchar *string[] = {"GTK_MESSAGE_INFO", "GTK_MESSAGE_WARNING", "GTK_MESSAGE_QUESTION", "GTK_MESSAGE_ERROR"};
  gint result;

  //メッセージダイアログを生成
  dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, mtype, btype[mtype], "This dialog is a GtkMessageDialog sample\n" "(GtkMessageType = %s)", string[mtype]);
  result = gtk_dialog_run(GTK_DIALOG(dialog));

  switch( result ){
    //「ＯＫ(O)」ボタンをクリックした時
    case GTK_RESPONSE_OK:
      g_print( "GTK_RESPONSE_OK is received.\n" );
      break;
    //「キャンセル(C)」ボタンをクリックした時
    case GTK_RESPONSE_CANCEL:
      g_print( "GTK_RESPONSE_CANCEL is received.\n" );
      break;
    //「閉じる(C)」ボタンをクリックした時
    case GTK_RESPONSE_CLOSE:
      g_print( "GTK_RESPONSE_CLOSE is received.\n" );
      break;
    //「はい(Y)」ボタンをクリックした時
    case GTK_RESPONSE_YES:
      g_print( "GTK_RESPONSE_YES is received.\n" );
      break;
    //「いいえ(N)」ボタンをクリックした時
    case GTK_RESPONSE_NO:
      g_print( "GTK_RESPONSE_NO is received.\n" );
      break;
    default:
      g_print( "Another response is received.\n" );
      break;
  }
  gtk_widget_destroy(dialog);
}

int main(int argc, char** argv)
{
  GtkWidget *hbox;
  GtkWidget *button;
  //各種ボタンの役割を指定
	gchar *stock[] = {"dialog-information", "dialog-warning", 
			"dialog-question", "dialog-error"};
  int i;

  gtk_init(&argc, &argv);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), "MessageDialog");
  gtk_container_set_border_width(GTK_CONTAINER(window), 5);
  g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_container_add(GTK_CONTAINER(window), hbox);

  //左から４種類（情報、警告、質問、エラー）のボタンを生成
  for( i = 0; i < 4; ++i){
    button = gtk_button_new_with_label(stock[i]);
    g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(show_dialog), GINT_TO_POINTER(i));
    gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);
  }
  //右端に「終了(Q)」ボタンを生成
  button = gtk_button_new_with_label("_Quit");
  g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(gtk_main_quit), NULL);
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);

  gtk_widget_show_all(window);
  gtk_main();

  return 0;
}
