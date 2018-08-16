
#include "tree_view_4_each_term_GTK.h"
#include "t_SGS_MHD_control_c.h"

struct SGS_MHD_control_c *mhd_ctl;
char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
char buf[LENGTHBUF];      /* character buffer for reading line */

static GtkWidget *main_window = NULL;

static void cb_close_window(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
    gtk_widget_show_all(main_window);
};

static void create_tree_view_window(GtkButton *button, gpointer user_data)
{
    struct coefs_view *coef_vws = (struct coefs_view *) user_data;
    
    static gint window_id = 0;
    GtkWidget *window;
    GtkWidget *vbox;
    
    gchar *title;
    
    
    coef_vws->dless_vws->dimless_tree_view = gtk_tree_view_new();
    init_dimless_tree_view(coef_vws->dless_vws);
    
    coef_vws->dless_vws->default_dless_view = gtk_tree_view_new();
    create_used_dimless_tree_views(coef_vws->dless_vws);
    

    coef_vws->mom_vws->coefs_tree_view = gtk_tree_view_new();
    coef_vws->mom_vws->new_coefs_tree_view = gtk_tree_view_new();
    coef_vws->mom_vws->dimless_tree_view = coef_vws->dless_vws->dimless_tree_view;
    init_momentum_tree_view(coef_vws->mom_vws);
    
    
    /* ウィンドウ作成 */
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    title = g_strdup_printf("GtkTreeModelSort #%d", ++window_id);
    gtk_window_set_title(GTK_WINDOW(window), title);
    g_free(title);
    
    
    vbox = gtk_vbox_new(FALSE, 0);
    /* Close window bottun */
    button = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
    gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(button), "clicked", 
                     G_CALLBACK(cb_close_window), window);
    
    add_dimless_selection_box(coef_vws->dless_vws, vbox);
    
    add_dimless_combobox_vbox(coef_vws->dless_vws, vbox);

    add_thermal_buo_selection_box(coef_vws->mom_vws, vbox);
    
    gtk_container_add(GTK_CONTAINER(window), vbox);
    
    
    gtk_widget_show_all(window);
};


int main(int argc, char **argv)
{
    GtkWidget *hbox;
    GtkWidget *button;
    struct coefs_view *coef_vws;
    
    srand((unsigned)time(NULL));
    
    mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
    alloc_SGS_MHD_control_c(mhd_ctl);
    read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);
    
    coef_vws = (struct coefs_view *) malloc(sizeof(struct coefs_view));
    init_coefs_views_GTK(mhd_ctl->model_ctl, coef_vws);

    gtk_init(&argc, &argv);
    
    main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(main_window), "GtkTreeModelSort");
    g_signal_connect(G_OBJECT(main_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
    
    hbox = gtk_hbox_new(TRUE, 10);
    
    button = gtk_button_new_with_label("Create Window");
    g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(create_tree_view_window), coef_vws);
    gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
    
    gtk_container_add(GTK_CONTAINER(main_window), hbox);
    gtk_container_set_border_width(GTK_CONTAINER(main_window), 10);
    gtk_widget_show_all(main_window);
    
    gtk_main();
    return 0;
}

