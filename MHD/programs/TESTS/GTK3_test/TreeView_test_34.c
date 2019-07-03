
#include "tree_view_4_pvr_colormap.h"
#include "t_SGS_MHD_control_c.h"

struct SGS_MHD_control_c *mhd_ctl;
char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
char buf[LENGTHBUF];      /* character buffer for reading line */

static GtkWidget *main_window = NULL;

void init_colormap_views(struct PVR_ctl_list *pvr1, struct colormap_view *color_vws){
    color_vws->cmap_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
    init_r2_clist_views(pvr1->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->colortbl_list, 
                        color_vws->cmap_vws);
    color_vws->opacity_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
    init_r2_clist_views(pvr1->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->linear_opacity_list, 
                        color_vws->opacity_vws);
    return;
}

static void cb_close_window(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
    gtk_widget_show_all(main_window);
};

static void create_tree_view_window(GtkButton *button, gpointer user_data)
{
    struct colormap_view *color_vws = (struct r2_clist_view *) user_data;
    
    static gint window_id = 0;
    GtkWidget *window;
    GtkWidget *vbox;
    
    gchar *title;
    
    create_real2_tree_view(color_vws->cmap_vws);
    create_real2_tree_view(color_vws->opacity_vws);
    
    
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
    
    add_real2_list_box_w_addbottun(color_vws->cmap_vws, vbox);
    gtk_container_add(GTK_CONTAINER(window), vbox);

    add_real2_list_box_w_addbottun(color_vws->opacity_vws, vbox);
    gtk_container_add(GTK_CONTAINER(window), vbox);
    
    gtk_widget_show_all(window);
};


int main(int argc, char **argv)
{
    GtkWidget *hbox;
    GtkWidget *button;
    struct colormap_view *color_vws;
    
    srand((unsigned)time(NULL));
    
    mhd_ctl = alloc_SGS_MHD_control_c();
    read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);
    
    color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
    init_colormap_views(&mhd_ctl->viz_c->pvr_ctl_list, color_vws);

    gtk_init(&argc, &argv);
    
    main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(main_window), "GtkTreeModelSort");
    g_signal_connect(G_OBJECT(main_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
    
    hbox = gtk_hbox_new(TRUE, 10);
    
    button = gtk_button_new_with_label("Create Window");
    g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(create_tree_view_window), color_vws);
    gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
    
    gtk_container_add(GTK_CONTAINER(main_window), hbox);
    gtk_container_set_border_width(GTK_CONTAINER(main_window), 10);
    gtk_widget_show_all(main_window);
    
    gtk_main();
    return 0;
}

