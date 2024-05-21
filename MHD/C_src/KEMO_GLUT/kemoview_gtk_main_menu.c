/*
 *  kemoview_gtk_main_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_main_menu.h"


struct main_buttons * init_main_buttons(struct kemoviewer_type *kemoviewer_data){
	struct main_buttons *mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
    if (mbot == NULL) {
        printf("malloc error for main_buttons\n");
        exit(0);
    }

    mbot->psf_gmenu = alloc_psf_gtk_menu();
    mbot->fline_menu = (struct fieldline_gtk_menu *) malloc(sizeof(struct fieldline_gtk_menu));
    mbot->mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
    mbot->evo_gmenu = init_evoluaiton_menu_box(kemoviewer_data);

    mbot->view_menu = (struct view_widgets *) malloc(sizeof(struct view_widgets));

	mbot->rot_gmenu = init_rotation_menu_box();
    mbot->quilt_gmenu = init_quilt_menu_box();
    mbot->lightparams_vws = init_light_views_4_viewer(kemoviewer_data->kemo_buffers->kemo_lights);
	return mbot;
};

void dealloc_main_buttons(struct main_buttons *mbot){
    dealloc_psf_gtk_menu(mbot->psf_gmenu);
    free(mbot->fline_menu);
    free(mbot->mesh_vws);
    free(mbot->evo_gmenu);

    dealloc_light_views_4_viewer(mbot->lightparams_vws);
	
	free(mbot->rot_gmenu);
	free(mbot->view_menu);
	
	free(mbot);
	return;
};

void open_kemoviewer_file_glfw(struct kemoviewer_type *kemo_sgl,
                               struct kemoviewer_gl_type *kemo_gl,
                               struct kv_string *filename,
                               struct main_buttons *mbot,
                               GtkWidget *window_main){
    struct kv_string *file_prefix = kemoview_alloc_kvstring();
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	int iflag_datatype = kemoview_set_data_format_flag(filename, file_prefix, stripped_ext);
	
	printf("file name: %s\n", filename->string);
	printf("file_prefix %s\n", file_prefix->string);
	printf("stripped_ext %s\n", stripped_ext->string);
    kemoview_free_kvstring(stripped_ext);
    kemoview_free_kvstring(file_prefix);
	
	iflag_datatype = kemoview_open_data(filename, kemo_sgl);
    kemoview_free_kvstring(filename);
	
    init_psf_window(kemo_sgl, kemo_gl, mbot->psf_gmenu, window_main);
    init_fline_window(kemo_sgl, mbot->fline_menu, window_main);
    init_mesh_window(kemo_sgl, mbot->mesh_vws, mbot->meshWin);

    activate_evolution_menu(kemo_sgl, mbot->itemTEvo);
	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
	return;
};


static void open_file_CB(GtkButton *button, gpointer user_data){
    struct kv_string *filename;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    struct kemoviewer_gl_type *kemo_sgl_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

    int iflag_set = kemoview_gtk_read_file_select(button, user_data);
    if(iflag_set == IZERO) return;
    GtkEntry *entry = GTK_ENTRY(user_data);
    GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
    filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    
    open_kemoviewer_file_glfw(kemo_sgl, kemo_sgl_gl, filename,
                              mbot, window_main);
    return;
};


static void gtkCopyToClipboard_CB(GtkButton *button, gpointer user_data){
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    struct kemoviewer_gl_type *kemo_sgl_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    
    struct gl_texure_image *render_image = alloc_kemoview_gl_texure();
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_STEREO){
        draw_anaglyph_to_rgb_gl(kemo_sgl, kemo_sgl_gl, render_image);
    }else{
        draw_objects_to_rgb_gl(kemo_sgl, kemo_sgl_gl, render_image);
    }
    
    struct gl_texure_image *fliped_img = alloc_kemoview_gl_texure();
    alloc_draw_psf_texture(render_image->nipxel_xy[0],
                           render_image->nipxel_xy[1],
                           fliped_img);
    flip_gl_bitmap(render_image->nipxel_xy[0], render_image->nipxel_xy[1],
                   render_image->texure_rgba, fliped_img->texure_rgba);
    GdkPixbuf* pixbuf = gdk_pixbuf_new_from_data((const guchar *) fliped_img->texure_rgba,
                                                 GDK_COLORSPACE_RGB, FALSE, 8,
                                                 fliped_img->nipxel_xy[0], fliped_img->nipxel_xy[1],
                                                 (3*fliped_img->nipxel_xy[0]),
                                                 NULL, NULL);

    GtkClipboard *clipboard = (GtkClipboard *) user_data;
    gtk_clipboard_set_image(clipboard, pixbuf);
    dealloc_kemoview_gl_texure(render_image);
    dealloc_kemoview_gl_texure(fliped_img);
    return;
}

/*
static void gtkhidetest_CB(GtkButton *button, gpointer user_data){
    struct main_buttons *mbot = (struct main_buttons *)user_data;
    gchar * text = gtk_button_get_label(button);
    char test1[1];
    test1[0] = text[1];
    if(test1[0] == 110){
        gtk_button_set_label(button, "Off");
        gtk_widget_set_sensitive(mbot->expander_view, FALSE);
        gtk_widget_set_sensitive(mbot->expander_pref, FALSE);
//        sel_mesh_menu_box(mbot, FALSE);
    }else if(test1[0] == 102){
        gtk_button_set_label(button, "On");
        gtk_widget_set_sensitive(mbot->expander_view, TRUE);
        gtk_widget_set_sensitive(mbot->expander_pref, TRUE);
//        sel_mesh_menu_box(mbot, TRUE);
    };
    return;
}
*/

static void image_save_CB(GtkButton *button, gpointer user_data){
    struct kemoviewer_type *kemo_sgl 
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    int iflag_set = kemoview_gtk_save_file_select(button, user_data);
    int id_imagefmt_by_input;
    int i_quilt;
    
    if(iflag_set == IZERO) return;
    
    int iflag_quilt = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE);
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);

    GtkEntry *entry = GTK_ENTRY(user_data);
    struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
    struct kv_string *file_prefix = kemoview_alloc_kvstring();
    
    kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
    id_imagefmt_by_input = kemoview_set_image_file_format_id(stripped_ext);
    if(id_imagefmt_by_input < 0) {
        id_imagefmt_by_input = kemoview_get_view_integer(kemo_sgl, IMAGE_FORMAT_FLAG);;
        kemoview_free_kvstring(file_prefix);
        file_prefix = kemoview_init_kvstring_by_string(filename->string);
    };
    if(id_imagefmt_by_input == 0) return;
    kemoview_free_kvstring(filename);
    kemoview_free_kvstring(stripped_ext);
    
    printf("header: %s\n", file_prefix->string);
    if(iflag_quilt == 0){
        kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
        kemoview_write_window_to_file(id_imagefmt_by_input, file_prefix,
                                      npix_x, npix_y, image);
    } else {
        int nimg_column = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
        int nimg_raw =    kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
        unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp((nimg_column * npix_x),
                                                                      (nimg_raw * npix_y));
        for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
            draw_quilt(i_quilt, kemo_sgl);
            kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
            kemoview_add_quilt_img(i_quilt, kemo_sgl, image, quilt_image);
        };
        kemoview_write_window_to_file(id_imagefmt_by_input, file_prefix,
                                      (nimg_column * npix_x),
                                      (nimg_raw * npix_y), quilt_image);
        free(quilt_image);
        printf("quilt! %d x %d\n", nimg_column, nimg_raw);
        draw_full(kemo_sgl);
    }
    free(image);
    kemoview_free_kvstring(file_prefix);
    
    return;
};

GtkWidget * make_gtk_open_file_box(struct main_buttons *mbot,
                                   GtkWidget *window,
                                   struct kemoviewer_type *kemo_sgl,
                                   struct kemoviewer_gl_type *kemo_gl){
    GtkWidget *hbox_open;
    /*
    GtkWidget *testButton = gtk_button_new_with_label("On");
    g_signal_connect(G_OBJECT(testButton), "clicked",
                     G_CALLBACK(gtkhidetest_CB), (gpointer) mbot);
    */
    
    GtkWidget *entry_file = gtk_entry_new();
    g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer)   window);
    g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer)  mbot);
    g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(entry_file), "kemoview_gl", (gpointer) kemo_gl);
    
    GtkWidget *menuGrid = make_gtk_menu_button(mbot, kemo_sgl);
    
    GtkWidget *open_Button = gtk_button_new_with_label("Open...");
    g_signal_connect(G_OBJECT(open_Button), "clicked",
                     G_CALLBACK(open_file_CB), (gpointer)entry_file);
    
    hbox_open = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_open), menuGrid, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_open), gtk_label_new("File: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_open), entry_file, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_open), open_Button, FALSE, FALSE, 0);
    return hbox_open;
}

GtkWidget * make_gtk_save_file_box(struct main_buttons *mbot,
                                   GtkWidget *quitButton,
                                   struct kemoviewer_type *kemo_sgl,
                                   struct kemoviewer_gl_type *kemo_gl){
    GtkWidget *savebox;
    GtkWidget *entry_save_file = gtk_entry_new();
    g_object_set_data(G_OBJECT(entry_save_file), "kemoview", (gpointer) kemo_sgl);
    GtkWidget *imageSave_Button = gtk_button_new_with_label("Save Image...");
    g_signal_connect(G_OBJECT(imageSave_Button), "clicked",
                     G_CALLBACK(image_save_CB), (gpointer) entry_save_file);
    
    GtkClipboard *clipboard;
    clipboard = gtk_clipboard_get(GDK_SELECTION_PRIMARY);                                                            
    gtk_clipboard_clear(clipboard);                                                                                  
    gtk_clipboard_set_text(clipboard, "", 0);                                                                        

    clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);                                                          
    gtk_clipboard_clear(clipboard);                                                                                
    gtk_clipboard_set_text(clipboard, "", 0);
    g_object_set_data(G_OBJECT(clipboard), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(clipboard), "kemoview_gl", (gpointer) kemo_gl);
    
    GtkWidget *copyButton = gtk_button_new_with_label("Copy");
    g_signal_connect(G_OBJECT(copyButton), "clicked",
                     G_CALLBACK(gtkCopyToClipboard_CB), (gpointer) clipboard);
    
    savebox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(savebox), imageSave_Button, FALSE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(savebox), copyButton, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(savebox), quitButton, TRUE, TRUE, 0);
    return savebox;
};

GtkWidget * make_gtk_main_menu_box(struct main_buttons *mbot,
                                   GtkWidget *quitButton, GtkWidget *window_main,
                                   struct kemoviewer_type *kemo_sgl,
                                   struct kemoviewer_gl_type *kemo_gl){
    GtkWidget *vbox_menu;
    
    GtkWidget *hbox_open = make_gtk_open_file_box(mbot, window_main, kemo_sgl, kemo_gl);
    GtkWidget *savebox = make_gtk_save_file_box(mbot, quitButton, kemo_sgl, kemo_gl);
    
    GtkWidget *hbox_viewtype = make_gtk_viewmode_menu_box(mbot->view_menu, kemo_sgl);
    GtkWidget *hbox_axis = make_axis_menu_box(kemo_sgl, window_main);
    GtkWidget *expander_rot = init_rotation_menu_expander(kemo_sgl,
                                                          mbot->rot_gmenu,
                                                          window_main);
//    GtkWidget *expander_evo = init_evolution_menu_expander(kemo_sgl, mbot->evo_gmenu, window_main);
    mbot->expander_view = init_viewmatrix_menu_expander(kemo_sgl,
                                                        mbot->view_menu,
                                                        window_main);
    mbot->expander_quilt = init_quilt_menu_expander(kemo_sgl,
                                                    mbot->quilt_gmenu,
                                                    mbot->view_menu, window_main);
    
    vbox_menu = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox_menu), hbox_open, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_menu), savebox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_menu), hbox_viewtype, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_menu), hbox_axis, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_menu), expander_rot, FALSE, FALSE, 0);
//    gtk_box_pack_start(GTK_BOX(vbox_menu), expander_evo, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_menu), mbot->expander_quilt, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_menu), mbot->expander_view, FALSE, FALSE, 0);
    return vbox_menu;
}

