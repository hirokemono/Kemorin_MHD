/*
 *  kemoview_gtk_menu_button.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_menu_button.h"

static void prefWindowclose_CB (GtkWidget *new_win, gpointer user_data)
{
    GtkWidget *menu_item = GTK_WIDGET(user_data);
    gtk_widget_set_sensitive(menu_item, TRUE);
};

static void pref_menu_CB(GtkWidget *menu_item, gpointer user_data)
{
    GtkWidget *main_window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent_win"));
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    struct lightparams_view *lightparams_vws
            = (struct lightparams_view *) g_object_get_data(G_OBJECT(user_data), "lights");

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    GtkWidget *pref_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(pref_win),
                    win_upperleft[0], (win_upperleft[1]+size_xy[1]+56));
    gtk_window_set_title(GTK_WINDOW(pref_win), "Preferences");
    gtk_widget_set_size_request(pref_win, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(pref_win), 5);
    g_signal_connect(G_OBJECT(pref_win), "destroy",
                     G_CALLBACK(prefWindowclose_CB), G_OBJECT(menu_item));
//    g_signal_connect(G_OBJECT(pref_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
//    g_signal_connect(G_OBJECT(pref_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
    
    GtkWidget *frame_pref = init_preference_scrollbox(kemo_gl, lightparams_vws, pref_win);
    gtk_container_add(GTK_CONTAINER(pref_win), frame_pref);
    gtk_widget_show_all(pref_win);
    gtk_widget_set_sensitive(menu_item, FALSE);
}

static void evo_menu_CB(GtkWidget *menu_item, gpointer user_data)
{
    GtkWidget *main_window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent_win"));
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    struct evolution_gtk_menu *evo_gmenu
            = (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "tevo_menu");

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    GtkWidget *evo_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(evo_win),
                    (win_upperleft[0]+size_xy[0]), win_upperleft[1]);
    gtk_window_set_title(GTK_WINDOW(evo_win), "Time evolution");
    gtk_widget_set_size_request(evo_win, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(evo_win), 5);
    g_signal_connect(G_OBJECT(evo_win), "destroy",
                     G_CALLBACK(prefWindowclose_CB), G_OBJECT(menu_item));
//    g_signal_connect(G_OBJECT(evo_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
//    g_signal_connect(G_OBJECT(evo_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
    
    GtkWidget *frame_evo = init_evolution_menu_frame(kemo_gl, evo_gmenu, evo_win);
    gtk_container_add(GTK_CONTAINER(evo_win), frame_evo);
    gtk_widget_show_all(evo_win);
    gtk_widget_set_sensitive(menu_item, FALSE);
}

GtkWidget *make_gtk_menu_button(struct kemoviewer_gl_type *kemo_gl,
                                GtkWidget *main_window,
                                struct main_buttons *mbot){
    GtkWidget *menu_widget = gtk_menu_new();
    g_object_set_data(G_OBJECT(menu_widget), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(menu_widget), "parent_win",  (gpointer) main_window);
    g_object_set_data(G_OBJECT(menu_widget), "lights", (gpointer) mbot->lightparams_vws);
    g_object_set_data(G_OBJECT(menu_widget), "tevo_menu", (gpointer) mbot->evo_gmenu);

    GtkWidget *itemPref = gtk_menu_item_new_with_mnemonic("Preferences...");
    g_signal_connect(itemPref, "activate", G_CALLBACK (pref_menu_CB), menu_widget);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu_widget), itemPref);

    mbot->itemTEvo = gtk_menu_item_new_with_mnemonic("Evolution...");
    g_signal_connect(mbot->itemTEvo, "activate", G_CALLBACK (evo_menu_CB), menu_widget);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu_widget), mbot->itemTEvo);


    GtkWidget *menuButton = gtk_menu_button_new();
    gtk_menu_button_set_popup (GTK_MENU_BUTTON (menuButton), menu_widget);
    gtk_widget_show_all (menu_widget);
    
    GtkWidget *menuGrid = gtk_grid_new();
    gtk_grid_attach (GTK_GRID (menuGrid), menuButton, 1, 1, 1, 1);
    
    return menuGrid;
}
