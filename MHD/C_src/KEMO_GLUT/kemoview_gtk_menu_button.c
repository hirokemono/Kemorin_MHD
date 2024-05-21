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
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(menu_item), "kemoview");
    struct lightparams_view *lightparams_vws
            = (struct lightparams_view *) g_object_get_data(G_OBJECT(menu_item), "lights");

    GtkWidget *pref_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(pref_win), "Preferences");
    gtk_widget_set_size_request(pref_win, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(pref_win), 5);
    g_signal_connect(G_OBJECT(pref_win), "destroy",
                     G_CALLBACK(prefWindowclose_CB), G_OBJECT(menu_item));
//    g_signal_connect(G_OBJECT(pref_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
//    g_signal_connect(G_OBJECT(pref_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
    
    GtkWidget *frame_pref = init_preference_frame(kemo_sgl,
                                                  lightparams_vws,
                                                  pref_win);
    gtk_container_add(GTK_CONTAINER(pref_win), frame_pref);
    gtk_widget_show_all(pref_win);
    gtk_widget_set_sensitive(menu_item, FALSE);
}

static void evo_menu_CB(GtkWidget *menu_item, gpointer user_data)
{
    struct evolution_gtk_menu *evo_gmenu
            = (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(menu_item), "tevo_menu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(menu_item), "kemoview");

    GtkWidget *evo_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(evo_win), "Time evolution");
    gtk_widget_set_size_request(evo_win, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(evo_win), 5);
    g_signal_connect(G_OBJECT(evo_win), "destroy",
                     G_CALLBACK(prefWindowclose_CB), G_OBJECT(menu_item));
//    g_signal_connect(G_OBJECT(evo_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
//    g_signal_connect(G_OBJECT(evo_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
    
    GtkWidget *frame_evo = init_evolution_menu_frame(kemo_sgl, evo_gmenu, evo_win);
    gtk_container_add(GTK_CONTAINER(evo_win), frame_evo);
    gtk_widget_show_all(evo_win);
    gtk_widget_set_sensitive(menu_item, FALSE);
}

GtkWidget *make_gtk_menu_button(struct main_buttons *mbot,
                                struct kemoviewer_type *kemo_sgl){
    GtkWidget *menu_widget = gtk_menu_new();
    GtkWidget *itemPref = gtk_menu_item_new_with_mnemonic("Preferences...");
    g_object_set_data(G_OBJECT(itemPref), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(itemPref), "lights", (gpointer) mbot->lightparams_vws);
    g_signal_connect(itemPref, "activate", G_CALLBACK (pref_menu_CB), NULL);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu_widget), itemPref);

    mbot->itemTEvo = gtk_menu_item_new_with_mnemonic("Evolution...");
    g_object_set_data(G_OBJECT(mbot->itemTEvo), "tevo_menu", (gpointer) mbot->evo_gmenu);
    g_object_set_data(G_OBJECT(mbot->itemTEvo), "kemoview", (gpointer) kemo_sgl);
    g_signal_connect(mbot->itemTEvo, "activate", G_CALLBACK (evo_menu_CB), NULL);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu_widget), mbot->itemTEvo);


    GtkWidget *menuButton = gtk_menu_button_new();
    gtk_menu_button_set_popup (GTK_MENU_BUTTON (menuButton), menu_widget);
    gtk_widget_show_all (menu_widget);
    
    GtkWidget *menuGrid = gtk_grid_new();
    gtk_grid_attach (GTK_GRID (menuGrid), menuButton, 1, 1, 1, 1);
    
    return menuGrid;
}