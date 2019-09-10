/*
 *  kemoview_gtk_colorsel.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_colorsel.h"

GtkWidget *window_Cmap;

void set_color_to_GTK(float color[4], GdkRGBA *gcolor)
{
    gcolor->red =   color[0];
    gcolor->green = color[1];
    gcolor->blue =  color[2];
    gcolor->alpha = color[3];
	return;
}

static void set_color_from_GTK(GtkColorChooser *colordialog, float color[4])
{
	GdkRGBA gcolor;
	
	gtk_color_chooser_get_rgba(colordialog, &gcolor);
	gtk_widget_destroy(window_Cmap);
	
    color[0] = (float) gcolor.red;
    color[1] = (float) gcolor.green;
    color[2] = (float) gcolor.blue;
    color[3] = (float) 1.0;
	/*printf("New background Color (R,G,B): %.7e %.7e %.7e \n", color[0], color[1], color[2]);*/
	
	return;
}

int kemoview_gtk_colorsel_CB(GtkWindow *parent_win, float color[4]){
	int response;
	GtkColorChooser *chooser;
	int iflag_set = 0;
	
	window_Cmap = gtk_color_chooser_dialog_new("Choose color", parent_win);
	gtk_widget_show_all(window_Cmap);
	
	response = gtk_dialog_run(GTK_DIALOG(window_Cmap));
	if (response == GTK_RESPONSE_OK){
		chooser = GTK_COLOR_CHOOSER(window_Cmap);
		set_color_from_GTK(chooser, color);
		iflag_set = 1;
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		gtk_widget_destroy(window_Cmap);
	}
	return iflag_set;
}
