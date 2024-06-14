/*
//  tree_view_4_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_PVR_COLORMAP_
#define TREE_VIEW_4_PVR_COLORMAP_

#include "calypso_GTK.h"
#include "t_control_chara_IO.h"
#include "t_control_real_IO.h"
#include "t_control_real2_IO.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"
#include "tree_view_real2_GTK.h"
#include "quicksort_c.h"
#include "set_rgb_colors_c.h"
#include "set_rgba_table_c.h"
#include "m_color_table_c.h"
#include "kemoview_gtk_routines.h"

struct colormap_view{
    int iflag_current_model;
    
    struct chara_ctl_item *colormap_mode_gtk;
    struct r2_clist_view *cmap_vws;
    struct r2_clist_view *opacity_vws;
    int iflag_cmap_loaded;
    
    struct colormap_params *cmap_param;
    GtkWidget *scrolled_window;
    
    GtkWidget *button_add;
    GtkWidget *button_delete;
    
    GtkWidget *combobox_cmap;
    GtkWidget *vbox_cmap;
    GtkWidget *hbox_cmap;
    
    GtkCellRenderer *renderer_value_spin;
    GtkCellRenderer *renderer_color_spin;
};

struct colormap_view * alloc_colormap_view(void);
struct colormap_view * init_colormap_views_4_ctl(struct colormap_ctl_c *cmap_c);
void dealloc_colormap_views(struct colormap_view *color_vws);


void colormap_data_edited_CB(gchar *path_str, gchar *new_text,
			struct colormap_view *color_vws);
void colormap_color_edited_CB(gchar *path_str, gchar *new_text,
			struct colormap_view *color_vws);
void add_colormap_list_items_CB(struct colormap_view *color_vws);
void delete_colormap_list_items_CB(struct colormap_view *color_vws);

void opacity_data_edited_CB(gchar *path_str, gchar *new_text, 
			struct colormap_view *color_vws);
void opacity_color_edited_CB(gchar *path_str, gchar *new_text, 
			struct colormap_view *color_vws);
void add_opacity_list_items_CB(struct colormap_view *color_vws);
void delete_opacity_list_items_CB(struct colormap_view *color_vws);

int set_color_mode_CB(GtkComboBox *combobox_cmap, struct colormap_view *color_vws);

gboolean expose_event_CB(cairo_t *cr, struct colormap_view *color_vws);

GtkWidget * init_combobox_cmap(int iflag);

#endif /* TREE_VIEW_4_PVR_COLORMAP_ */

