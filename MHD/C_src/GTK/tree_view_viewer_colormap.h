/*
//  tree_view_viewer_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_VIEWER_COLORMAP_H_
#define TREE_VIEW_VIEWER_COLORMAP_H_

#include "kemoviewer_gl.h"
#include "calypso_GTK.h"
#include "m_color_table_c.h"
#include "set_each_psf_parameters.h"
#include "tree_view_4_colormap.h"
#include "tree_view_4_pvr_colormap.h"

void init_colormap_params_4_viewer(int id_model,
                                   struct kemoviewer_gl_type *kemo_gl,
                                   struct colormap_view *color_vws);

void load_color_opacity_map_from_list(struct psf_menu_val *psf_current_menu, 
                                      struct colormap_view *color_vws);
GtkWidget * add_pvr_colormap_list_box(struct kemoviewer_type *kemo_sgl,
                                      struct colormap_view *color_vws);

#endif /* TREE_VIEW_VIEWER_COLORMAP_H_ */

