/*
//  tree_view_4_pvr_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef tree_view_4_pvr_colormap_h_
#define tree_view_4_pvr_colormap_h_

#include <gtk/gtk.h>

#include "t_control_real2_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_real2_GTK.h"
#include "t_SGS_MHD_control_c.h"

struct colormap_view{
    int index_cmap;
    struct r2_clist_view *cmap_vws;
    struct r2_clist_view *opacity_vws;
};

/* prototypes */

#endif /* tree_view_4_pvr_colormap_h_ */
