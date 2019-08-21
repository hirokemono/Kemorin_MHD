/*
//  m_kemoview_mesh.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/


#ifndef M_KEMOVIEWER_MESH_
#define M_KEMOVIEWER_MESH_


#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "read_data_4_kemoviewer.h"

struct kemoview_mesh{
    struct viewer_mesh        *mesh_d;
    struct mesh_menu_val      *mesh_m;
};

/* prototypes */ 
struct kemoview_mesh * init_kemoview_mesh();
void dealloc_kemoview_mesh(struct kemoview_mesh *kemo_mesh);

void reset_draw_mesh(struct kemoview_mesh *kemo_mesh);
void close_mesh_view(struct kemoview_mesh *kemo_mesh);
#endif
