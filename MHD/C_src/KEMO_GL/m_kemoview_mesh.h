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
#include "m_colorbar_work.h"
#include "read_data_4_kemoviewer.h"

struct kemoview_mesh{
    struct viewer_mesh        *mesh_d;
    struct mesh_menu_val      *mesh_m;
        
    float bg_color[4];
    float text_color[4];
};

/* prototypes */ 
struct kemoview_mesh * init_kemoview_mesh(void);
void dealloc_kemoview_mesh(struct kemoview_mesh *kemo_mesh);

void reset_draw_mesh(struct kemoview_mesh *kemo_mesh);
void close_mesh_view(struct kemoview_mesh *kemo_mesh);

int get_num_of_mesh_group(int iflag_group, struct kemoview_mesh *kemo_mesh);

void set_draw_mesh_flag(int iflag_group, int selected, int igrp, int iflag, 
					   struct kemoview_mesh *kemo_mesh);
void toggle_draw_mesh_flag(int iflag_group, int selected, int igrp, 
						   struct kemoview_mesh *kemo_mesh);
int get_draw_mesh_flag(struct kemoview_mesh *kemo_mesh, 
						int iflag_group, int selected, int igrp);

void set_mesh_color_flag(int iflag_group, int selected, int icolor,
						 struct kemoview_mesh *kemo_mesh);
int get_mesh_color_flag(int iflag_group, int selected, 
						struct kemoview_mesh *kemo_mesh);

void set_mesh_color_code(int iflag_group, int selected, float color_code4[4],
						 struct kemoview_mesh *kemo_mesh);
void get_mesh_color_code(struct kemoview_mesh *kemo_mesh, 
						 int iflag_group, int selected, float color_code4[4]);

void set_mesh_opacity(int iflag_group, double opacity_in, struct kemoview_mesh *kemo_mesh);
double get_mesh_opacity(struct kemoview_mesh *kemo_mesh, int iflag_group);

#endif
