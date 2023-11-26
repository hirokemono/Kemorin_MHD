/*
//  kemoviewer_gl.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include <stdio.h>
#include "kemoviewer_gl.h"

/*  OpenGL routines */


void kemoview_gl_init_lighting(struct kemoviewer_type *kemoviewer){
    kemo_gl_initial_lighting_c(kemoviewer->kemo_shaders);
    assign_kemoview_VAOs(kemoviewer->kemo_VAOs);
    return;
}
void kemoview_gl_background_color(struct kemoviewer_type *kemoviewer){
    set_gl_bg_color(kemoviewer->kemo_mesh->bg_color);
    return;
}

void kemoview_init_gl_background_color(struct kemoviewer_type *kemoviewer){
    init_bg_color_kemoview(kemoviewer->kemo_mesh->bg_color,
                           kemoviewer->kemo_mesh->text_color);
    set_bg_color_kemoview(kemoviewer->kemo_mesh->bg_color,
                          kemoviewer->kemo_mesh->text_color);
    return;
};

/*  Old routines */

struct shader_ids sampleShader;

void kemoview_draw_menu_setup(struct kemoviewer_type *kemoviewer){
    init_gl_menu_setup(kemoviewer->kemo_shaders);
    return;
};

void kemo_Cleanup(struct kemoviewer_type *kemoviewer)
{
  destory_shaders(kemoviewer->kemo_shaders->simple);
  destory_shaders(kemoviewer->kemo_shaders->simple);
}
