//
//  kemoviewer_gl.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
//

#ifndef kemoviewer_gl_h_
#define kemoviewer_gl_h_

#ifdef __APPLE__
#include<OpenGL/gl3.h>
#else
#include<GL/gl.h>
#endif

#include "kemoviewer.h"

#include "m_kemoviewer_data.h"
#include "init_gl_lighting_c.h"
#include "move_draw_objects_gl.h"

#ifdef __cplusplus
extern "C" {
#endif


/*  OopenGL routines */
    void kemoview_gl_init_lighting(struct kemoviewer_type *kemoviewer);
    void kemoview_gl_background_color(struct kemoviewer_type *kemoviewer);
    void kemoview_init_gl_background_color(struct kemoviewer_type *kemoviewer);

/*  Old routines */
    void kemoview_draw_menu_setup(struct kemoviewer_type *kemoviewer);
    void kemo_Cleanup(struct kemoviewer_type *kemoviewer);

#ifdef __cplusplus
}
#endif

#endif /* kemoviewer_gl_h_ */
