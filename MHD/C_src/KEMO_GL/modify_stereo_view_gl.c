
/* modify_stereo_view_gl.c*/

#include "modify_stereo_view_gl.h"

void modify_mono_kemoview(struct kemoviewer_type *kemoview,
                          struct kemoviewer_gl_type *kemo_gl){
    glDrawBuffer(GL_BACK);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    update_draw_objects_gl3(kemoview, kemo_gl);
    return;
};

void modify_stereo_kemoview(int iflag_draw_mode,
                            struct kemoviewer_type *kemoview,
                            struct kemoviewer_gl_type *kemo_gl){
    kemoview->view_s->iflag_draw_mode = iflag_draw_mode;
    glDrawBuffer(GL_BACK);
    
    if(kemoview->view_s->iflag_view_type == VIEW_STEREO){
        update_draw_anaglyph_gl3(kemoview, kemo_gl);
    } else {
        update_projection_struct(kemoview->view_s);
        modify_view_by_struct(kemoview->view_s);
        modify_mono_kemoview(kemoview, kemo_gl);
    }
	return;
};
