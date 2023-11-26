
/* modify_stereo_view_gl.h */

#ifndef MODIFY_STEREO_VIEW_GL_
#define MODIFY_STEREO_VIEW_GL_

#include "kemoviewer_param_c.h"
#include "m_transfer_matrices.h"
#include "m_gl_transfer_matrix.h"
#include "move_draw_objects_gl.h"

/* prototypes */

void modify_mono_kemoview(struct kemoviewer_type *kemoview);
void modify_stereo_anaglyph(struct kemoviewer_type *kemoview);
void modify_stereo_kemoview(int iflag_draw_mode,
                            struct kemoviewer_type *kemoview);

#endif
