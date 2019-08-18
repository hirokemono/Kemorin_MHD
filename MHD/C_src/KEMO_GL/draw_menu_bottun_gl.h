
/* draw_menu_bottun_gl.h */

#ifndef DRAW_MENUBOTTUN_GL_
#define DRAW_MENUBOTTUN_GL_

#include <math.h>
#include "ysglfontdata.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "set_menu_bottun_to_buf.h"


#define MENU_HEIGHT 32
#define MENU_WIDTH  64

/* prototypes */

void draw_menubottun_gl();
void VBO_for_Menu(struct VAO_ids *menu_VAO, struct kemoview_shaders *kemo_shaders);

#endif

