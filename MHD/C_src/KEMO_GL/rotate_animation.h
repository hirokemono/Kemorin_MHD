
/*  rotate_animation.h */

#ifndef ROTATE_ANIMATION_
#define ROTATE_ANIMATION_

#include "calypso_param_c.h"
#include "m_gl_transfer_matrix.h"

/* prototypes */

void init_rot_animation(struct view_element *view_s);
void reset_rot_animation(struct view_element *view_s);
void add_animation_rotation(struct view_element *view_s, double dt);

#endif
