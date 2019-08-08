
/* move_draw_objects_gl.h */

#ifndef MOVE_DRAW_OBJECT_GL_
#define MOVE_DRAW_OBJECT_GL_
/* Size of window */ 

#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "draw_axis_c.h"
#include "draw_object_kemo.h"
#include "draw_colorbar_gl.h"
#include "draw_fieldlines.h"
#include "draw_mapgrid.h"
#include "draw_coastline.h"
#include "drawcube_gl.h"
#include "draw_patches_4_PSF.h"


/* prototypes */ 
void draw_objects(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
				  struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
				  struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
				  struct fline_menu_val *fline_m, struct view_element *view_s,
				  struct buffer_for_gl *gl_buf);
#endif
