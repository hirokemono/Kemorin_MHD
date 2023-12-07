
/* view_modifier_gtk.h */

#ifndef VIEW_MODIFIER_GTK_
#define VIEW_MODIFIER_GTK_

#include <math.h>
#include <stdio.h>

#include "calypso_GTK.h"
#include "kemoviewer_gl.h"

#define ZOOM    1
#define PAN     2
#define ROTATE  3
#define SCALE   4
#define WALKTO  5

/* prototypes */

GtkWidget * open_kemoviwer_gl_panel(int npixel_x, int npixel_y);

void gtk_callbacks_init(gtk_callbacks_init);
void set_GTKindowSize(int width, int height,
                      struct kemoviewer_type *kemo_sgl);

void draw_fast(struct kemoviewer_type *kemo_sgl);
void draw_full(struct kemoviewer_type *kemo_sgl);
void sel_write_rotate_views(struct kemoviewer_type *kemo_sgl,
                            int iflag_img, struct kv_string *image_prefix,
                            int i_axis, int inc_deg);
void sel_write_evolution_views(struct kemoviewer_type *kemo_sgl,
                               int iflag_img, struct kv_string *image_prefix,
                               int ist_udt, int ied_udt, int inc_udt);

void set_viewtype_mode(int selected);

#endif
