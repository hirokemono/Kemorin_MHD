
/* sort_by_patch_distance.h */

#ifndef SORT_BY_PATCH_DISTANCE_
#define SORT_BY_PATCH_DISTANCE_

#include "calypso_param_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_gl_transfer_matrix.h"
#include "quicksort_c.h"

/* prototypes */

void copy_patch_distance_mesh(struct viewer_mesh *mesh_s);
void sort_by_patch_distance_mesh(struct viewer_mesh *mesh_s, struct view_element *view_s);

int sort_by_patch_distance_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                 struct kemo_array_control *psf_a, struct view_element *view_s);
#endif
