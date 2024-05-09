
/* sort_by_patch_distance.h */

#ifndef SORT_BY_PATCH_DISTANCE_
#define SORT_BY_PATCH_DISTANCE_

#include "calypso_param_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_gl_transfer_matrix.h"
#include "quicksort_c.h"
#include "bitonic_sort_pthread.h"
/*
#ifdef __APPLE__
  #include <Accelerate/Accelerate.h>
#endif
*/
/* prototypes */

int sort_by_patch_distance_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                 struct kemo_array_control *psf_a, struct view_element *view_s);
#endif
